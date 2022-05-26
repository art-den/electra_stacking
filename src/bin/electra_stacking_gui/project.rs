use std::collections::HashMap;
use std::{path::*, io::*, fs::*, collections::HashSet};
use std::sync::{*, atomic::AtomicBool, atomic::Ordering};
use serde::*;
use gettextrs::*;
use chrono::prelude::*;
use electra_stacking::{
    calc::*,
    progress::*,
    stacking_utils::*,
    light_file::*,
    stars::*,
    image_raw::*,
    image_norm::*,
    image_formats::*,
    fs_utils::*
};
use crate::config::*;

const MASTER_DARK_FN: &str = "master-dark.es_raw";
const MASTER_FLAT_FN: &str = "master-flat.es_raw";
const MASTER_BIAS_FN: &str = "master-bias.es_raw";

#[derive(Serialize, Deserialize, Clone)]
#[serde(default)]
pub struct Project {
    pub config: ProjectConfig,
    pub cleanup_conf: ClenupConf,
    pub groups: Vec<ProjectGroup>,
    pub ref_image: Option<PathBuf>,

    #[serde(skip)]
    pub file_name: Option<PathBuf>,

    #[serde(skip)]
    pub changed: bool,
}

pub enum CanExecStackLightsRes {
    Ok,
    NoRefFile,
}

impl Default for Project {
    fn default() -> Self {
        Project {
            config: Default::default(),
            cleanup_conf: Default::default(),
            groups: Vec::new(),
            file_name: None,
            ref_image: None,
            changed: false,
        }
    }
}

impl Project {
    pub fn load(&mut self, file_name: &Path) -> anyhow::Result<()> {
        let reader = BufReader::new(File::open(file_name)?);
        *self = serde_json::from_reader(reader)?;
        Ok(())
    }

    pub fn save(&mut self, file_name: &Path) -> anyhow::Result<()> {
        let writer = BufWriter::new(File::create(file_name)?);
        serde_json::to_writer_pretty(writer, self)?;
        self.changed = false;
        Ok(())
    }

    pub fn make_default(&mut self) {
        self.groups.clear();
        let mut group = ProjectGroup::default();
        group.options = GroupOptions::default();
        self.groups.push(group);
    }

    pub fn add_default_group_if_empty(&mut self) {
        if !self.groups.is_empty() { return; }
        self.make_default();
    }

    pub fn group_exists(&self, uuid: &str) -> bool {
        self.groups
            .iter()
            .position(|g| g.uuid == uuid)
            .is_some()
    }

    pub fn find_group_by_uuid_mut(&mut self, uuid: &str) -> Option<&mut ProjectGroup> {
        self.groups
            .iter_mut()
            .find(|g| g.uuid == uuid)
    }

    pub fn add_new_group(&mut self, options: GroupOptions) {
        let mut group = ProjectGroup::default();
        group.options = options;
        self.groups.push(group);
        self.changed = true;
    }

    pub fn remove_group(&mut self, group_idx: usize) -> ProjectGroup {
        let result = self.groups.remove(group_idx);
        self.changed = true;
        result
    }

    pub fn set_new_config(&mut self, config: ProjectConfig) {
        self.config = config;
        self.changed = true;
    }

    pub fn register_light_files(
        &self,
        progress:    &ProgressTs,
        cancel_flag: &Arc<AtomicBool>,
        cpu_load:    CpuLoad,
    ) -> anyhow::Result<HashMap<PathBuf, RegInfo>> {
        let total_files_cnt: usize =
            self.groups.iter()
            .map(|g| g.light_files.list.len())
            .sum();

        if total_files_cnt == 0 {
            anyhow::bail!(gettext("No files to register"));
        }

        let thread_pool = rayon::ThreadPoolBuilder::new()
            .num_threads(cpu_load.to_threads_count())
            .build()?;

        // master-files
        for (idx, group) in self.groups.iter().enumerate() {
            if cancel_flag.load(Ordering::Relaxed) { return Ok(HashMap::new()); }
            group.create_master_files(
                idx,
                progress,
                cancel_flag,
                &self.config,
                &thread_pool
            )?;
        }

        let mut result = Mutex::new(HashMap::new());
        for (idx, group) in self.groups.iter().enumerate() {
            if cancel_flag.load(Ordering::Relaxed) { return Ok(HashMap::new()); }
            group.register_light_files(
                idx,
                progress,
                cancel_flag,
                &thread_pool,
                &mut result
            )?;
        }

        Ok(result.into_inner().unwrap())
    }

    pub fn update_light_files_reg_info(&mut self, reg_info: HashMap<PathBuf, RegInfo>) {
        for group in &mut self.groups {
            group.light_files.update_reg_info(&reg_info);
        }
        self.changed = true;
    }

    pub fn can_exec_cleanup(&self) -> bool {
        for group in &self.groups {
            if !group.can_exec_cleanup() { return false; }
        }
        true
    }

    pub fn cleanup_light_files(&mut self) -> anyhow::Result<usize> {
        let mut cleaned_up_cnt = 0_usize;
        for group in &mut self.groups {
            cleaned_up_cnt += group.cleanup_light_files(&self.cleanup_conf)?;
        }
        self.changed = true;
        Ok(cleaned_up_cnt)
    }

    pub fn can_exec_stack_light_files(&self) -> CanExecStackLightsRes {
        if self.ref_image.is_none() {
            return CanExecStackLightsRes::NoRefFile;
        }
        CanExecStackLightsRes::Ok
    }

    pub fn stack_light_files(
        &self,
        progress:    &ProgressTs,
        cancel_flag: &Arc<AtomicBool>,
        cpu_load:    CpuLoad,
    ) -> anyhow::Result<StackLightsResult> {
        let result_file_name = self.get_result_file_name()?;

        let thread_pool = rayon::ThreadPoolBuilder::new()
            .num_threads(cpu_load.to_threads_count())
            .build()?;

        // master-files

        for (idx, group) in self.groups.iter().enumerate() {
            if cancel_flag.load(Ordering::Relaxed) { anyhow::bail!("Termimated") }
            group.create_master_files(
                idx,
                progress,
                cancel_flag,
                &self.config,
                &thread_pool
            )?;
        }

        let bin = match self.config.image_size {
            ImageSize::Original => 1,
            ImageSize::Bin2x2 => 2,
        };

        // Find and load reference files

        progress.lock().unwrap().stage(&gettext(
            "Loading reference image..."
        ));

        let group_with_ref_file = self
            .find_group_with_light_file(self.ref_image.as_ref().unwrap())
            .ok_or_else(|| anyhow::anyhow!(gettext("Can't find group with reference image")))?;

        let ref_cal = CalibrationData::load(
            &group_with_ref_file.flat_files.get_master_full_file_name(MASTER_FLAT_FN),
            &group_with_ref_file.dark_files.get_master_full_file_name(MASTER_DARK_FN),
            &group_with_ref_file.bias_files.get_master_full_file_name(MASTER_BIAS_FN),
        )?;

        let ref_data = RefBgData::new(self.ref_image.as_ref().unwrap(), &ref_cal, bin)?;

        // temporary light files

        let temp_file_names = Mutex::new(Vec::<TempFileData>::new());
        let files_to_del_later = Mutex::new(FilesToDeleteLater::new());

        for (idx, group) in self.groups.iter().enumerate() {
            if cancel_flag.load(Ordering::Relaxed) {
                anyhow::bail!(gettext("Termimated"))
            }
            if !group.used {
                continue;
            }

            progress.lock().unwrap().stage(&format!(
                "Processing group {}",
                group.name(idx)
            ));

            create_temp_light_files(
                &progress,
                group.light_files.get_selected_file_names(),
                &group.flat_files.get_master_full_file_name(MASTER_FLAT_FN),
                &group.dark_files.get_master_full_file_name(MASTER_DARK_FN),
                &group.bias_files.get_master_full_file_name(MASTER_BIAS_FN),
                &ref_data,
                bin,
                &temp_file_names,
                &files_to_del_later,
                &thread_pool,
                cancel_flag
            )?;
        }

        if cancel_flag.load(Ordering::Relaxed) {
            anyhow::bail!(gettext("Termimated"))
        }
        if temp_file_names.lock().unwrap().is_empty() {
            anyhow::bail!(gettext("No light files to stack"));
        }

        // stacking all temporary light files into result image

        progress.lock().unwrap().stage(&gettext(
            "Stacking all images into result image file..."
        ));

        merge_temp_light_files(
            &progress,
            &temp_file_names.lock().unwrap(),
            &self.config.light_calc_opts,
            ref_data.image.image.is_rgb(),
            ref_data.image.image.width(),
            ref_data.image.image.height(),
            &result_file_name,
            &cancel_flag
        )?;

        if cancel_flag.load(Ordering::Relaxed) {
            anyhow::bail!(gettext("Termimated"))
        }

        Ok(StackLightsResult {
            file_name: result_file_name,
        })
    }

    fn find_group_with_light_file(&self, file_name: &Path) -> Option<&ProjectGroup> {
        for group in &self.groups {
            let res = group.light_files.list.iter().find(|&f| f.file_name == file_name);
            if res.is_some() {
                return Some(group);
            }
        }
        None
    }

    pub fn calc_time(&self) -> f64 {
        self.groups
            .iter()
            .filter(|g| g.used)
            .map(|g| g.light_files.calc_total_exp_time())
            .sum()
    }

    pub fn get_result_file_name(&self) -> anyhow::Result<PathBuf> {
        if let Some(file_name) = &self.file_name {
            let file_ext = match self.config.res_img_type {
                ResFileType::Fit => "fit",
                ResFileType::Tif => "tif",
            };
            Ok(file_name
                .with_file_name(self.config.name.as_ref().unwrap_or(&"result".to_string()).trim())
                .with_extension(file_ext)
            )
        } else {
            anyhow::bail!(gettext("You have to save project before"));
        }
    }

    pub fn get_total_light_files_count(&self) -> usize {
        self.groups.iter()
            .map(|g| g.light_files.list.len())
            .sum()
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub enum ImageSize {
    Original,
    Bin2x2,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ResFileType {
    Fit,
    Tif,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(default)]
pub struct ProjectConfig {
    pub name: Option<String>,
    pub image_size: ImageSize,
    pub light_calc_opts: CalcOpts,
    pub dark_calc_opts: CalcOpts,
    pub flat_calc_opts: CalcOpts,
    pub bias_calc_opts: CalcOpts,
    pub res_img_type: ResFileType,
}

impl Default for ProjectConfig {
    fn default() -> Self {
        ProjectConfig {
            name: None,
            image_size: ImageSize::Original,
            light_calc_opts: CalcOpts::default(),
            dark_calc_opts: CalcOpts::default(),
            flat_calc_opts: CalcOpts::default(),
            bias_calc_opts: CalcOpts::default(),
            res_img_type: ResFileType::Fit,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ProjectFileType {
    Light,
    Dark,
    Flat,
    Bias,
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(default)]
pub struct ProjectGroup {
    pub options: GroupOptions,
    pub used: bool,
    pub uuid: String,
    pub light_files: ProjectFiles,
    pub dark_files: ProjectFiles,
    pub bias_files: ProjectFiles,
    pub flat_files: ProjectFiles,
}

impl Default for ProjectGroup {
    fn default() -> Self {
        ProjectGroup {
            options: GroupOptions::default(),
            used: true,
            uuid: uuid::Uuid::new_v4().to_string(),
            dark_files: ProjectFiles::default(),
            bias_files: ProjectFiles::default(),
            flat_files: ProjectFiles::default(),
            light_files: ProjectFiles::default(),
        }
    }
}

impl ProjectGroup {
    pub fn name(&self, group_index: usize) -> String {
        if let Some(name) = &self.options.name {
            return name.clone();
        }

        if let Some(first_file) = self.light_files.list.first() {
            match (first_file.iso, first_file.exp) {
                (Some(iso), Some(exp)) =>
                    return format!("#{}. ISO={}, Exp={:.0}sec", group_index+1, iso, exp),
                (None, Some(exp)) =>
                    return format!("#{}. Exp={:.0}sec", group_index+1, exp),
                (Some(iso), None) =>
                    return format!("#{}. ISO={}", group_index+1, iso),
                (_, _) => ()
            }
        }
        if group_index == 0 {
            return gettext("Main group")
        } else {
            return format!("{} #{}", gettext("Group"), group_index+1)
        }
    }

    pub fn get_file_list_by_type(&self, file_type: ProjectFileType) -> &ProjectFiles {
        match file_type {
            ProjectFileType::Light => &self.light_files,
            ProjectFileType::Dark => &self.dark_files,
            ProjectFileType::Flat => &self.flat_files,
            ProjectFileType::Bias => &self.bias_files,
        }
    }

    pub fn get_file_list_by_type_mut(&mut self, file_type: ProjectFileType) -> &mut ProjectFiles {
        match file_type {
            ProjectFileType::Light => &mut self.light_files,
            ProjectFileType::Dark => &mut self.dark_files,
            ProjectFileType::Flat => &mut self.flat_files,
            ProjectFileType::Bias => &mut self.bias_files,
        }
    }

    pub fn change_file_types(
        &mut self,
        from_type:    ProjectFileType,
        to_type:      ProjectFileType,
        file_indices: Vec<usize>,
    ) {
        let from_files = self.get_file_list_by_type_mut(from_type);
        let files_to_move = from_files.remove_files_by_idx(file_indices);
        let to_files = self.get_file_list_by_type_mut(to_type);
        for file in files_to_move {
            to_files.list.push(file);
        }
    }

    fn create_master_files(
        &self,
        group_index: usize,
        progress:    &ProgressTs,
        cancel_flag: &Arc<AtomicBool>,
        config:      &ProjectConfig,
        thread_pool: &rayon::ThreadPool,
    ) -> anyhow::Result<()> {
        let bias_recreated = self.create_master_bias(
            group_index,
            progress,
            cancel_flag,
            &config.bias_calc_opts,
            &thread_pool
        )?;

        self.create_master_dark(
            group_index,
            progress,
            cancel_flag,
            &config.dark_calc_opts,
            &thread_pool
        )?;

        self.create_master_flat(
            group_index,
            progress,
            cancel_flag,
            &config.flat_calc_opts,
            &self.bias_files.get_master_full_file_name(MASTER_BIAS_FN),
            &thread_pool,
            bias_recreated
        )?;

        Ok(())
    }


    fn create_master_dark(
        &self,
        group_index: usize,
        progress:    &ProgressTs,
        cancel_flag: &Arc<AtomicBool>,
        calc_opts:   &CalcOpts,
        thread_pool: &rayon::ThreadPool,
    ) -> anyhow::Result<()> {
        progress.lock().unwrap().stage(&format!(
            "Creating master-dark for group {}",
            self.name(group_index)
        ));

        Self::create_master_file(
            &self.dark_files,
            cancel_flag,
            MASTER_DARK_FN,
            |file_names, file_name| {
                create_master_dark_or_bias_file(
                    file_names,
                    calc_opts,
                    file_name,
                    progress,
                    thread_pool,
                    cancel_flag
                )
            }
        )?;
        Ok(())
    }

    fn create_master_flat(
        &self,
        group_index:         usize,
        progress:            &ProgressTs,
        cancel_flag:         &Arc<AtomicBool>,
        calc_opts:           &CalcOpts,
        master_bias_file:    &Option<PathBuf>,
        thread_pool:         &rayon::ThreadPool,
        force_even_if_exist: bool,
    ) -> anyhow::Result<()> {
        progress.lock().unwrap().stage(&format!(
            "Creating master-flat for group {}",
            self.name(group_index)
        ));

        Self::create_master_file(
            &self.flat_files,
            cancel_flag,
            MASTER_FLAT_FN,
            |file_names, file_name| {
                create_master_flat_file(
                    file_names,
                    calc_opts,
                    master_bias_file,
                    file_name,
                    progress,
                    thread_pool,
                    cancel_flag,
                    force_even_if_exist
                )
            }
        )?;
        Ok(())
    }

    fn create_master_bias(
        &self,
        group_index: usize,
        progress:    &ProgressTs,
        cancel_flag: &Arc<AtomicBool>,
        calc_opts:   &CalcOpts,
        thread_pool: &rayon::ThreadPool,
    ) -> anyhow::Result<bool> {
        progress.lock().unwrap().stage(&format!(
            "Creating master-bias for group {}",
            self.name(group_index)
        ));

        Self::create_master_file(
            &self.bias_files,
            cancel_flag,
            MASTER_BIAS_FN,
            |file_names, file_name| {
                create_master_dark_or_bias_file(
                    file_names,
                    calc_opts,
                    file_name,
                    progress,
                    thread_pool,
                    cancel_flag
                )
            }
        )
    }

    fn create_master_file<F>(
        calibr_files: &ProjectFiles,
        cancel_flag:  &Arc<AtomicBool>,
        file_name:    &str,
        create_fun:   F,
    ) -> anyhow::Result<bool>
        where F: FnOnce(&[PathBuf], &PathBuf) -> anyhow::Result<bool>
    {
        if cancel_flag.load(Ordering::Relaxed) { return Ok(false); }
        let file_names = calibr_files.get_selected_file_names();
        if file_names.is_empty() { return Ok(false); }
        let file_name = calibr_files.get_master_full_file_name(file_name).unwrap();
        create_fun(&file_names, &file_name)
    }

    fn register_light_files(
        &self,
        group_idx:   usize,
        progress:    &ProgressTs,
        cancel_flag: &Arc<AtomicBool>,
        thread_pool: &rayon::ThreadPool,
        result:      &mut Mutex<HashMap<PathBuf, RegInfo>>,
    ) -> anyhow::Result<()> {
        progress.lock().unwrap().stage(&format!(
            "Registering files for group {}...",
            self.name(group_idx)
        ));
        progress.lock().unwrap().set_total(self.light_files.list.len());
        progress.lock().unwrap().progress(false, &gettext(
            "Loading calibration master files..."
        ));

        let cal_data = CalibrationData::load(
            &self.flat_files.get_master_full_file_name(MASTER_FLAT_FN),
            &self.dark_files.get_master_full_file_name(MASTER_DARK_FN),
            &self.bias_files.get_master_full_file_name(MASTER_BIAS_FN),
        )?;

        let cur_result = Mutex::new(anyhow::Result::<()>::Ok(()));

        thread_pool.scope(|s| {
            for file in &self.light_files.list {
                s.spawn(|_| {
                    if cancel_flag.load(Ordering::Relaxed)
                    || cur_result.lock().unwrap().is_err() {
                        return;
                    }
                    let load_light_file_res = LightFile::load(
                        &file.file_name,
                        &cal_data,
                        None,
                        LoadLightFlags::STARS
                        | LoadLightFlags::NOISE
                        | LoadLightFlags::BACKGROUND
                        | LoadLightFlags::SHARPNESS,
                        1
                    );
                    let light_file = match load_light_file_res {
                        Ok(light_file) => light_file,
                        Err(err) => {
                            *cur_result.lock().unwrap() = Err(anyhow::anyhow!(
                                r#"Error "{}" during loading of file "{}""#,
                                err.to_string(),
                                file.file_name.to_str().unwrap_or("")
                            ));
                            return;
                        },
                    };

                    let stars_stat_res = calc_stars_stat(&light_file.stars,&light_file.grey);
                    let stars_stat = match stars_stat_res {
                        Ok(stars_stat) => stars_stat,
                        Err(err) => {
                            *cur_result.lock().unwrap() = Err(anyhow::anyhow!(
                                r#"Error "{}" during stars statistics calculation for file "{}""#,
                                err.to_string(),
                                file.file_name.to_str().unwrap_or("")
                            ));
                            return;
                        }
                    };

                    progress.lock().unwrap().progress(true, file.file_name.to_str().unwrap_or(""));
                    result.lock().unwrap().insert(
                        file.file_name.clone(),
                        RegInfo {
                            noise:       light_file.noise,
                            background:  light_file.background,
                            sharpness:   light_file.sharpness,
                            fwhm:        stars_stat.fwhm,
                            stars:       light_file.stars.len(),
                            stars_r_dev: stars_stat.aver_r_dev,
                        }
                    );
                });
            }
        });

        if cancel_flag.load(Ordering::Relaxed) {
            anyhow::bail!(gettext("Terminated"));
        }

        cur_result.into_inner()?
    }

    pub fn can_exec_cleanup(&self) -> bool {
        for file in &self.light_files.list {
            if file.reg_info.is_none() { return false; }
        }
        true
    }

    pub fn cleanup_light_files(&mut self, conf: &ClenupConf) -> anyhow::Result<usize> {
        if conf.check_before_execute {
            for file in &mut self.light_files.list {
                file.used = true;
                file.flags = 0;
            }
        }

        let before_checked_cnt = self.light_files.list
            .iter()
            .filter(|f| f.used)
            .count();

        self.cleanup_by_conf(
            &conf.stars_r_dev,
            false,
            true,
            FILE_FLAG_CLEANUP_R_DEV,
            |reg_info: &RegInfo| reg_info.stars_r_dev
        )?;

        self.cleanup_by_conf(
            &conf.stars_fwhm,
            false,
            true,
            FILE_FLAG_CLEANUP_FWHM,
            |reg_info: &RegInfo| reg_info.fwhm
        )?;

        self.cleanup_by_conf(
            &conf.stars_count,
            true,
            true,
            FILE_FLAG_CLEANUP_STARS,
            |reg_info: &RegInfo| reg_info.stars as f32
        )?;

        self.cleanup_by_conf(
            &conf.img_sharpness,
            true,
            false,
            FILE_FLAG_CLEANUP_SHARPNESS,
            |reg_info: &RegInfo| reg_info.sharpness
        )?;

        self.cleanup_by_conf(
            &conf.noise,
            false,
            true,
            FILE_FLAG_CLEANUP_NOISE,
            |reg_info: &RegInfo| reg_info.noise
        )?;

        self.cleanup_by_conf(
            &conf.background,
            false,
            true,
            FILE_FLAG_CLEANUP_BG,
            |reg_info: &RegInfo| reg_info.background
        )?;

        let after_checked_cnt = self.light_files.list
            .iter()
            .filter(|f| f.used)
            .count();


        Ok(before_checked_cnt-after_checked_cnt)
    }

    fn cleanup_by_conf<F: Fn(&RegInfo) -> f32>(
        &mut self,
        conf: &ClenupConfItem,
        remove_min: bool,
        remove_max: bool,
        flags: FileFlags,
        fun: F,
    )  -> anyhow::Result<()> {
        match conf.mode {
            CleanupMode::SigmaClipping =>
                self.cleanup_by_sigma_clipping(conf, remove_min, remove_max, flags, fun),
            CleanupMode::Percent =>
                self.cleanup_by_percent(conf, remove_min, remove_max, flags, fun),
        }
    }

    fn cleanup_by_sigma_clipping<F: Fn(&RegInfo) -> f32>(
        &mut self,
        conf: &ClenupConfItem,
        remove_min: bool,
        remove_max: bool,
        flags: FileFlags,
        fun: F,
    ) -> anyhow::Result<()> {
        if !conf.used { return Ok(()); }

        let len = self.light_files.list.len();
        if len == 0 { return Ok(()); }

        let mut to_exclude = Vec::new();
        to_exclude.resize(len, false);

        let mut values = Vec::new();
        for _ in 0..conf.repeats {
            values.clear();
            for (item, to_exclude) in self.light_files.list.iter().zip(&to_exclude) {
                if *to_exclude { continue; }
                let reg_info = item.reg_info.as_ref().unwrap();
                let value = fun(reg_info);
                if value != 0.0 {
                    values.push(CalcValue::new(value as f64));
                }
            }

            if values.is_empty() {
                anyhow::bail!(gettext("Conditions are too strict"));
            }

            let (mean, dev) =
                mean_and_std_dev(&values)
                    .ok_or_else(|| anyhow::anyhow!("mean_and_std_dev"))?;

            let max = mean + dev * conf.kappa as f64;
            let min = mean - dev * conf.kappa as f64;

            for (item, to_exclude) in self.light_files.list.iter_mut().zip(&mut to_exclude) {
                if *to_exclude { continue; }
                let reg_info = item.reg_info.as_ref().unwrap();
                let value = fun(reg_info) as f64;

                if (remove_min && value < min) || (remove_max && value > max) {
                    item.used = false;
                    item.flags |= flags;
                    *to_exclude = true;
                }
            }
        }

        Ok(())
    }

    fn cleanup_by_percent<F: Fn(&RegInfo) -> f32>(
        &mut self,
        conf: &ClenupConfItem,
        remove_min: bool,
        remove_max: bool,
        flags: FileFlags,
        fun: F,
    ) -> anyhow::Result<()> {
        if !conf.used { return Ok(()); }
        if self.light_files.list.is_empty() {
            return Ok(());
        }

        let mut items = Vec::new();
        for (idx, file) in self.light_files.list.iter().enumerate() {
            let reg_info = file.reg_info.as_ref().unwrap();
            items.push((idx, fun(reg_info)));
        }

        items.sort_by(|i1, i2| cmp_f32(&i1.1, &i2.1));

        let percent_cnt = self.light_files.list.len() * conf.percent as usize / 100;

        if remove_min {
            for (idx, _) in &items[..percent_cnt] {
                self.light_files.list[*idx].used = false;
            }
        }

        if remove_max {
            for (idx, _) in &items[items.len()-percent_cnt..] {
                let item = &mut self.light_files.list[*idx];
                item.used = false;
                item.flags |= flags;
            }
        }

        Ok(())
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(default)]
pub struct GroupOptions {
    pub name: Option<String>,
}

impl Default for GroupOptions {
    fn default() -> Self {
        Self {
            name: None,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(default)]
pub struct ProjectFiles {
    pub list: Vec<ProjectFile>,
}

impl Default for ProjectFiles {
    fn default() -> Self {
        ProjectFiles {
            list: Vec::new(),
        }
    }
}

impl ProjectFiles {
    fn get_path(&self) -> PathBuf {
        assert!(!self.list.is_empty());
        self.list[0].file_name
            .parent()
            .unwrap()
            .into()
    }

    pub fn add_files(&mut self, file_info: Vec<SrcFileInfo>) {
        for info in file_info {
            self.list.push(ProjectFile::new_from_info(info))
        }
    }

    pub fn retain_files_if_they_are_not_here(&self, files: &mut Vec<PathBuf>) {
        let existing = self.list
            .iter()
            .map(|f| &f.file_name)
            .collect::<HashSet<_>>();

        files.retain(|file_name| {
            !existing.contains(file_name)
        });
    }

    fn get_master_full_file_name(&self, short_file_name: &str) -> Option<PathBuf> {
        let used_count = self.list
            .iter()
            .filter(|f| f.used)
            .count();
        if used_count != 0 {
            Some(self.get_path().join(PathBuf::from(short_file_name)))
        } else {
            None
        }
    }

    pub fn check_all(&mut self, value: bool) {
        for file in &mut self.list {
            file.used = value;
        }
    }

    pub fn check_by_indices(&mut self, indices: &[usize], value: bool) {
        for &idx in indices {
            self.list[idx].used = value;
        }
    }

    pub fn update_reg_info(&mut self, reg_info: &HashMap<PathBuf, RegInfo>) {
        for file in &mut self.list {
            if let Some(info) = reg_info.get(&file.file_name) {
                file.reg_info = Some(info.clone());
                file.flags = 0;
            }
        }
    }

    pub fn calc_total_exp_time(&self) -> f64 {
        self.list
            .iter()
            .filter(|f| f.used)
            .map(|f| f.exp.unwrap_or(0.0) as f64)
            .sum()
    }

    fn get_selected_file_names(&self) -> Vec<PathBuf> {
        self.list
            .iter()
            .filter(|f| f.used)
            .map(|f| f.file_name.clone())
            .collect()
    }

    pub fn remove_files_by_idx(&mut self, mut indices: Vec<usize>) -> Vec<ProjectFile> {
        indices.sort();
        indices
            .into_iter()
            .rev()
            .map(|idx| self.list.remove(idx))
            .collect()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(default)]
pub struct RegInfo {
    pub noise: f32,
    pub background: f32,
    pub fwhm: f32,
    pub stars: usize,
    pub stars_r_dev: f32,
    pub sharpness: f32,
}

impl Default for RegInfo {
    fn default() -> Self {
        Self {
            noise: 0.0,
            background: 0.0,
            fwhm: 0.0,
            stars: 0,
            stars_r_dev: 0.0,
            sharpness: 0.0
        }
    }
}

pub type FileFlags = u8;
pub const FILE_FLAG_CLEANUP_R_DEV:     FileFlags = 1 << 0;
pub const FILE_FLAG_CLEANUP_FWHM:      FileFlags = 1 << 1;
pub const FILE_FLAG_CLEANUP_STARS:     FileFlags = 1 << 2;
pub const FILE_FLAG_CLEANUP_SHARPNESS: FileFlags = 1 << 3;
pub const FILE_FLAG_CLEANUP_NOISE:     FileFlags = 1 << 4;
pub const FILE_FLAG_CLEANUP_BG:        FileFlags = 1 << 5;

#[derive(Serialize, Deserialize, Clone)]
#[serde(default)]
pub struct ProjectFile {
    pub used: bool,
    pub file_name: PathBuf,
    pub width: Option<usize>,
    pub height: Option<usize>,
    pub file_time: Option<DateTime<Local>>,
    pub cfa_type: Option<CfaType>,
    pub iso: Option<u32>,
    pub exp: Option<f32>,
    pub fnumber: Option<f32>,
    pub camera: Option<String>,
    pub reg_info: Option<RegInfo>,
    pub flags: FileFlags,
}

impl Default for ProjectFile {
    fn default() -> Self {
        Self {
            used: true,
            file_name: PathBuf::new(),
            file_time: None,
            cfa_type: None,
            iso: None,
            exp: None,
            width: None,
            height: None,
            fnumber: None,
            camera: None,
            reg_info: None,
            flags: 0,
        }
    }
}

impl ProjectFile {
    fn new_from_info(info: SrcFileInfo) -> ProjectFile {
        ProjectFile {
            file_name: info.file_name,
            file_time: info.file_time,
            cfa_type: info.cfa_type,
            iso: info.iso,
            exp: info.exp,
            width: Some(info.width),
            height: Some(info.height),
            fnumber: info.fnumber,
            camera: info.camera,
            .. Default::default()
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum CleanupMode {
    SigmaClipping,
    Percent,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(default)]
pub struct ClenupConfItem {
    pub used: bool,
    pub mode: CleanupMode,
    pub kappa: f32,
    pub repeats: u32,
    pub percent: u32,
}

impl ClenupConfItem {
    fn new(used: bool, mode: CleanupMode) -> Self {
        Self { used, mode, .. ClenupConfItem::default() }
    }
}

impl Default for ClenupConfItem {
    fn default() -> Self {
        Self {
            used: true,
            mode: CleanupMode::SigmaClipping,
            kappa: 1.8,
            repeats: 10,
            percent: 5,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(default)]
pub struct ClenupConf {
    pub check_before_execute: bool,
    pub stars_r_dev: ClenupConfItem,
    pub stars_fwhm: ClenupConfItem,
    pub stars_count: ClenupConfItem,
    pub img_sharpness: ClenupConfItem,
    pub noise: ClenupConfItem,
    pub background: ClenupConfItem,
}

impl Default for ClenupConf {
    fn default() -> Self {
        Self {
            check_before_execute: true,
            stars_r_dev: Default::default(),
            stars_fwhm: Default::default(),
            stars_count: ClenupConfItem{ kappa: 2.0, .. ClenupConfItem::default() },
            img_sharpness: ClenupConfItem::new(false, CleanupMode::Percent),
            noise: ClenupConfItem::new(false, CleanupMode::Percent),
            background: ClenupConfItem::new(false, CleanupMode::Percent),
        }
    }
}

pub struct StackLightsResult {
    pub file_name: PathBuf,
}