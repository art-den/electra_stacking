use std::{path::*, io::*, fs::*, collections::*, rc::*, cell::*};
use std::sync::{Arc, Mutex, atomic::AtomicBool, atomic::Ordering};
use electra_stacking::log_utils::TimeLogger;
use serde::*;
use gettextrs::*;
use chrono::prelude::*;
use electra_stacking::{
    calc::*,
    progress::*,
    stacking_utils::*,
    light_file::*,
    image_raw::*,
    image_norm::*,
    image_formats::*,
    fs_utils::*
};
use crate::config::*;

const MASTER_DARK_FN: &str = "master-dark.es_raw";
const MASTER_FLAT_FN: &str = "master-flat.es_raw";
const MASTER_BIAS_FN: &str = "master-bias.es_raw";

#[derive(Serialize, Deserialize, Clone, Default)]
#[serde(default)]
pub struct Project {
    config: ProjectConfig,
    cleanup_conf: ClenupConf,
    groups: Vec<ProjectGroup>,
    ref_image: Option<PathBuf>,
    file_name: Option<PathBuf>,

    #[serde(skip)]
    changed: Rc<Cell<bool>>,
}

pub enum CanExecStackLightsRes {
    Ok,
    NoRefFile,
}

impl Project {
    fn make_file_names_absolute(&mut self) {
        let relative_path = self.file_name
            .as_ref()
            .and_then(|file_name| file_name.parent())
            .map(|file_name| file_name.to_path_buf());
        if let Some(relative_path) = relative_path {
            for group in &mut self.groups {
                group.make_file_names_absolute(&relative_path);
            }
        }
    }

    fn make_file_names_relative(&mut self) {
        let relative_path = self.file_name
            .as_ref()
            .and_then(|file_name| file_name.parent())
            .map(|file_name| file_name.to_path_buf());
        if let Some(relative_path) = relative_path {
            for group in &mut self.groups {
                group.make_file_names_relative(&relative_path);
            }
        }
    }

    pub fn load(&mut self, file_name: &Path) -> anyhow::Result<()> {
        let reader = BufReader::new(File::open(file_name)?);
        *self = serde_json::from_reader(reader)?;
        let weak_changed = Rc::downgrade(&self.changed);
        for group in &mut self.groups {
            group.assign_project_changed_flag(weak_changed.clone());
        }
        self.file_name = Some(file_name.to_path_buf());
        self.make_file_names_absolute();
        Ok(())
    }

    pub fn save(&mut self, file_name: &Path) -> anyhow::Result<()> {
        let writer = BufWriter::new(File::create(file_name)?);
        self.file_name = Some(file_name.to_path_buf());
        self.make_file_names_relative();
        serde_json::to_writer_pretty(writer, self)?;
        self.make_file_names_absolute();
        self.reset_changed_flag();
        Ok(())
    }

    pub fn to_json_string(&self) -> String {
        serde_json::to_string(self).unwrap()
    }

    pub fn from_json_string(json_string: &str) -> Project {
        serde_json::from_str(json_string).unwrap()
    }

    pub fn changed(&self) -> bool {
        self.changed.get()
    }

    pub fn reset_changed_flag(&mut self) {
        self.changed.set(false);
    }

    pub fn make_default(&mut self) {
        self.groups.clear();
        self.add_new_group(Default::default());
        self.changed.set(false);
    }

    pub fn config(&self) -> &ProjectConfig {
        &self.config
    }

    pub fn set_config(&mut self, new_config: ProjectConfig) {
        self.config = new_config;
        self.changed.set(true);
    }

    pub fn cleanup_conf(&self) -> &ClenupConf {
        &self.cleanup_conf
    }

    pub fn set_cleanup_conf(&mut self, new_cleanup_conf: ClenupConf) {
        self.cleanup_conf = new_cleanup_conf;
        self.changed.set(true);
    }

    pub fn groups(&self) -> &Vec<ProjectGroup> {
        &self.groups
    }

    pub fn ref_image(&self) -> &Option<PathBuf> {
        &self.ref_image
    }

    pub fn set_ref_image(&mut self, new_ref_image: PathBuf) {
        if self.ref_image.as_ref() == Some(&new_ref_image) {
            return;
        }
        if let Some(prev_file_name) = self.ref_image.clone() {
            if let Some(prev_file) = self.find_file_by_name_mut(&prev_file_name) {
                prev_file.mask_as_changed();
            }
        }
        self.ref_image = Some(new_ref_image);
        if let Some(file_name) = self.ref_image.clone() {
            if let Some(file) = self.find_file_by_name_mut(&file_name) {
                file.mask_as_changed();
            }
        }
        self.changed.set(true);
    }

    pub fn file_name(&self) -> &Option<PathBuf> {
        &self.file_name
    }

    pub fn add_default_group_if_empty(&mut self) {
        if !self.groups.is_empty() { return; }
        self.make_default();
    }

    pub fn group_exists(&self, uuid: &str) -> bool {
        self.groups
            .iter()
            .any(|g| g.uuid == uuid)
    }

    pub fn group_by_index_mut(&mut self, index: usize) -> &mut ProjectGroup {
        &mut self.groups[index]
    }

    pub fn find_group_by_uuid_mut(&mut self, uuid: &str) -> Option<&mut ProjectGroup> {
        self.groups
            .iter_mut()
            .find(|g| g.uuid == uuid)
    }

    pub fn add_new_group(&mut self, options: GroupOptions) {
        let mut group = ProjectGroup {
            options,
            .. Default::default()
        };
        group.assign_project_changed_flag(Rc::downgrade(&self.changed));
        self.groups.push(group);
        self.changed.set(true);
    }

    pub fn remove_group(&mut self, group_idx: usize) -> ProjectGroup {
        let mut result = self.groups.remove(group_idx);
        self.changed.set(true);
        result.project_changed = Default::default();
        result
    }

    pub fn set_new_config(&mut self, config: ProjectConfig) {
        self.config = config;
        self.changed.set(true);
    }

    pub fn register_light_files(
        &self,
        progress:    &ProgressTs,
        cancel_flag: &Arc<AtomicBool>,
        cpu_load:    CpuLoad,
    ) -> anyhow::Result<HashMap<PathBuf, anyhow::Result<RegInfo>>> {
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

        let result = Mutex::new(HashMap::new());
        for (idx, group) in self.groups.iter().enumerate() {
            if cancel_flag.load(Ordering::Relaxed) { return Ok(HashMap::new()); }
            group.register_light_files(
                idx,
                progress,
                cancel_flag,
                &thread_pool,
                &result,
                self.config.save_common_star_img,
                &self.config.raw_params
            )?;
        }

        Ok(result.into_inner().unwrap())
    }

    pub fn update_light_files_reg_info(&mut self, reg_info: HashMap<PathBuf, anyhow::Result<RegInfo>>) {
        for group in &mut self.groups {
            group.light_files.update_reg_info(&reg_info);
        }
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

        for (idx, group) in self.groups.iter().filter(|g| g.used).enumerate() {
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
            group_with_ref_file.flat_files.get_master_full_file_name(MASTER_FLAT_FN).as_deref(),
            group_with_ref_file.dark_files.get_master_full_file_name(MASTER_DARK_FN).as_deref(),
            group_with_ref_file.bias_files.get_master_full_file_name(MASTER_BIAS_FN).as_deref(),
        )?;

        let ref_data = RefBgData::new(
            self.ref_image.as_ref().unwrap(),
            &ref_cal,
            bin,
            &self.config.raw_params
        )?;

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

            let save_aligned_mode =
                match (self.config.save_aligned_img, self.config.res_img_type) {
                    (true, ResFileType::Fit) => SaveAlignedImageMode::Fits,
                    (true, ResFileType::Tif) => SaveAlignedImageMode::Tif,
                    _                        => SaveAlignedImageMode::No,
                };

            create_temp_light_files(
                progress,
                group.light_files.get_selected_file_names(),
                group.flat_files.get_master_full_file_name(MASTER_FLAT_FN).as_deref(),
                group.dark_files.get_master_full_file_name(MASTER_DARK_FN).as_deref(),
                group.bias_files.get_master_full_file_name(MASTER_BIAS_FN).as_deref(),
                &ref_data,
                bin,
                &self.config.raw_params,
                &temp_file_names,
                &files_to_del_later,
                &thread_pool,
                cancel_flag,
                idx,
                save_aligned_mode
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
            progress,
            &temp_file_names.lock().unwrap(),
            &self.config.light_calc_opts,
            ref_data.image.image.is_rgb(),
            ref_data.image.image.width(),
            ref_data.image.image.height(),
            &result_file_name,
            cancel_flag
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
            Ok(file_name
                .with_file_name(self.config.name.as_ref().unwrap_or(&"result".to_string()).trim())
                .with_extension(self.config.res_img_type.get_file_ext())
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

    fn find_file_by_name_mut(&mut self, file_name: &Path) -> Option<&mut ProjectFile> {
        for group in &mut self.groups {
            let result = group.find_file_by_name_mut(file_name);
            if result.is_some() { return result; }
        }
        None
    }

}

#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub enum ImageSize {
    Original,
    Bin2x2,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
pub enum ResFileType {
    Fit,
    Tif,
}

impl ResFileType {
    pub fn get_file_ext(self) -> &'static str {
        match self {
            ResFileType::Fit => FIT_EXTS[0],
            ResFileType::Tif => TIF_EXTS[0],
        }
    }
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
    pub save_aligned_img: bool,
    pub save_common_star_img: bool,
    pub raw_params: RawOpenParams,
}

impl Default for ProjectConfig {
    fn default() -> Self {
        ProjectConfig {
            name: None,
            image_size: ImageSize::Original,
            light_calc_opts: CalcOpts::default(),
            dark_calc_opts: CalcOpts::default(),
            flat_calc_opts: CalcOpts{ mode: CalcMode::Mean, .. CalcOpts::default() },
            bias_calc_opts: CalcOpts::default(),
            res_img_type: ResFileType::Fit,
            save_aligned_img: false,
            save_common_star_img: false,
            raw_params: RawOpenParams::default(),
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
    options: GroupOptions,
    used: bool,
    uuid: String,
    pub light_files: ProjectFiles,
    pub dark_files: ProjectFiles,
    pub bias_files: ProjectFiles,
    pub flat_files: ProjectFiles,

    #[serde(skip)]
    project_changed: Weak<Cell<bool>>,
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
            project_changed: Default::default(),
        }
    }
}

impl ProjectGroup {
    fn assign_project_changed_flag(&mut self, project_changed: Weak<Cell<bool>>) {
        self.light_files.assign_project_changed_flag(project_changed.clone());
        self.dark_files.assign_project_changed_flag(project_changed.clone());
        self.bias_files.assign_project_changed_flag(project_changed.clone());
        self.flat_files.assign_project_changed_flag(project_changed.clone());
        self.project_changed = project_changed;
    }

    fn make_file_names_absolute(&mut self, relative_path: &Path) {
        self.light_files.make_file_names_absolute(relative_path);
        self.dark_files.make_file_names_absolute(relative_path);
        self.bias_files.make_file_names_absolute(relative_path);
        self.flat_files.make_file_names_absolute(relative_path);
    }

    fn make_file_names_relative(&mut self, relative_path: &Path) {
        self.light_files.make_file_names_relative(relative_path);
        self.dark_files.make_file_names_relative(relative_path);
        self.bias_files.make_file_names_relative(relative_path);
        self.flat_files.make_file_names_relative(relative_path);
    }

    pub fn options(&self) -> &GroupOptions {
        &self.options
    }

    pub fn set_options(&mut self, new_options: GroupOptions) {
        self.options = new_options;
        self.project_changed.upgrade().unwrap().set(true);
    }

    pub fn used(&self) -> bool {
        self.used
    }

    pub fn uuid(&self) -> &str {
        &self.uuid
    }

    pub fn set_used(&mut self, used: bool) {
        if self.used == used {
            return;
        }
        self.used = used;
        self.project_changed.upgrade().unwrap().set(true);
    }

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
            gettext("Main group")
        } else {
            format!("{} #{}", gettext("Group"), group_index+1)
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

    pub fn file_list_by_type_mut(&mut self, file_type: ProjectFileType) -> &mut ProjectFiles {
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
        let from_files = self.file_list_by_type_mut(from_type);
        let files_to_move = from_files.remove_files_by_idx(file_indices);
        let to_files = self.file_list_by_type_mut(to_type);
        to_files.add_files(files_to_move);
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
            thread_pool
        )?;

        self.create_master_dark(
            group_index,
            progress,
            cancel_flag,
            &config.dark_calc_opts,
            thread_pool
        )?;

        self.create_master_flat(
            group_index,
            progress,
            cancel_flag,
            &config.flat_calc_opts,
            &self.bias_files.get_master_full_file_name(MASTER_BIAS_FN),
            thread_pool,
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
        where F: FnOnce(&[PathBuf], &Path) -> anyhow::Result<bool>
    {
        if cancel_flag.load(Ordering::Relaxed) { return Ok(false); }
        let file_names = calibr_files.get_selected_file_names();
        if file_names.is_empty() { return Ok(false); }
        let file_name = calibr_files.get_master_full_file_name(file_name).unwrap();
        create_fun(&file_names, &file_name)
    }

    fn register_light_files(
        &self,
        group_idx:     usize,
        progress:      &ProgressTs,
        cancel_flag:   &Arc<AtomicBool>,
        thread_pool:   &rayon::ThreadPool,
        result:        &Mutex<HashMap<PathBuf, anyhow::Result<RegInfo>>>,
        save_star_img: bool,
        raw_params:    &RawOpenParams,
    ) -> anyhow::Result<()> {
        progress.lock().unwrap().stage(&format!(
            "Registering files for group {}...",
            self.name(group_idx)
        ));
        progress.lock().unwrap()
            .set_total(self.light_files.list.len());
        progress.lock().unwrap()
            .progress(false, &gettext(
                "Loading calibration master files..."
            ));

        let cal_data = CalibrationData::load(
            self.flat_files.get_master_full_file_name(MASTER_FLAT_FN).as_deref(),
            self.dark_files.get_master_full_file_name(MASTER_DARK_FN).as_deref(),
            self.bias_files.get_master_full_file_name(MASTER_BIAS_FN).as_deref(),
        )?;

        let cur_result = Mutex::new(anyhow::Result::<()>::Ok(()));

        let file_names: Vec<_> = self.light_files.list
            .iter()
            .map(|f| f.file_name.clone())
            .collect();

        thread_pool.scope(|s| {
            for file_name in file_names {
                s.spawn(|_| {
                    let file_name = file_name;
                    if cancel_flag.load(Ordering::Relaxed)
                    || cur_result.lock().unwrap().is_err() {
                        return;
                    }

                    let time_log = TimeLogger::start();

                    let load_light_file_res = LightFile::load_and_calc_params(
                        &file_name,
                        &cal_data,
                          LoadLightFlags::STARS
                        | LoadLightFlags::STARS_STAT
                        | LoadLightFlags::NOISE
                        | LoadLightFlags::BACKGROUND,
                        OpenMode::Processing,
                        1,
                        raw_params
                    );

                    let file_result = match load_light_file_res {
                        Ok(light_file) => {
                            let stars_stat = light_file.stars_stat.unwrap();
                            if save_star_img {
                                let common_star_img_fn = file_name.with_extension("common_star.tif");
                                let save_res = save_grayscale_image_to_tiff_file(
                                    &stars_stat.common_stars_img,
                                    &ImageInfo::default(),
                                    &common_star_img_fn
                                );
                                if let Err(err) = save_res {
                                    *cur_result.lock().unwrap() = Err(anyhow::anyhow!(
                                        r#"Error "{}" during saving common star image to file "{}""#,
                                        err.to_string(),
                                        common_star_img_fn.to_str().unwrap_or("")
                                    ));
                                    return;
                                }
                            }
                            Ok(RegInfo {
                                noise:       light_file.noise,
                                background:  light_file.background,
                                fwhm:        stars_stat.fwhm,
                                stars:       light_file.stars.len(),
                                stars_r_dev: stars_stat.aver_r_dev,
                            })
                        },

                        Err(err) =>
                            Err(err),
                    };

                    result.lock().unwrap().insert(
                        file_name.clone(),
                        file_result
                    );

                    progress.lock().unwrap()
                        .progress(true, file_name.to_str().unwrap_or(""));

                    time_log.log("Register file TOTAL");
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
            if (file.flags & FILE_FLAG_ERROR) != 0 { continue; }
            if file.reg_info.is_none() { return false; }
        }
        true
    }

    pub fn cleanup_light_files(&mut self, conf: &ClenupConf) -> anyhow::Result<usize> {
        if conf.check_before_execute {
            for file in &mut self.light_files.list {
                if (file.flags & FILE_FLAG_ERROR) == 0 {
                    file.set_used(true);
                    file.set_flags(0);
                } else {
                    file.set_used(false);
                }
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
        conf:         &ClenupConfItem,
        remove_min:   bool,
        remove_max:   bool,
        flags_to_set: FileFlags,
        fun:          F,
    )  -> anyhow::Result<()> {
        match conf.mode {
            CleanupMode::SigmaClipping =>
                self.cleanup_by_sigma_clipping(conf, remove_min, remove_max, flags_to_set, fun),
            CleanupMode::Percent =>
                self.cleanup_by_percent(conf, remove_min, remove_max, flags_to_set, fun),
            CleanupMode::MinMax =>
                self.cleanup_by_min_max(conf, flags_to_set, fun),
        }
    }

    fn cleanup_by_sigma_clipping<F: Fn(&RegInfo) -> f32>(
        &mut self,
        conf:         &ClenupConfItem,
        remove_min:   bool,
        remove_max:   bool,
        flags_to_set: FileFlags,
        fun:          F,
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
                if *to_exclude || (item.flags & FILE_FLAG_ERROR) != 0 {
                    continue;
                }
                if let Some(reg_info) = item.reg_info.as_ref() {
                    let value = fun(reg_info);
                    if value != 0.0 {
                        values.push(CalcValue::new(value as f64));
                    }
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
                if *to_exclude || (item.flags & FILE_FLAG_ERROR) != 0 {
                    continue;
                }
                if let Some(reg_info) = item.reg_info.as_ref() {
                    let value = fun(reg_info) as f64;
                    if (remove_min && value < min) || (remove_max && value > max) {
                        item.set_used(false);
                        item.set_flags(*item.flags() | flags_to_set);
                        *to_exclude = true;
                    }
                }
            }
        }

        Ok(())
    }

    fn cleanup_by_percent<F: Fn(&RegInfo) -> f32>(
        &mut self,
        conf:         &ClenupConfItem,
        remove_min:   bool,
        remove_max:   bool,
        flags_to_set: FileFlags,
        fun:          F,
    ) -> anyhow::Result<()> {
        if !conf.used { return Ok(()); }
        if self.light_files.list.is_empty() {
            return Ok(());
        }

        let mut items = Vec::new();
        for (idx, file) in self.light_files.list.iter().enumerate() {
            if (file.flags & FILE_FLAG_ERROR) != 0 {
                continue;
            }
            if let Some(reg_info) = file.reg_info.as_ref() {
                items.push((idx, fun(reg_info)));
            }
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
                item.set_used(false);
                item.set_flags(*item.flags() | flags_to_set);
            }
        }

        Ok(())
    }

    fn cleanup_by_min_max<F: Fn(&RegInfo) -> f32>(
        &mut self,
        conf:         &ClenupConfItem,
        flags_to_set: FileFlags,
        fun:          F,
    ) -> anyhow::Result<()> {
        for file in &mut self.light_files.list {
            if (file.flags & FILE_FLAG_ERROR) != 0 {
                continue;
            }
            if let Some(reg_info) = file.reg_info.as_ref() {
                let value = fun(reg_info);
                if let Some(min) = conf.min { if value < min {
                    file.set_used(false);
                    file.set_flags(*file.flags() | flags_to_set);
                }}
                if let Some(max) = conf.max { if value > max {
                    file.set_used(false);
                    file.set_flags(*file.flags() | flags_to_set);
                }}
            }
        }
        Ok(())
    }

    fn find_file_by_name_mut(&mut self, file_name: &Path) -> Option<&mut ProjectFile> {
        for ftype in [
            &mut self.light_files,
            &mut self.dark_files,
            &mut self.flat_files,
            &mut self.bias_files
        ] {
            let result = ftype.find_file_by_name_mut(file_name);
            if result.is_some() { return result; }
        }
        None
    }


}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
#[serde(default)]
pub struct GroupOptions {
    pub name: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Default)]
#[serde(default)]
pub struct ProjectFiles {
    list: Vec<ProjectFile>,

    #[serde(skip)]
    project_changed: Weak<Cell<bool>>,
}

impl ProjectFiles {
    fn assign_project_changed_flag(&mut self, project_changed: Weak<Cell<bool>>) {
        for file in &mut self.list {
            file.assign_project_changed_flag(project_changed.clone());
        }
        self.project_changed = project_changed;
    }

    fn make_file_names_absolute(&mut self, relative_path: &Path) {
        for file in &mut self.list {
            file.make_file_names_absolute(relative_path);
        }
    }

    fn make_file_names_relative(&mut self, relative_path: &Path) {
        for file in &mut self.list {
            file.make_file_names_relative(relative_path);
        }
    }

    pub fn list(&self) -> &Vec<ProjectFile> {
        &self.list
    }

    pub fn file_by_index_mut(&mut self, index: usize) -> &mut ProjectFile {
        &mut self.list[index]
    }

    fn get_path(&self) -> PathBuf {
        assert!(!self.list.is_empty());
        self.list[0].file_name
            .parent()
            .unwrap()
            .into()
    }

    pub fn add_files_from_src_file_info(&mut self, file_info: Vec<ImageInfo>) {
        if file_info.is_empty() {
            return;
        }
        for info in file_info {
            let mut file = ProjectFile::new_from_info(info);
            file.project_changed = self.project_changed.clone();
            self.list.push(file)
        }
        self.project_changed.upgrade().unwrap().set(true);
    }

    pub fn add_files(&mut self, files: Vec<ProjectFile>) {
        if files.is_empty() {
            return;
        }
        for mut file in files {
            file.project_changed = self.project_changed.clone();
            self.list.push(file);
        }
        self.project_changed.upgrade().unwrap().set(true);
    }

    pub fn remove_files_by_idx(&mut self, mut indices: Vec<usize>) -> Vec<ProjectFile> {
        if indices.is_empty() {
            return vec![];
        }
        indices.sort_unstable();
        let result = indices
            .iter()
            .rev()
            .map(|&idx| self.list.remove(idx))
            .map(|mut file| { file.project_changed = Default::default(); file })
            .collect();
        self.project_changed.upgrade().unwrap().set(true);
        result
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
            if (file.flags & FILE_FLAG_ERROR) != 0 {
                continue;
            }
            file.set_used(value);
        }
    }

    pub fn check_by_indices(&mut self, indices: &[usize], value: bool) {
        for &idx in indices {
            let file = &mut self.list[idx];
            if (file.flags & FILE_FLAG_ERROR) != 0 {
                continue;
            }
            file.set_used(value);
        }
    }

    pub fn update_reg_info(&mut self, reg_info: &HashMap<PathBuf, anyhow::Result<RegInfo>>) {
        for file in &mut self.list {
            if let Some(info) = reg_info.get(&file.file_name) {
                match info {
                    Ok(info) => {
                        file.set_reg_info(Some(info.clone()));
                        file.set_flags(0);
                        file.set_error_text(None);
                    },
                    Err(err) => {
                        file.set_reg_info(None);
                        file.set_flags(FILE_FLAG_ERROR);
                        file.set_error_text(Some(err.to_string()));
                        file.set_used(false);
                    },
                }
            }
        }
    }

    pub fn get_checked_count(&self) -> usize {
        self.list
            .iter()
            .filter(|f| f.used)
            .count()
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

    fn find_file_by_name_mut(&mut self, file_name: &Path) -> Option<&mut ProjectFile> {
        self.list.iter_mut().find(|f| f.file_name == file_name)
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
}

impl Default for RegInfo {
    fn default() -> Self {
        Self {
            noise: 0.0,
            background: 0.0,
            fwhm: 0.0,
            stars: 0,
            stars_r_dev: 0.0,
        }
    }
}

pub type FileFlags = u16;
pub const FILE_FLAG_CLEANUP_R_DEV:     FileFlags = 1 << 0;
pub const FILE_FLAG_CLEANUP_FWHM:      FileFlags = 1 << 1;
pub const FILE_FLAG_CLEANUP_STARS:     FileFlags = 1 << 2;
pub const FILE_FLAG_CLEANUP_NOISE:     FileFlags = 1 << 3;
pub const FILE_FLAG_CLEANUP_BG:        FileFlags = 1 << 4;
pub const FILE_FLAG_ERROR:             FileFlags = 1 << 15;

#[derive(Serialize, Deserialize, Clone)]
#[serde(default)]
pub struct ProjectFile {
    used: bool,
    file_name: PathBuf,
    width: Option<usize>,
    height: Option<usize>,
    file_time: Option<DateTime<Local>>,
    cfa_type: Option<CfaType>,
    iso: Option<u32>,
    exp: Option<f32>,
    fnumber: Option<f32>,
    focal_len: Option<f32>,
    camera: Option<String>,
    reg_info: Option<RegInfo>,
    flags: FileFlags,
    error_text: Option<String>,

    #[serde(skip)]
    project_changed: Weak<Cell<bool>>,

    #[serde(skip)]
    change_count: u32,
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
            focal_len: None,
            camera: None,
            reg_info: None,
            flags: 0,
            error_text: None,
            project_changed: Default::default(),
            change_count: 0,
        }
    }
}

impl ProjectFile {
    fn assign_project_changed_flag(&mut self, project_changed: Weak<Cell<bool>>) {
        self.project_changed = project_changed;
    }

    fn make_file_names_absolute(&mut self, relative_path: &Path) {
        use path_absolutize::*;
        if let Ok(file_name) = self.file_name.absolutize_from(relative_path) {
            self.file_name = file_name.to_path_buf();
        }
    }

    fn make_file_names_relative(&mut self, relative_path: &Path) {
        use pathdiff::diff_paths;
        if let Some(file_name) = diff_paths(&self.file_name, relative_path) {
            self.file_name = file_name;
        }
    }

    fn new_from_info(info: ImageInfo) -> ProjectFile {
        ProjectFile {
            file_name: info.file_name,
            file_time: info.file_time,
            cfa_type: info.cfa_type,
            iso: info.iso,
            exp: info.exp.map(|v| v as f32),
            width: Some(info.width),
            height: Some(info.height),
            focal_len: info.focal_len,
            fnumber: info.fnumber,
            camera: info.camera,
            .. Default::default()
        }
    }

    pub fn used(&self) -> bool {
        self.used
    }

    pub fn set_used(&mut self, used: bool) {
        if self.used == used { return; }
        self.used = used;
        self.change_count += 1;
        self.project_changed.upgrade().unwrap().set(true);
    }

    pub fn change_count(&self) -> u32 {
        self.change_count
    }

    pub fn mask_as_changed(&mut self) {
        self.change_count += 1;
    }

    pub fn file_name(&self) -> &PathBuf {
        &self.file_name
    }

    pub fn width(&self) -> &Option<usize> {
        &self.width
    }

    pub fn height(&self) -> &Option<usize> {
        &self.height
    }

    pub fn file_time(&self) -> &Option<DateTime<Local>> {
        &self.file_time
    }

    pub fn iso(&self) -> &Option<u32> {
        &self.iso
    }

    pub fn exp(&self) -> &Option<f32> {
        &self.exp
    }

    pub fn fnumber(&self) -> &Option<f32> {
        &self.fnumber
    }

    pub fn focal_len(&self) -> &Option<f32> {
        &self.focal_len
    }

    pub fn camera(&self) -> &Option<String> {
        &self.camera
    }

    pub fn reg_info(&self) -> &Option<RegInfo> {
        &self.reg_info
    }

    pub fn set_reg_info(&mut self, info: Option<RegInfo>) {
        self.reg_info = info;
        self.change_count += 1;
        self.project_changed.upgrade().unwrap().set(true);
    }

    pub fn flags(&self) -> &FileFlags {
        &self.flags
    }

    pub fn set_flags(&mut self, flags: FileFlags) {
        if self.flags == flags { return; }
        self.flags = flags;
        self.change_count += 1;
        self.project_changed.upgrade().unwrap().set(true);
    }

    pub fn set_error_text(&mut self, error_text: Option<String>) {
        if self.error_text == error_text { return; }
        self.error_text = error_text;
        self.change_count += 1;
        self.project_changed.upgrade().unwrap().set(true);
    }

    pub fn get_error_test(&self) -> Option<&str> {
        self.error_text.as_deref()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum CleanupMode {
    SigmaClipping,
    Percent,
    MinMax,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(default)]
pub struct ClenupConfItem {
    pub used: bool,
    pub mode: CleanupMode,
    pub kappa: f32,
    pub repeats: u32,
    pub percent: u32,
    pub min: Option<f32>,
    pub max: Option<f32>,
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
            min: None,
            max: None,
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
    pub noise: ClenupConfItem,
    pub background: ClenupConfItem,
}

impl Default for ClenupConf {
    fn default() -> Self {
        Self {
            check_before_execute: true,
            stars_r_dev: Default::default(),
            stars_fwhm: Default::default(),
            stars_count: ClenupConfItem{ kappa: 3.0, .. ClenupConfItem::default() },
            noise: ClenupConfItem::new(false, CleanupMode::Percent),
            background: ClenupConfItem::new(false, CleanupMode::Percent),
        }
    }
}

pub struct StackLightsResult {
    pub file_name: PathBuf,
}