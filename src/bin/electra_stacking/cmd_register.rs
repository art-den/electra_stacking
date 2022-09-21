use std::{path::*, sync::*};
use structopt::*;
use serde::{Serialize, Deserialize};
use crate::{consts::*, image_raw::*, progress::*, fs_utils::*, light_file::*};

#[derive(StructOpt, Debug)]
pub struct CmdOptions {
    /// path to directory containing RAW flat files
    #[structopt(long, parse(from_os_str))]
    path: PathBuf,

    /// Master-flat file
    #[structopt(long, parse(from_os_str))]
    master_flat: Option<PathBuf>,

    /// Master-dark file
    #[structopt(long, parse(from_os_str))]
    master_dark: Option<PathBuf>,

    /// Master-bias file
    #[structopt(long, parse(from_os_str))]
    master_bias: Option<PathBuf>,

    /// Extensions of RAW lights files
    #[structopt(long, default_value = DEF_RAW_EXTS)]
    exts: String,

    /// Number of parallel tasks
    #[structopt(long, default_value = "1")]
    num_tasks: usize,
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    let progress = Arc::new(Mutex::new(ProgressConsole::new()));
    progress.lock().unwrap()
        .progress(false, "Searching files...");

    let file_names_list = get_files_list(&options.path, &options.exts, true)?;
    progress.lock().unwrap()
        .set_total(file_names_list.len());

    let cal_data = Arc::new(CalibrationData::load(
        options.master_flat.as_deref(),
        options.master_dark.as_deref(),
        options.master_bias.as_deref()
    )?);

    let thread_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(options.num_tasks)
        .build()?;

    thread_pool.scope(|s| {
        for file_name in file_names_list.iter() {
            s.spawn(|_| {
                progress.lock().unwrap()
                    .progress(true, extract_file_name(file_name));

                let light_file = LightFile::load_and_calc_params(
                    file_name,
                    &cal_data,
                      LoadLightFlags::STARS
                    | LoadLightFlags::STARS_STAT
                    | LoadLightFlags::NOISE
                    | LoadLightFlags::BACKGROUND,
                    OpenMode::Processing,
                    1,
                    &Default::default(),
                    false,
                ).expect("Can't load light file");

                let stars_stat = light_file.stars_stat.unwrap();

                let file_data = LightFileRegInfo {
                    file_name:   extract_file_name(file_name).to_string(),
                    noise:       light_file.noise,
                    background:  light_file.background,
                    stars_r:     stars_stat.fwhm,
                    stars_r_dev: stars_stat.aver_r_dev,
                };

                let file_data_str = serde_json::to_string_pretty(&file_data)
                    .expect("Can't serialize");
                let info_file_name = get_light_info_file_name(file_name);
                std::fs::write(info_file_name, &file_data_str)
                    .expect("Can't write file registration info");
            });
        }
    });

    progress.lock().unwrap().percent(0, 100, "Done!");

    Ok(())
}

#[derive(Serialize, Deserialize)]
pub struct LightFileRegInfo {
    pub file_name: String,
    pub noise: f32,
    pub background: f32,
    pub stars_r: f32,
    pub stars_r_dev: f32,
}
