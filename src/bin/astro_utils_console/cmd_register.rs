use std::{path::*, sync::*};
use structopt::*;
use crossbeam::sync::WaitGroup;
use crate::{consts::*, progress::*, fs_utils::*, light_file::*, stars::*, calc::*};

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

    /// Extensions of RAW lights files
    #[structopt(long, default_value = DEF_RAW_EXTS)]
    exts: String,

    /// Number of parallel tasks
    #[structopt(long, default_value = "1")]
    num_tasks: usize,
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    let progress = Arc::new(Mutex::new(ProgressConsole::new()));
    progress.lock().unwrap().progress(false, "Searching files...");

    let file_names_list = get_files_list(&options.path, &options.exts, true)?;
    progress.lock().unwrap().set_total(file_names_list.len());

    let cal_data = Arc::new(CalibrationData::load(
        &options.master_flat,
        &options.master_dark,
    )?);

    let disk_access_mutex = Arc::new(Mutex::new(()));
    let thread_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(options.num_tasks)
        .build()?;

    let all_tasks_finished_waiter = WaitGroup::new();

    for file_name in file_names_list.iter() {
        let wait = all_tasks_finished_waiter.clone();
        let file_name = file_name.clone();
        let progress = Arc::clone(&progress);
        let cal_data = Arc::clone(&cal_data);
        let disk_access_mutex = Arc::clone(&disk_access_mutex);

        thread_pool.spawn(move || {
            progress.lock().unwrap().progress(true, extract_file_name(&file_name));

            let light_file = LightFile::load(
                &file_name,
                &cal_data,
                Some(&disk_access_mutex),
                LoadLightFlags::STARS
                | LoadLightFlags::NOISE
                | LoadLightFlags::BACKGROUND,
                1
            ).expect("Can't load light file");

            let stars_stat = calc_stars_stat(&light_file.stars);

            let file_data = LightFileRegInfo {
                file_name:   extract_file_name(&file_name).to_string(),
                noise:       light_file.noise,
                background:  light_file.background,
                stars_r:     stars_stat.aver_r,
                stars_r_dev: stars_stat.aver_r_dev,
            };

            let file_data_str = serde_json::to_string_pretty(&file_data).expect("Can't serialize");
            let info_file_name = get_light_info_file_name(&file_name);
            std::fs::write(info_file_name, &file_data_str).expect("Can't write file registration info");

            drop(wait)
        });
    }

    all_tasks_finished_waiter.wait();

    progress.lock().unwrap().percent(0, 100, "Done!");
    progress.lock().unwrap().next_step();

    Ok(())
}

struct StarsStat {
    aver_r: f64,
    aver_r_dev: f64,
}

fn calc_stars_stat(stars: &Stars) -> StarsStat {
    let mut r_values = Vec::new();
    let mut r_dev_values = Vec::new();
    for star in stars.iter() {
        r_values.push(CalcValue::new(star.radius));
        if !star.overexposured {
            r_dev_values.push(CalcValue::new(star.radius_std_dev));
        }
    }

    let aver_r_opt = cappa_sigma_weighted(&mut r_values, 3.0, 5, true, true);
    let aver_r = if let Some(res) = aver_r_opt { res.result } else { 0.0 };

    let aver_r_dev_opt = cappa_sigma_weighted(&mut r_dev_values, 3.0, 5, true, true);
    let aver_r_dev = if let Some(res) = aver_r_dev_opt { res.result } else { 0.0 };

    StarsStat { aver_r, aver_r_dev }
}
