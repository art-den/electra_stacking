use std::{path::*, sync::{*, atomic::AtomicBool}};
use anyhow::bail;
use structopt::*;
use crate::{
    consts::*,
    calc::*,
    fs_utils::*,
    progress::*,
    image_raw::*,
    image_norm::*,
    stacking_utils::*,
};

#[derive(StructOpt, Debug)]
pub struct CmdOptions {
    /// path to directory containing RAW flat files
    #[structopt(long, parse(from_os_str))]
    path: Vec<PathBuf>,

    /// Master-flat file
    #[structopt(long, parse(from_os_str))]
    master_flat: Vec<PathBuf>,

    /// Master-dark file
    #[structopt(long, parse(from_os_str))]
    master_dark: Vec<PathBuf>,

    /// Reference light file
    #[structopt(long, parse(from_os_str))]
    ref_file: PathBuf,

    /// Extensions of RAW lights files
    #[structopt(long, default_value = DEF_RAW_EXTS)]
    exts: String,

    #[structopt(flatten)]
    calc_opts: CalcOpts,

    /// File name of result file (with .tif extension)
    #[structopt(long, parse(from_os_str))]
    result_file: PathBuf,

    /// Number of parallel tasks
    #[structopt(long, default_value = "1")]
    num_tasks: usize,

    /// reduce image size by `bin` times.
    /// Only value 2 is supported
    #[structopt(long)]
    bin: Option<usize>,
}

struct DirData {
    path: PathBuf,
    master_dark: Option<PathBuf>,
    master_flat: Option<PathBuf>,
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    if options.path.is_empty() {
        bail!("You must define at least one --path");
    }

    let mut dir_data = Vec::new();

    if options.path.len() == 1 {
        if options.master_dark.len() > 1 {
            bail!("Wrong amount of master dark files");
        }
        if options.master_flat.len() > 1 {
            bail!("Wrong amount of master flat files");
        }

        dir_data.push(DirData {
            path: options.path[0].clone(),
            master_dark:
                if !options.master_dark.is_empty() { Some(options.master_dark[0].clone()) }
                else { None },
            master_flat:
                if !options.master_flat.is_empty() { Some(options.master_flat[0].clone()) }
                else { None },
        });
    } else {
        if options.path.len() != options.master_dark.len() {
            bail!("Wrong amount of master dark files")
        }
        if options.path.len() != options.master_flat.len() {
            bail!("Wrong amount of master flat files")
        }

        for i in 0..options.path.len() {
            dir_data.push(DirData {
                path:        options.path[i].clone(),
                master_dark: Some(options.master_dark[i].clone()),
                master_flat: Some(options.master_flat[i].clone()),
            });
        }
    }

    let progress = ProgressConsole::new_ts();
    let cancel_flag = Arc::new(AtomicBool::new(false));

    progress.lock().unwrap().percent(0, 100, "Loading reference file...");

    let ref_cal = CalibrationData::load(
        &dir_data[0].master_flat,
        &dir_data[0].master_dark,
        &None // TODO: Add bias support for console version
    )?;

    let ref_bg_data = RefBgData::new(
        &options.ref_file,
        &ref_cal,
        options.bin.unwrap_or(1)
    )?;

    let thread_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(options.num_tasks)
        .build()?;

    let temp_file_names = Mutex::new(Vec::<TempFileData>::new());
    let files_to_del_later = Mutex::new(FilesToDeleteLater::new());

    for item in dir_data.iter() {
        progress.lock().unwrap().percent(0, 100, "Searching files...");
        log::info!("Searching files in {} ...", path_to_str(&item.path));
        let file_names_list = get_files_list(&item.path, &options.exts, true)?;

        create_temp_light_files(
            &progress,
            file_names_list,
            &item.master_flat,
            &item.master_dark,
            &None, // TODO: Add bias support for console version
            &ref_bg_data,
            options.bin.unwrap_or(1),
            &temp_file_names,
            &files_to_del_later,
            &thread_pool,
            &cancel_flag
        )?;

        progress.lock().unwrap().percent(
            100, 100,
            format!("{} done", path_to_str(&item.path)).as_str()
        );
    }

    if temp_file_names.lock().unwrap().is_empty() {
        anyhow::bail!("Nothing to merge");
    }

    merge_temp_light_files(
        &progress,
        &temp_file_names.lock().unwrap(),
        &options.calc_opts,
        ref_bg_data.image.image.is_rgb(),
        ref_bg_data.image.image.width(),
        ref_bg_data.image.image.height(),
        &options.result_file,
        &cancel_flag
    )?;

    drop(files_to_del_later);

    Ok(())
}
