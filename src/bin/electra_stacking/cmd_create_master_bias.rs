use structopt::*;
use std::{path::*, sync::Arc, sync::atomic::AtomicBool};
use crate::{
    fs_utils::*,
    stacking_utils::*,
    calc::*,
    consts::*,
    progress::*,
};

#[derive(StructOpt, Debug)]
pub struct CmdOptions {
    /// path to directory containing RAW bias files
    #[structopt(short, long, parse(from_os_str))]
    path: PathBuf,

    /// Extension of RAW bias files
    #[structopt(short, long, default_value = DEF_RAW_EXTS)]
    exts: String,

    #[structopt(flatten)]
    calc_opts: CalcOpts,

    /// File name of result master bias file (with .raw extension)
    #[structopt(short, long, parse(from_os_str))]
    result_file: PathBuf,

    /// Number of parallel tasks
    #[structopt(long, default_value = "1")]
    num_tasks: usize,
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    let files_list = get_files_list(&options.path, &options.exts, true)?;
    let cancel_flag = Arc::new(AtomicBool::new(false));
    let progress = ProgressConsole::new_ts();

    let thread_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(options.num_tasks)
        .build()?;

    create_master_dark_or_bias_file(
        &files_list,
        &options.calc_opts,
        &options.result_file,
        &progress,
        &thread_pool,
        &cancel_flag,
    )?;

    Ok(())
}
