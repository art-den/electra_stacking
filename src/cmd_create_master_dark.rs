use structopt::*;
use std::{path::*};
use crate::{
    fs_utils::*,
    stacking_utils::*,
    calc::*,
    consts::*,
    progress::*,
    image_raw::*
};

#[derive(StructOpt, Debug)]
pub struct CmdOptions {
    /// path to directory containing RAW dark files
    #[structopt(short, long, parse(from_os_str))]
    path: PathBuf,

    /// Extension of RAW dark files
    #[structopt(short, long, default_value = DEF_RAW_EXTS)]
    exts: String,

    #[structopt(flatten)]
    calc_opts: CalcOpts,

    /// File name of result master dark file (with .raw extension)
    #[structopt(short, long, parse(from_os_str))]
    result_file: PathBuf,

    /// Number of parallel tasks
    #[structopt(long, default_value = "1")]
    num_tasks: usize,
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    create_master_file(
        &options.path,
        &options.exts,
        &options.calc_opts,
        &options.result_file,
        RawLoadFlags::APPLY_BLACK_AND_WB,
        |path| get_temp_dark_file_name(path),
        |_| true,
        |_| (),
        ProgressConsole::new_ts(),
        options.num_tasks
    )
}
