#![allow(dead_code)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::new_without_default)]

use std::path::*;
use structopt::StructOpt;
use electra_stacking::*;

mod cmd_cleanup;
mod cmd_convert;
mod cmd_create_master_dark;
mod cmd_create_master_flat;
mod cmd_create_master_bias;
mod cmd_register;
mod cmd_stack_lights;
mod consts;

#[derive(StructOpt, Debug)]
enum SubCommands {
    /// Create master dark file from several dark files
    MasterDark(cmd_create_master_dark::CmdOptions),

    /// Create master flat file from several flat files
    MasterFlat(cmd_create_master_flat::CmdOptions),

    /// Create master flat file from several flat files
    MasterBias(cmd_create_master_bias::CmdOptions),

    /// Stack light files into result light file
    StackLights(cmd_stack_lights::CmdOptions),

    /// Register light files
    Register(cmd_register::CmdOptions),

    /// Cleanup bad light files
    Cleanup(cmd_cleanup::CmdOptions),

    /// Convert image from one format to another
    Convert(cmd_convert::CmdOptions),
}

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(flatten)]
    cmd: SubCommands,

    /// Path for saving log file
    #[structopt(long, parse(from_os_str))]
    log_path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();

    if let Some(log_path) = &opt.log_path {
        log_utils::start_logger(log_path)?;
        log::info!("Program started. Options = {:#?}", opt.cmd);
    }

    match opt.cmd {
        SubCommands::MasterDark(opts) =>
            cmd_create_master_dark::execute(opts),

        SubCommands::MasterFlat(opts) =>
            cmd_create_master_flat::execute(opts),

        SubCommands::MasterBias(opts) =>
            cmd_create_master_bias::execute(opts),

        SubCommands::StackLights(opts) =>
            cmd_stack_lights::execute(opts),

        SubCommands::Register(opts) =>
            cmd_register::execute(opts),

        SubCommands::Cleanup(opts) =>
            cmd_cleanup::execute(opts),

        SubCommands::Convert(opts) =>
            cmd_convert::execute(opts),
    }
}
