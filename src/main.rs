#![allow(dead_code)]

mod image;
mod image_norm;
mod image_raw;
mod image_formats;
mod light_file;
mod fs_utils;
mod log_utils;
mod calc;
mod consts;
mod stars;
mod tests;
mod progress;
mod cmd_merge_lrgb;
mod cmd_create_master_dark;
mod cmd_create_master_flat;
mod cmd_stack_lights;
mod cmd_register;
mod cmd_cleanup;
mod cmd_convert;

use std::path::*;
use log::info;
use structopt::StructOpt;
use flexi_logger::*;

#[derive(StructOpt, Debug)]
enum SubCommands {
    /// Merge l,r,g,b files into color one
    MergeLRGB(cmd_merge_lrgb::CmdOptions),

    /// Create master dark file from several dark files
    MasterDark(cmd_create_master_dark::CmdOptions),

    /// Create master flat file from several flat files
    MasterFlat(cmd_create_master_flat::CmdOptions),

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
/*    let raw = image_raw::RawImage::load_camera_raw_file(
        &r"F:\temp\23\IMG_8405.CR2".into(),
        image_raw::RawLoadFlags::APPLY_BLACK_AND_WB
    )?;
    let image = raw.demosaic_linear()?;
    image_formats::save_image_to_file(&image, &raw.info.exif, &r"F:\result.tif".into())?;
    return Ok(());
*/
/*/
    let cal_data = light_file::CalibrationData { dark_image: None, flat_image: None, hot_pixels: Vec::new(), };
    for _ in 0..10 {
        let _file = light_file::LightFile::load(
            &r"F:\astro\test_2\lights2\00000250.DNG".into(),
            &cal_data,
            light_file::LoadLightFlags::STARS
        )?;
    }
    return Ok(());
*/
    let opt = Opt::from_args();

    if let Some(log_path) = &opt.log_path {
        start_logger(log_path)?;
        info!("Program started. Options = {:#?}", opt.cmd);
    }

    let result = match opt.cmd {
        SubCommands::MergeLRGB(opts) =>
            cmd_merge_lrgb::execute(opts),

        SubCommands::MasterDark(opts) =>
            cmd_create_master_dark::execute(opts),

        SubCommands::MasterFlat(opts) =>
            cmd_create_master_flat::execute(opts),

        SubCommands::StackLights(opts) =>
            cmd_stack_lights::execute(opts),

        SubCommands::Register(opts) =>
            cmd_register::execute(opts),

        SubCommands::Cleanup(opts) =>
            cmd_cleanup::execute(opts),

        SubCommands::Convert(opts) =>
            cmd_convert::execute(opts),
    };

    result
}

fn start_logger(log_path: &PathBuf) -> anyhow::Result<()> {
    let custom_format_fun = |
        w:      &mut dyn std::io::Write,
        now:    &mut DeferredNow,
        record: &Record
    | -> Result<(), std::io::Error> {
        write!(
            w, "[{}] {} {}",
            now.format(TS_DASHES_BLANK_COLONS_DOT_BLANK),
            record.level(),
            record.args()
        )
    };

    Logger::try_with_str("info")?
        .log_to_file(
            FileSpec::default()
                .directory(log_path)
                .basename("astro-utils")
        )
        .format(custom_format_fun)
        .print_message()
        .start()?;

    Ok(())
}