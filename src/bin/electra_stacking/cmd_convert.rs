use std::path::*;
use structopt::*;
use electra_stacking::image_formats::*;

#[derive(StructOpt, Debug)]
pub struct CmdOptions {
    /// name of file to convert
    #[structopt(long, short, parse(from_os_str))]
    src_file: PathBuf,

    /// name of destination file
    #[structopt(long, short, parse(from_os_str))]
    dst_file: PathBuf,
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    let file = load_image_from_file(&options.src_file)?;

    if let RawOrImage::Image(mut image) = file.image {
        image.check_contains_inf_or_nan(true, true)?;
        image.normalize_if_greater_1();
        save_image_to_file(&image, &file.info, &options.dst_file)?;
        Ok(())
    } else {
        anyhow::bail!("Raw format is not supported");
    }
}