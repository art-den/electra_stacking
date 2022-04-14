use std::path::*;
use structopt::*;
use astro_utils::image_formats::*;

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
    let mut image = load_image_from_file(&options.src_file)?;
    image.image.check_contains_inf_or_nan()?;
    image.image.normalize_if_greater_1();
    save_image_to_file(&image.image, &image.exif, &options.dst_file)?;
    Ok(())
}