use structopt::*;
use std::{path::*};
use crate::{calc::*, consts::*, fs_utils::*, image_raw::*, progress::*};

#[derive(StructOpt, Debug)]
pub struct CmdOptions {
    /// path to directory containing RAW flat files
    #[structopt(short, long, parse(from_os_str))]
    path: PathBuf,

    /// Extension of RAW flat files
    #[structopt(short, long, default_value = DEF_RAW_EXTS)]
    exts: String,

    #[structopt(flatten)]
    calc_opts: CalcOpts,

    /// File name of result master flat file (with .raw extension)
    #[structopt(short, long, parse(from_os_str))]
    result_file: PathBuf,
}

fn postprocess_single_flat_image_color(
    raw_image:   &mut RawImage,
    cc:          CfaColor,
    black_level: f32,
    white_level: f32) -> bool
{
    let center_x = raw_image.info.width / 2;
    let center_y = raw_image.info.height / 2;
    let center_size = (raw_image.info.width + raw_image.info.height) / 40; // have to be odd

    let mut data = Vec::new();

    for (x, y, v) in raw_image.data.iter_rect_crd(
        center_x-center_size,
        center_y-center_size+1,
        center_x+center_size,
        center_y+center_size+1)
    {
        if raw_image.info.cfa.get_pixel_color(x, y) != cc { continue; }
        data.push(v);
    }

    if data.is_empty() { return true; }

    let high_index = 95 * data.len() / 100;
    let center_max = *data.select_nth_unstable_by(high_index, cmp_f32).1;
    let center_level_percent = 100.0 * (center_max - black_level) / (white_level - black_level);

    log::info!("{:?} -> {:.2}% at center", cc, center_level_percent);

    if center_level_percent > 90.0 {
        log::info!("Dropped due to overexposure");
        return false;
    }

    data.clear();
    for (x, y, v) in raw_image.data.iter_crd() {
        if raw_image.info.cfa.get_pixel_color(x, y) != cc { continue; }
        data.push(v);
    }

    if data.is_empty() { return true; }

    let high_index = 99 * data.len() / 100;
    let norm_value = *data.select_nth_unstable_by(high_index, cmp_f32).1 - black_level;

    for (x, y, v) in raw_image.data.iter_crd_mut() {
        if raw_image.info.cfa.get_pixel_color(x, y) != cc { continue; }
        *v = (*v - black_level) / norm_value;
    }

    true
}

fn postprocess_single_flat_image(raw_image: &mut RawImage) -> bool {
    let mono_ok = postprocess_single_flat_image_color(
        raw_image,
        CfaColor::Mono,
        raw_image.info.black_values[0],
        raw_image.info.max_values[0],
    );

    let r_ok = postprocess_single_flat_image_color(
        raw_image,
        CfaColor::R,
        raw_image.info.black_values[0],
        raw_image.info.max_values[0]
    );

    let g_ok = postprocess_single_flat_image_color(
        raw_image,
        CfaColor::G,
        raw_image.info.black_values[1],
        raw_image.info.max_values[1]
    );

    let b_ok = postprocess_single_flat_image_color(
        raw_image,
        CfaColor::B,
        raw_image.info.black_values[2],
        raw_image.info.max_values[2]
    );

    raw_image.info.max_values.fill(1.0);
    raw_image.info.black_values.fill(0.0);

    mono_ok && r_ok && g_ok && b_ok
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    create_master_file(
        &options.path,
        &options.exts,
        &options.calc_opts,
        &options.result_file,
        RawLoadFlags::empty(),
        |path| get_temp_flat_file_name(path),
        postprocess_single_flat_image,
        |_| (),
        ProgressConsole::new_ts()
    )
}