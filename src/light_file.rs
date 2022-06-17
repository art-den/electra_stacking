use std::{path::*};
use bitflags::bitflags;
use itertools::Itertools;
use crate::{image::*, image_formats::*, image_raw::*, stars::*, log_utils::*, calc::*};


bitflags! { pub struct LoadLightFlags: u32 {
    const STARS       = 1;
    const STARS_STAT  = 2;
    const NOISE       = 4;
    const BACKGROUND  = 8;
}}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum OpenMode {
    Preview,
    Processing,
}

pub struct LightFile {
    pub image:      Image,
    pub info:       ImageInfo,
    pub stars:      Stars,
    pub stars_stat: Option<StarsStat>,
    pub noise:      f32,
    pub background: f32,
}

impl LightFile {
    pub fn load_and_calc_params(
        file_name: &Path,
        cal_data:  &CalibrationData,
        flags:     LoadLightFlags,
        open_mode: OpenMode,
        bin:       usize,
    ) -> anyhow::Result<LightFile> {
        log::info!(
            "LightFile::load_and_calc_params: file_name={}, flags={:?}, open_mode={:?}, bin={}",
            file_name.to_str().unwrap_or(""),
            flags,
            open_mode,
            bin
        );

        let stars_flag = flags.contains(LoadLightFlags::STARS);
        let stars_stat_flag = flags.contains(LoadLightFlags::STARS_STAT);

        let image_data = load_image_from_file(file_name)?;

        let (mut image, mut overexposures) = match image_data.image {
            RawOrImage::Image(image) =>
                (image, Vec::new()),
            RawOrImage::Raw(mut raw) => {
                let demosaic = if bin == 2 {
                    DemosaicAlgo::Linear
                } else { match open_mode {
                    OpenMode::Preview => DemosaicAlgo::Linear,
                    OpenMode::Processing => DemosaicAlgo::ColorRatio,
                }};

                let mut overexposures = raw.get_overexposures();

                raw.extract_black();

                raw.calibrate(cal_data)?;

                let log = TimeLogger::start();
                let result = raw.demosaic(
                    demosaic,
                    open_mode == OpenMode::Preview
                )?;
                log.log(&format!("demosaicing {:?}", demosaic));

                overexposures.retain(|&(x, y)|
                    !cal_data.hot_pixels.contains(&BadPixel { x, y })
                );

                (result, overexposures)
            },
        };

        let img_layer_to_calc = if image.is_greyscale() {
            &image.l
        } else {
            &image.g
        };

        let noise = if stars_flag || stars_stat_flag || flags.contains(LoadLightFlags::NOISE) {
            let noise_log = TimeLogger::start();
            let result = calc_noise(img_layer_to_calc);
            noise_log.log("noise calculation");
            result as f32
        } else {
            0.0
        };

        let background = if flags.contains(LoadLightFlags::BACKGROUND) {
            let bg_log = TimeLogger::start();
            let result = calc_background(img_layer_to_calc);
            bg_log.log("background calculation");
            result
        } else {
            0.0
        };

        drop(img_layer_to_calc);

        if bin == 2 {
            let bin_log = TimeLogger::start();
            image = image.decrease_2x();
            bin_log.log("decreasing image 2x");
        }

        let img_layer_to_calc = if image.is_greyscale() {
            &image.l
        } else {
            &image.g
        };

        let stars = if stars_flag || stars_stat_flag {
            let stars_log = TimeLogger::start();
            let result = find_stars_on_image(
                &img_layer_to_calc,
                Some(&image),
                Some(noise),
                true
            )?;
            stars_log.log("looking for stars on image");
            log::info!("stars count = {}", result.len());
            result
        } else {
            Stars::new()
        };

        let stars_stat = if stars_stat_flag {
            Some(calc_stars_stat(&stars, &img_layer_to_calc)?)
        } else {
            None
        };

        drop(img_layer_to_calc);

        if bin == 2 {
            for (x, y) in &mut overexposures {
                *x /= 2;
                *y /= 2;
            }
        }
        image.mark_overexposures(&overexposures);

        Ok(LightFile{
            info: image_data.info,
            image,
            stars,
            stars_stat,
            background,
            noise,
        })
    }
}

#[inline(never)]
fn calc_noise(image: &ImageLayerF32) -> f64 {
    let mut temp_values = Vec::with_capacity(image.as_slice().len());
    for (b1, b2, v, a1, a2) in image.iter().copied().tuple_windows() {
        let back = (b1 + b2 + a1 + a2) * 0.25;
        let diff = v - back;
        temp_values.push(diff*diff);
    }

    // use lower 25% of noise_values to calc noise
    const PART: usize = 4;
    let pos = temp_values.len() / PART;
    temp_values.select_nth_unstable_by(pos, cmp_f32);
    let sum = temp_values[0..pos]
        .iter()
        .fold(0_f64, |acc, v| acc + *v as f64);
    f64::sqrt(sum / pos as f64) * PART as f64
}

fn calc_background(image: &ImageLayerF32) -> f32 {
    let mut temp_values = Vec::with_capacity(image.as_slice().len());
    for v in image.as_slice() { temp_values.push(*v); }
    let pos = temp_values.len() / 4;
    *temp_values.select_nth_unstable_by(pos, cmp_f32).1
}
