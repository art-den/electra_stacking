use std::{path::*};
use bitflags::bitflags;
use itertools::Itertools;
use serde::{Serialize, Deserialize};
use crate::{image::*, image_formats::*, image_raw::*, stars::*, log_utils::*, calc::*, fs_utils};

bitflags! { pub struct LoadLightFlags: u32 {
    const STARS = 1;
    const NOISE = 2;
    const BACKGROUND = 4;
}}

pub struct LightFile {
    pub image:      Image,
    pub exif:       Exif,
    pub grey:       ImageLayerF32,
    pub stars:      Stars,
    pub noise:      f64,
    pub background: f32,
}

impl LightFile {
    pub fn load(
        file_name:  &PathBuf,
        cal_data:   &CalibrationData,
        disk_mutex: Option<&std::sync::Mutex<()>>,
        flags:      LoadLightFlags,
        bin:        usize,
    ) -> anyhow::Result<LightFile> {
        let mut src_data = if is_image_file_name(file_name) {
            load_image_from_file(file_name)
        } else {
            load_raw_light_file(file_name, cal_data, disk_mutex)
        }?;

        if bin == 2 {
            let bin_log = TimeLogger::start();
            src_data.image = src_data.image.decrease_2x();
            bin_log.log("decreasing image 2x");
        }

        let grey_image = if !flags.is_empty() {
            let grey_log = TimeLogger::start();
            let mut result = src_data.image.create_greyscale_layer();
            grey_log.log("creating grey image");
            let infareas_log = TimeLogger::start();
            result.fill_inf_areas();
            infareas_log.log("fill_inf_areas");
            result
        } else {
            ImageLayerF32::new_empty()
        };

        let stars = if flags.contains(LoadLightFlags::STARS) {
            let stars_log = TimeLogger::start();
            let result = find_stars_on_image(&grey_image);
            stars_log.log("looking for stars on image");
            log::info!("stars count = {}", result.len());
            result
        } else {
            Stars::new()
        };

        let mut temp_values = Vec::new();

        let noise = if flags.contains(LoadLightFlags::NOISE) {
            let noise_log = TimeLogger::start();
            let result = calc_noise(&mut temp_values, &grey_image);
            noise_log.log("noise calculation");
            result
        } else {
            0.0
        };

        let background = if flags.contains(LoadLightFlags::BACKGROUND) {
            let bg_log = TimeLogger::start();
            let result = calc_background(&mut temp_values, &grey_image);
            bg_log.log("background calculation");
            result
        } else {
            0.0
        };

        Ok(LightFile{
            exif: src_data.exif,
            image: src_data.image,
            grey: grey_image,
            stars,
            background,
            noise,
        })
    }
}

fn check_raw_data(
    raw_info:    &RawImageInfo,
    cal_info:    &RawImageInfo,
    mode:        &str,
    master_dark: bool
) -> anyhow::Result<()> {
    let compare = |item, raw: &dyn std::fmt::Display, cal: &dyn std::fmt::Display| -> anyhow::Result<()> {
        if raw.to_string() != cal.to_string() {
            anyhow::bail!(
                "{} differs for {}: '{}' and '{}'",
                item, mode, raw, cal
            );
        }
        Ok(())
    };

    compare("Width", &raw_info.width, &cal_info.width)?;
    compare("Height", &raw_info.height, &cal_info.height)?;
    let raw_cam = raw_info.exif.camera.as_ref().and_then(|v| Some(&v[..])).unwrap_or("");
    let cal_cam = cal_info.exif.camera.as_ref().and_then(|v| Some(&v[..])).unwrap_or("");
    compare("Camera model", &raw_cam, &cal_cam)?;
    compare("Color pattern", &raw_info.cfa, &cal_info.cfa)?;

    if master_dark {
        compare("ISO", &raw_info.exif.iso.unwrap_or(0), &cal_info.exif.iso.unwrap_or(0))?;
        let raw_exp_time = format!("{:.1}", raw_info.exif.exp_time.unwrap_or(0.0));
        let cal_exp_time = format!("{:.1}", cal_info.exif.exp_time.unwrap_or(0.0));
        compare("Exposure time", &raw_exp_time, &cal_exp_time)?;
    }

    Ok(())
}

fn load_raw_light_file(
    file_name:  &PathBuf,
    cal_data:   &CalibrationData,
    disk_mutex: Option<&std::sync::Mutex<()>>,
) -> anyhow::Result<SrcImageData> {
    // load raw file
    let raw_log = TimeLogger::start();
    let mut raw_image = RawImage::load_camera_raw_file(
        file_name,
          RawLoadFlags::APPLY_BLACK_AND_WB
        | RawLoadFlags::INF_OVEREXPOSURES,
        disk_mutex
    )?;
    raw_log.log("loading raw image");

    // extract master-dark image
    if let Some(dark) = &cal_data.dark_image {
        check_raw_data(&raw_image.info, &dark.info, "master dark", true)?;
        let dark_log = TimeLogger::start();
        raw_image.data -= &dark.data;
        dark_log.log("remove dark data");
    }

    // flatten by master-flat
    if let Some(flat) = &cal_data.flat_image {
        check_raw_data(&raw_image.info, &flat.info, "master flat", false)?;
        let flat_log = TimeLogger::start();
        raw_image.data *= &flat.data;
        flat_log.log("flatten image");
    }

    // remove hot pixels from RAW image
    let hp_log = TimeLogger::start();
    raw_image.remove_hot_pixels(&cal_data.hot_pixels);
    hp_log.log("remowing hot pixels");

    // do demosaic
    let dem_log = TimeLogger::start();
    let image = raw_image.demosaic_linear()?;
    dem_log.log("demosaic");

    // return result
    Ok(SrcImageData {
        image,
        exif: raw_image.info.exif,
    })
}

#[inline(never)]
fn calc_noise(temp_values: &mut Vec<f32>, grey_image: &ImageLayerF32) -> f64 {
    temp_values.clear();
    for (b1, b2, v, a1, a2) in grey_image.iter().copied().tuple_windows() {
        let back = (b1 + b2 + a1 + a2) * 0.25;
        let diff = v - back;
        temp_values.push(diff*diff);
    }

    // use lower 10% of noise_values to calc noise
    let pos = temp_values.len() / 10;
    temp_values.select_nth_unstable_by(pos, cmp_f32);
    let sum = temp_values[0..pos]
        .iter()
        .fold(0_f64, |acc, v| acc + *v as f64);
    f64::sqrt(sum / pos as f64)
}

fn calc_background(temp_values: &mut Vec<f32>, grey_image: &ImageLayerF32) -> f32 {
    let grey_slice = grey_image.as_slice();
    temp_values.resize(grey_slice.len(), 0.0);
    temp_values.copy_from_slice(grey_slice);
    let pos = temp_values.len() / 4;
    *temp_values.select_nth_unstable_by(pos, cmp_f32).1
}

#[derive(Serialize, Deserialize)]
pub struct LightFileRegInfo {
    pub file_name: String,
    pub noise: f64,
    pub background: f32,
    pub stars_r: f64,
    pub stars_r_dev: f64,
}

pub struct CalibrationData {
    pub dark_image: Option<RawImage>,
    pub flat_image: Option<RawImage>,
    pub hot_pixels: Vec<HotPixel>,
}

impl CalibrationData {
    pub fn load(
        master_flat: &Option<PathBuf>,
        master_dark: &Option<PathBuf>
    ) -> anyhow::Result<CalibrationData> {
        let dark_image = match master_dark {
            Some(file_name) => {
                log::info!(
                    "loading master dark '{}'...",
                    fs_utils::path_to_str(file_name)
                );
                Some(RawImage::new_from_internal_format_file(file_name)?)
            }
            None => None,
        };

        let hot_pixels = match &dark_image {
            Some(image) => {
                let result = image.find_hot_pixels();
                log::info!("hot pixels count = {}", result.len());
                result
            }
            None => Vec::new(),
        };

        let flat_image = match master_flat {
            Some(file_name) => {
                log::info!(
                    "loading master flat '{}'...",
                    fs_utils::path_to_str(file_name)
                );
                let mut image = RawImage::new_from_internal_format_file(file_name)?;
                image.remove_hot_pixels(&hot_pixels);
                let filter_log = TimeLogger::start();
                let mut image = image.filter_flat_image();
                filter_log.log("filtering flat image");
                for v in image.data.iter_mut() { *v = 1.0 / *v; }
                Some(image)
            }
            None => None,
        };

        Ok(CalibrationData {
            dark_image,
            flat_image,
            hot_pixels
        })
    }
}
