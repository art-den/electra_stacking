use std::{path::*};
use bitflags::bitflags;
use itertools::Itertools;
use serde::{Serialize, Deserialize};
use crate::{image::*, image_formats::*, image_raw::*, stars::*, log_utils::*, calc::*};

bitflags! { pub struct LoadLightFlags: u32 {
    const STARS = 1;
    const NOISE = 2;
    const BACKGROUND = 4;
    const SHARPNESS = 8;
    const FAST_DEMOSAIC = 16;
}}

pub struct LightFile {
    pub image:      Image,
    pub exif:       Exif,
    pub grey:       ImageLayerF32,
    pub stars:      Stars,
    pub noise:      f32,
    pub background: f32,
    pub sharpness:  f32,
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
            load_raw_light_file(
                file_name,
                cal_data,
                disk_mutex,
                flags.contains(LoadLightFlags::FAST_DEMOSAIC)
            )
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

        let mut temp_values = Vec::new();

        let noise = if flags.contains(LoadLightFlags::NOISE)
                    || flags.contains(LoadLightFlags::STARS)
        {
            let noise_log = TimeLogger::start();
            let result = calc_noise(&mut temp_values, &grey_image);
            noise_log.log("noise calculation");
            result as f32
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

        let stars = if flags.contains(LoadLightFlags::STARS) {
            let stars_log = TimeLogger::start();
            let result = find_stars_on_image(&grey_image, &src_data.image, Some(noise));
            stars_log.log("looking for stars on image");
            log::info!("stars count = {}", result.len());
            result
        } else {
            Stars::new()
        };

        let freq = if flags.contains(LoadLightFlags::SHARPNESS) {
            let f_log = TimeLogger::start();
            let img = if src_data.image.is_greyscale() {
                &src_data.image.l
            } else {
                &src_data.image.g
            };
            let result = calc_sharpness(&img);
            f_log.log("freq calculation");
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
            sharpness: freq,
        })
    }
}

fn check_raw_data(
    raw_info:    &RawImageInfo,
    cal_info:    &RawImageInfo,
    mode:        &str,
    file_name:   &Path,
    master_dark: bool
) -> anyhow::Result<()> {
    let compare = |item, raw: &dyn std::fmt::Display, cal: &dyn std::fmt::Display| -> anyhow::Result<()> {
        if raw.to_string() != cal.to_string() {
            anyhow::bail!(
                "{} differs for {}: ('{}' != '{}') for file {}",
                item, mode, raw, cal, file_name.to_str().unwrap_or("")
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
    file_name:     &PathBuf,
    cal_data:      &CalibrationData,
    disk_mutex:    Option<&std::sync::Mutex<()>>,
    fast_demosaic: bool,
) -> anyhow::Result<SrcImageData> {
    // load raw file
    let raw_log = TimeLogger::start();
    let mut raw_image = RawImage::load_camera_raw_file(
        file_name,
          RawLoadFlags::EXTRACT_BLACK
        | RawLoadFlags::INF_OVEREXPOSURES,
        disk_mutex
    )?;
    raw_log.log("loading raw image");

    // extract master-bias image
    if let Some(bias) = &cal_data.bias_image {
        check_raw_data(&raw_image.info, &bias.info, "master bias", file_name, false)?;
        raw_image.data -= &bias.data;
    }

    // extract master-dark image
    if let Some(dark) = &cal_data.dark_image {
        check_raw_data(&raw_image.info, &dark.info, "master dark", file_name, true)?;
        raw_image.data -= &dark.data;
    }

    // flatten by master-flat
    if let Some(flat) = &cal_data.flat_image {
        check_raw_data(&raw_image.info, &flat.info, "master flat", file_name, false)?;
        raw_image.data *= &flat.data;
    }

    // remove hot pixels from RAW image
    raw_image.remove_hot_pixels(&cal_data.hot_pixels);

    // do demosaic
    let dem_log = TimeLogger::start();
    let image = raw_image.demosaic_linear(fast_demosaic)?;
    if fast_demosaic {
        dem_log.log("fast demosaic");
    } else {
        dem_log.log("demosaic");
    }

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

    // use lower 25% of noise_values to calc noise
    const PART: usize = 4;
    let pos = temp_values.len() / PART;
    temp_values.select_nth_unstable_by(pos, cmp_f32);
    let sum = temp_values[0..pos]
        .iter()
        .fold(0_f64, |acc, v| acc + *v as f64);
    f64::sqrt(sum / pos as f64) * PART as f64
}

fn calc_background(temp_values: &mut Vec<f32>, grey_image: &ImageLayerF32) -> f32 {
    let grey_slice = grey_image.as_slice();
    temp_values.resize(grey_slice.len(), 0.0);
    temp_values.copy_from_slice(grey_slice);
    let pos = temp_values.len() / 4;
    *temp_values.select_nth_unstable_by(pos, cmp_f32).1
}

fn calc_sharpness(grey_image: &ImageLayerF32) -> f32 {
    let mut high_freq = Vec::new();
    let mut mid_freq = Vec::new();
    let mut calc_values = Vec::new();

    let mut calc_by_dir = |dir| {
        high_freq.clear();
        mid_freq.clear();
        grey_image.foreach_row_and_col(
            dir,
            |values, _, _| {
                for (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) in values.iter().tuple_windows() {
                    let high = (v4+v5-v6-v7).abs() / 2.0;
                    let mid = (v1+v2+v3+v4+v5-v6-v7-v8-v9-v10).abs() / 5.0;
                    if high.is_infinite() || mid.is_infinite() || high.is_nan() || mid.is_nan() {
                        continue;
                    }
                    high_freq.push(high);
                    mid_freq.push(mid);
                }
            }
        );

        let top_pos = high_freq.len() - high_freq.len() / 1000;
        let low_pos = high_freq.len() / 10;

        high_freq.select_nth_unstable_by(top_pos, cmp_f32);
        calc_values.clear();
        for v in &high_freq[top_pos..] {
            calc_values.push(*v as f64);
        }
        let top_high = mean_f64(&calc_values);

        high_freq.select_nth_unstable_by(low_pos, cmp_f32);
        calc_values.clear();
        for v in &high_freq[..low_pos] {
            calc_values.push(*v as f64);
        }
        let low_high = mean_f64(&calc_values);

        mid_freq.select_nth_unstable_by(top_pos, cmp_f32);
        calc_values.clear();
        for v in &mid_freq[top_pos..] {
            calc_values.push(*v as f64);
        }
        let top_mid = mean_f64(&calc_values);

        mid_freq.select_nth_unstable_by(low_pos, cmp_f32);
        calc_values.clear();
        for v in &mid_freq[..low_pos] {
            calc_values.push(*v as f64);
        }
        let low_mid = mean_f64(&calc_values);

        (top_high - low_high) / (top_mid - low_mid)
    };

    let by_cols_value = calc_by_dir(IterType::Cols);
    let by_rows_value = calc_by_dir(IterType::Rows);

    by_cols_value.min(by_rows_value) as f32
}

#[derive(Serialize, Deserialize)]
pub struct LightFileRegInfo {
    pub file_name: String,
    pub noise: f32,
    pub background: f32,
    pub stars_r: f32,
    pub stars_r_dev: f32,
}

