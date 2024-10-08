use std::{path::*, collections::{HashSet, HashMap}, hash::Hash};
use itertools::{izip, Itertools};
use serde::{Serialize, Deserialize};
use crate::{image::*, fs_utils, log_utils::*, calc::*, image_io::*};

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq)]
pub enum CfaColor {
    R,
    G,
    B,
    Mono,
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Debug)]
pub enum CfaType {
    GBRG,
    RGGB,
    BGGR,
    GRBG,
}

impl CfaType {
    pub fn from_string(text: &str) -> Option<CfaType> {
        match text {
            "GBRG" => Some(CfaType::GBRG),
            "RGGB" => Some(CfaType::RGGB),
            "BGGR" => Some(CfaType::BGGR),
            "GRBG" => Some(CfaType::GRBG),
            _ => None,
        }
    }

    pub fn get_arr(self) -> CfaArr {
        use CfaColor::*;
        match self {
            CfaType::GBRG => [[G, B], [R, G]],
            CfaType::RGGB => [[R, G], [G, B]],
            CfaType::BGGR => [[B, G], [G, R]],
            CfaType::GRBG => [[G, R], [B, G]],
        }
    }
}

type CfaArr = [[CfaColor; 2]; 2];

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Debug)]
pub struct CfaPattern {
    pub pattern_type: CfaType,
    arr: CfaArr,
}

impl CfaPattern {
    #[inline(always)]
    fn get_color_type(&self, x: Crd, y: Crd) -> CfaColor {
        self.arr[(y & 1) as usize][(x & 1) as usize]
    }

    fn get_row(&self, y: Crd) -> &[CfaColor; 2] {
        &self.arr[(y & 1) as usize]
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Debug)]
pub enum Cfa {
    Mono,
    Pattern(CfaPattern),
}

impl Default for Cfa {
    fn default() -> Self {
        Cfa::Mono
    }
}

impl Cfa {
    pub fn from_str(cfa_str: &str) -> Cfa {
        if let Some(ct) = CfaType::from_string(cfa_str) {
            Self::from_cfa_type(Some(ct))
        } else {
            Cfa::Mono
        }
    }

    pub fn from_cfa_type(ct: Option<CfaType>) -> Cfa {
        match ct {
            Some(ct) => Cfa::Pattern(CfaPattern {
                pattern_type: ct,
                arr: ct.get_arr()
            }),
            None =>
                Cfa::Mono,
        }
    }

    #[inline(always)]
    pub fn get_pixel_color(&self, x: Crd, y: Crd) -> CfaColor {
        match self {
            Cfa::Mono       => CfaColor::Mono,
            Cfa::Pattern(p) => p.get_color_type(x, y),
        }
    }
}

impl std::fmt::Display for Cfa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DemosaicAlgo {
    Linear,
    ColorRatio,
}

pub struct RawImage {
    pub info: RawImageInfo,
    pub data: ImageLayerF32,
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub struct BadPixel {
    pub x: Crd,
    pub y: Crd,
}

impl RawImage {
    pub fn new_from_info(info: RawImageInfo) -> RawImage {
        let width = info.width;
        let height = info.height;
        RawImage {
            info,
            data: ImageLayer::new(width, height)
        }
    }

    pub fn load(file_name: &Path) -> anyhow::Result<(RawImage, ImageInfo)> {
        let raw = rawloader::decode_file(file_name)?;

        let crop_left = raw.crops[3] as Crd;
        let crop_top = raw.crops[0] as Crd;
        let width = raw.width as Crd - raw.crops[1] as Crd - crop_left;
        let height = raw.height as Crd - raw.crops[2] as Crd - crop_top;

        let mut info = ImageInfo::default();
        if let Some(raw_exif) = &raw.exif {
            info.exp = raw_exif.get_rational(rawloader::Tag::ExposureTime).map(|v| v as f64);
            info.fnumber = raw_exif.get_rational(rawloader::Tag::FNumber);
            info.iso = raw_exif.get_uint(rawloader::Tag::ISOSpeed);
            info.focal_len = raw_exif.get_rational(rawloader::Tag::FocalLength);
            info.camera = raw_exif.get_str(rawloader::Tag::Model).map(|v| v.to_string());
        }

        let cam_to_rgb = if !raw.xyz_to_cam[0][0].is_nan() {
            use nalgebra::*;

            let xyz_2_cam = matrix![
                raw.xyz_to_cam[0][0], raw.xyz_to_cam[0][1], raw.xyz_to_cam[0][2];
                raw.xyz_to_cam[1][0], raw.xyz_to_cam[1][1], raw.xyz_to_cam[1][2];
                raw.xyz_to_cam[2][0], raw.xyz_to_cam[2][1], raw.xyz_to_cam[2][2];
            ];

            let srgb_2_xyz = matrix![
                0.4124564, 0.3575761, 0.1804375;
                0.2126729, 0.7151522, 0.0721750;
                0.0193339, 0.1191920, 0.9503041;
            ];

            let mut srgb_to_cam = xyz_2_cam * srgb_2_xyz;

            for row in 0..3 {
                let sum: f32 = srgb_to_cam.row(row).sum();
                if sum != 0.0 { for v in srgb_to_cam.row_mut(row).iter_mut() {
                    *v /= sum;
                }}
            }

            let srgb_to_cam = srgb_to_cam.normalize();
            let cam_to_rgb = srgb_to_cam.try_inverse().unwrap();

            Some([
                cam_to_rgb[0], cam_to_rgb[1], cam_to_rgb[2],
                cam_to_rgb[3], cam_to_rgb[4], cam_to_rgb[5],
                cam_to_rgb[6], cam_to_rgb[7], cam_to_rgb[8],
            ])
        } else {
            None
        };

        let mut raw_info = RawImageInfo {
            width, height,
            max_values:   [0.0; 4],
            black_values: [0.0; 4],
            wb:           [0.0; 4],
            cam_to_rgb,
            cfa:          Cfa::from_str(&raw. cropped_cfa().name),
            camera:       info.camera.clone(),
            exposure:     info.exp.map(|v| v as f32),
            iso:          info.iso,
        };

        let mut data = ImageLayer::<f32>::new(width, height);

        fn copy_raw_data<T: Into<f32> + Copy>(
            dst: &mut [f32],
            src: &[T],
            raw_width: usize,
            crop_left: usize,
            crop_top: usize,
            image_width: usize,
            image_height: usize)
        {
            for y in 0..image_height {
                let dst_start = y * image_width;
                let dst = &mut dst[dst_start..dst_start+image_width];
                let src_start = (y+crop_top)*raw_width + crop_left;
                let src = &src[src_start..src_start+image_width];
                for (s, d) in src.iter().zip(dst) {
                    *d = (*s).into();
                }
            }
        }

        match raw.data {
            rawloader::RawImageData::Integer(raw_data) => {
                copy_raw_data(
                    data.as_slice_mut(),
                    &raw_data,
                    raw.width,
                    crop_left as usize,
                    crop_top as usize,
                    width as usize,
                    height as usize,
                );
            }
            rawloader::RawImageData::Float(raw_data) => {
                copy_raw_data(
                    data.as_slice_mut(),
                    &raw_data,
                    raw.width,
                    crop_left as usize,
                    crop_top as usize,
                    width as usize,
                    height as usize,
                );
            }
        }

        let mut min_wb = raw.wb_coeffs
            .iter()
            .copied()
            .filter(|v| !v.is_nan())
            .min_by(cmp_f32)
            .unwrap_or(1.0);

        if min_wb == 0.0 { min_wb = 1.0; }

        for i in 0..4 {
            raw_info.black_values[i] = raw.blacklevels[i] as f32;
            raw_info.max_values[i] = raw.whitelevels[i] as f32;
            raw_info.wb[i] =
                if !raw.wb_coeffs[i].is_nan() {
                    raw.wb_coeffs[i] / min_wb
                } else {
                    0.0
                };
        }

        Ok((RawImage{ info: raw_info, data }, info))
    }

    pub fn get_overexposures(&self) -> Vec<(Crd, Crd)> {
        let mut max_values = [0_f32; 4];

        let clip_value = Self::find_clip_value(self.data.as_slice());
        for i in 0..4 {
            let white_value = self.info.max_values[i];
            max_values[i] =
                if clip_value < white_value
                && white_value < 1.2 * clip_value {
                    clip_value
                } else {
                    white_value
                };
        }

        let mut overexposures = Vec::new();
        let mut calc = |color, max| {
            for (x, y, v) in self.data.iter_crd() {
                if self.info.cfa.get_pixel_color(x, y) != color {
                    continue;
                }

                if v >= max {
                    overexposures.push((x, y));
                }
            }
        };

        const K: f32 = 0.97;

        calc(CfaColor::Mono, max_values[0] * K);
        calc(CfaColor::R, max_values[0] * K);
        calc(CfaColor::G, max_values[1] * K);
        calc(CfaColor::B, max_values[2] * K);

        overexposures
    }

    pub fn extract_black(&mut self) {
        let mut extract = |color, black_level| {
            for (x, y, v) in self.data.iter_crd_mut() {
                if self.info.cfa.get_pixel_color(x, y) != color {
                    continue;
                }

                *v -= black_level;
            }
        };

        extract(CfaColor::Mono, self.info.black_values[0]);
        extract(CfaColor::R, self.info.black_values[0]);
        extract(CfaColor::G, self.info.black_values[1]);
        extract(CfaColor::B, self.info.black_values[2]);

        for i in 0..4 {
            self.info.max_values[i] -= self.info.black_values[i];
            self.info.black_values[i] = 0.0;
        }
    }

    pub fn calibrate(&mut self, cal_data: &CalibrationData) -> anyhow::Result<()> {
        // extract master-bias image
        if let Some(bias) = &cal_data.bias_image {
            CalibrationData::is_usable_for_raw(&self.info, &bias.info, "master bias", false)?;
            self.data -= &bias.data;
        }

        // extract master-dark image
        if let Some(dark) = &cal_data.dark_image {
            CalibrationData::is_usable_for_raw(&self.info, &dark.info, "master dark", true)?;

            // Allow 20% of difference in exposure times
            let cal_exp = dark.info.exposure.unwrap_or(0.0);
            let exp = self.info.exposure.unwrap_or(0.0);
            let exp_diff = (cal_exp - exp).abs();
            if exp_diff == 0.0 || exp_diff < exp * 0.2 {
                self.data -= &dark.data;
            } else {
                log::info!("Master dark is used only for hot bixels because exposures differ")
            }
        }

        // flatten by master-flat
        if let Some(flat) = &cal_data.flat_image {
            CalibrationData::is_usable_for_raw(&self.info, &flat.info, "master flat", false)?;
            self.data *= &flat.data;
        }

        // remove hot pixels from RAW image
        self.remove_bad_pixels(&cal_data.hot_pixels);

        Ok(())
    }

    fn find_clip_value(data: &[f32]) -> f32 {
        let mut max_value = 1e10;
        loop {
            let max_image_value = data.iter()
                .copied()
                .filter(|v| *v < max_value)
                .max_by(cmp_f32)
                .unwrap_or(0.0);

            let count = data.iter()
                .copied()
                .filter(|v| *v >= max_image_value)
                .count();

            max_value = max_image_value;
            if count > 10 { break; }
        }
        max_value
    }

    pub fn demosaic(&self, demosaic_algo: DemosaicAlgo, mt: bool) -> anyhow::Result<Image> {
        if let Cfa::Pattern(p) = &self.info.cfa {
            match demosaic_algo {
                DemosaicAlgo::Linear =>
                    self.demosaic_bayer_linear(p, mt),
                DemosaicAlgo::ColorRatio =>
                    self.demosaic_bayer_color_ratio(p, mt),
            }
        } else {
            let mut grayscale = Image::new_grey(self.info.width, self.info.height);
            grayscale.l = self.data.clone();
            Ok(grayscale)
        }
    }

    fn demosaic_bayer_linear(&self, p: &CfaPattern, _mt: bool) -> anyhow::Result<Image> {
        if self.data.is_empty() {
            anyhow::bail!("Raw image is empty");
        }

        let width = self.data.width();
        let height = self.data.height();

        let mut result = Image::new_color(self.info.width, self.info.height);

        for y in 1..height-1 {
            let cfa_iter = self.iter_cfa(y);
            let top_iter = self.data.iter_row(y-1);
            let iter = self.data.iter_row(y);
            let bottom_iter = self.data.iter_row(y+1);
            let dst_r_iter = result.r.iter_row_mut(y).skip(1);
            let dst_g_iter = result.g.iter_row_mut(y).skip(1);
            let dst_b_iter = result.b.iter_row_mut(y).skip(1);

            for ((v11, v12, v13),
                 (v21, v22, v23),
                 (v31, v32, v33),
                 (c21, c22),
                 r, g, b) in
            izip!(
                top_iter.tuple_windows(),
                iter.tuple_windows(),
                bottom_iter.tuple_windows(),
                cfa_iter.tuple_windows(),
                dst_r_iter, dst_g_iter, dst_b_iter
            ) {
                match *c22 {
                    CfaColor::R => {
                        *r = *v22;
                        *g = (*v12 + *v21 + *v23 + *v32) * 0.25;
                        *b = (*v11 + *v13 + *v31 + *v33) * 0.25;
                    },
                    CfaColor::G => {
                        *g = *v22;
                        if *c21 == CfaColor::B {
                            *b = (v21 + v23) * 0.5;
                            *r = (v12 + v32) * 0.5;
                        } else {
                            *r = (v21 + v23) * 0.5;
                            *b = (v12 + v32) * 0.5;
                        }
                    },
                    CfaColor::B => {
                        *b = *v22;
                        *g = (*v12 + *v21 + *v23 + *v32) * 0.25;
                        *r = (*v11 + *v13 + *v31 + *v33) * 0.25;
                    },
                    CfaColor::Mono =>
                        unreachable!(),
                }

            }
        }

        let mut demosaic_pt_slow = |x, y, cfa_type| {
            let layer = match cfa_type {
                CfaColor::R => &mut result.r,
                CfaColor::G => &mut result.g,
                CfaColor::B => &mut result.b,
                CfaColor::Mono => unreachable!(),
            };

            let mut sum = 0_f32;
            let mut cnt = 0u16;
            for dy in -1..=1 {
                let sy = y + dy;
                for dx in -1..=1 {
                    let sx = x + dx;
                    if p.get_color_type(sx, sy) == cfa_type {
                        if let Some(v) = self.data.get(sx, sy) {
                            cnt += 1;
                            sum += v;
                        }
                    }
                }
            }

            layer.set(x, y, sum / cnt as f32);
        };

        for x in 1..width-1 {
            demosaic_pt_slow(x, 0, CfaColor::R);
            demosaic_pt_slow(x, 0, CfaColor::G);
            demosaic_pt_slow(x, 0, CfaColor::B);
            demosaic_pt_slow(x, height-1, CfaColor::R);
            demosaic_pt_slow(x, height-1, CfaColor::G);
            demosaic_pt_slow(x, height-1, CfaColor::B);
        }

        for y in 1..height-1 {
            demosaic_pt_slow(0, y, CfaColor::R);
            demosaic_pt_slow(0, y, CfaColor::G);
            demosaic_pt_slow(0, y, CfaColor::B);
            demosaic_pt_slow(width-1, y, CfaColor::R);
            demosaic_pt_slow(width-1, y, CfaColor::G);
            demosaic_pt_slow(width-1, y, CfaColor::B);
        }

        Ok(result)
    }

    fn demosaic_bayer_color_ratio(&self, p: &CfaPattern, _mt: bool) -> anyhow::Result<Image> {
        if self.data.is_empty() {
            anyhow::bail!("Raw image is empty");
        }

        const MAX_DIFF_WITH_LIN: f32 = 12.0;

        // simple green by linear interpolation

        let mut result_image = Image::new_color(self.info.width, self.info.height);

        let demosaic_green = |y, d_row: &mut[f32]| {
            let s_row = self.data.row(y);
            for (x, (d, s)) in d_row.iter_mut().zip(s_row).enumerate() {
                let x = x as Crd;
                let raw_ct = p.get_color_type(x, y);

                *d = if raw_ct == CfaColor::G {
                    *s
                } else {
                    let g1 = self.data.get(x, y-1);
                    let g2 = self.data.get(x, y+1);
                    let g3 = self.data.get(x-1, y);
                    let g4 = self.data.get(x+1, y);

                    let mut sum = 0_f32;
                    let mut cnt = 0_u16;
                    for v in [g1, g2, g3, g4] {
                        if let Some(v) = v {
                            sum += v;
                            cnt += 1;
                        }
                    }
                    if cnt != 0 { sum / cnt as f32 } else { 0.0 }
                };
            }
        };

        for y in 0..self.data.height() {
            demosaic_green(y, result_image.g.row_mut(y));
        }

        // red and blue on green

        let demosaic_red_or_blue_on_green = |y, d_row: &mut[f32], ct: CfaColor| {
            const PATH_VERT:  &[(Crd, Crd)] = &[(0, -1), (0, 1)];
            const PATH_HORIZ: &[(Crd, Crd)] = &[(-1, 0), (1, 0)];
            const PATH_DIAG:  &[(Crd, Crd)] = &[(-1, -1), (1, -1), (-1, 1), (1, 1)];

            let s_row = self.data.row(y);
            let green_row = result_image.g.row(y);
            for (x, (d, s, &green)) in izip!(d_row.iter_mut(), s_row, green_row).enumerate() {
                let x = x as Crd;
                let raw_ct = p.get_color_type(x, y);

                *d = if raw_ct == ct {
                    *s
                } else {
                    let path = match (raw_ct, ct) {
                        (CfaColor::R, CfaColor::B) =>
                            PATH_DIAG,
                        (CfaColor::B, CfaColor::R) =>
                            PATH_DIAG,
                        (CfaColor::G, CfaColor::R) if p.get_color_type(x+1, y) == CfaColor::R =>
                            PATH_HORIZ,
                        (CfaColor::G, CfaColor::R) =>
                            PATH_VERT,
                        (CfaColor::G, CfaColor::B) if p.get_color_type(x+1, y) == CfaColor::B =>
                            PATH_HORIZ,
                        (CfaColor::G, CfaColor::B) =>
                            PATH_VERT,
                        (_, _) =>
                            panic!("Internal error"),
                    };

                    let mut green_sum = 0_f32;
                    let mut color_sum = 0_f32;
                    let mut cnt = 0_u16;
                    for crd in path {
                        let (sx, sy) = (x+crd.0, y+crd.1);
                        if let (Some(v), Some(green)) = (self.data.get(sx, sy), result_image.g.get(sx, sy)) {
                            debug_assert!(p.get_color_type(x+crd.0, y+crd.1) == ct);
                            green_sum += green;
                            color_sum += v;
                            cnt += 1;
                        }
                    }

                    let lin_value = color_sum / cnt as f32;
                    if cnt >= 2 && 2.0 * green_sum > color_sum && green_sum > 0.0 {
                        let color_ratio_value = green * color_sum / green_sum;
                        let diff = if color_ratio_value == 0.0 {
                            0.0
                        } else {
                            lin_value / color_ratio_value
                        };
                        if diff > MAX_DIFF_WITH_LIN || diff < 1.0/MAX_DIFF_WITH_LIN {
                            lin_value
                        } else {
                            color_ratio_value
                        }
                    } else {
                        lin_value
                    }
                };
            }
        };

        for y in 0..self.data.height() {
            demosaic_red_or_blue_on_green(y, result_image.r.row_mut(y), CfaColor::R);
            demosaic_red_or_blue_on_green(y, result_image.b.row_mut(y), CfaColor::B);
        }

        // green on red and blue

        let demosaic_green_on_red_or_blue = |y, d_row: &mut[f32]| {
            let s_row = self.data.row(y);
            for (x, (d, red_or_blue)) in d_row.iter_mut().zip(s_row).enumerate() {
                let x = x as Crd;
                let raw_ct = p.get_color_type(x, y);

                if raw_ct == CfaColor::G {
                    continue;
                }

                let hint = if raw_ct == CfaColor::R {
                    &result_image.r
                } else {
                    &result_image.b
                };

                let (g1, h1) = (self.data.get(x, y-1), hint.get(x, y-1));
                let (g2, h2) = (self.data.get(x, y+1), hint.get(x, y+1));
                let (g3, h3) = (self.data.get(x-1, y), hint.get(x-1, y));
                let (g4, h4) = (self.data.get(x+1, y), hint.get(x+1, y));

                let mut red_or_blue_sum = 0_f32;
                let mut green_sum = 0_f32;
                let mut cnt = 0_u16;
                for (g, h) in [(g1, h1), (g2, h2), (g3, h3), (g4, h4)] {
                    if let (Some(g), Some(h)) = (g, h) {
                        if h != 0.0 {
                            red_or_blue_sum += h;
                            green_sum += g;
                            cnt += 1;
                        }
                    }
                }

                if cnt >= 2 && 2.0 * red_or_blue_sum > green_sum && red_or_blue_sum > 0.0 {
                    let color_ratio_value = red_or_blue * green_sum / red_or_blue_sum;
                    let diff = if color_ratio_value != 0.0 {
                        *d / color_ratio_value
                    } else {
                        0.0
                    };
                    if diff < MAX_DIFF_WITH_LIN && diff > 1.0/MAX_DIFF_WITH_LIN {
                        *d = color_ratio_value;
                    }
                }
            }
        };

        for y in 0..self.data.height() {
            demosaic_green_on_red_or_blue(y, result_image.g.row_mut(y));
        }

        Ok(result_image)
    }

    pub fn find_hot_pixels_in_dark_file(&self) -> HashSet<BadPixel> {
        const PART: usize = 1000;
        const K: f32 = 10.0;

        let mut result = HashSet::new();
        let mut coords = Vec::new();

        let store_diff_values = |
            it:          IterType,
            main_crd:    Crd,
            coords:      &[Crd],
            diff_values: &mut Vec<f32>,
            values:      &mut Vec<f32>
        | {
            values.clear();
            match it {
                IterType::Cols =>
                    for v in self.data.iter_col(main_crd) { values.push(*v); }
                IterType::Rows =>
                    for v in self.data.iter_row(main_crd) { values.push(*v); }
                _ =>
                    unreachable!(),
            }

            Self::find_hot_pixels_in_line(&values, &coords, |_values_crd, aver, value| {
                let diff = value-aver.abs();
                diff_values.push(diff);
            });
        };

        let mut find_hot_pixels = |
            it:       IterType,
            main_crd: Crd,
            coords:   &[Crd],
            values:   &mut Vec<f32>,
            border:   f32
        | {
            values.clear();
            match it {
                IterType::Cols =>
                    for v in self.data.iter_col(main_crd) { values.push(*v); }
                IterType::Rows =>
                    for v in self.data.iter_row(main_crd) { values.push(*v); }
                _ =>
                    unreachable!(),
            }

            Self::find_hot_pixels_in_line(&values, &coords, |values_crd, aver, value| {
                let diff = (value-aver).abs();
                if diff > border { match it {
                    IterType::Cols =>
                        result.insert(BadPixel { x: main_crd, y: values_crd }),
                    IterType::Rows =>
                        result.insert(BadPixel { x: values_crd, y: main_crd }),
                    _ =>
                        unreachable!(),
                };}
            });
        };

        let mut values = Vec::new();
        let mut diff_values = Vec::new();

        coords.clear();
        diff_values.clear();
        for x in 0..self.data.width() { coords.push(x); }
        for y in 0..self.data.height() {
            store_diff_values(IterType::Rows, y, &coords, &mut diff_values, &mut values);
        }
        let pos = diff_values.len() - diff_values.len()/PART;
        let border = K * *diff_values.select_nth_unstable_by(pos, cmp_f32).1;
        for y in 0..self.data.height() {
            find_hot_pixels(IterType::Rows, y, &coords, &mut values, border);
        }

        coords.clear();
        diff_values.clear();
        for y in 0..self.data.height() { coords.push(y); }
        for x in 0..self.data.width() {
            store_diff_values(IterType::Cols, x, &coords, &mut diff_values, &mut values);
        }
        let pos = diff_values.len() - diff_values.len()/PART;
        let border = K * *diff_values.select_nth_unstable_by(pos, cmp_f32).1;
        for x in 0..self.data.width() {
            find_hot_pixels(IterType::Cols, x, &coords, &mut values, border);
        }

        result
    }

    pub fn find_hot_pixels_in_light_file(&self, percentile: usize, k: f32) -> HashSet<BadPixel> {
        let mut result = HashMap::new();

        let fill_diff_values = |
            it:       IterType,
            main_crd: Crd,
            cc:       CfaColor,
            coords:   &mut Vec<Crd>,
            values:   &mut Vec<f32>,
            diff_pos: &mut Vec<f32>,
            diff_neg: &mut Vec<f32>,
        | {
            coords.clear();
            values.clear();
            match it {
                IterType::Cols =>
                    for (crd, v) in self.iter_col_color(main_crd, cc) {
                        values.push(v);
                        coords.push(crd);
                    }
                IterType::Rows =>
                    for (crd, v) in self.iter_row_color(main_crd, cc) {
                        values.push(v);
                        coords.push(crd);
                    }
                _ =>
                    unreachable!(),
            }

            Self::find_hot_pixels_in_line(&values, &coords, |_values_crd, aver, value| {
                let diff = value-aver;
                if diff >= 0.0 {
                    diff_pos.push(diff);
                } else {
                    diff_neg.push(-diff);
                }
            });
        };

        let mut fill_hot_pixels = |
            it:         IterType,
            main_crd:   Crd,
            cc:         CfaColor,
            coords:     &mut Vec<Crd>,
            values:     &mut Vec<f32>,
            border_pos: f32,
            border_neg: f32,
        | {
            coords.clear();
            values.clear();
            match it {
                IterType::Cols =>
                    for (crd, v) in self.iter_col_color(main_crd, cc) {
                        values.push(v);
                        coords.push(crd);
                    }
                IterType::Rows =>
                    for (crd, v) in self.iter_row_color(main_crd, cc) {
                        values.push(v);
                        coords.push(crd);
                    }
                _ =>
                    unreachable!(),
            }

            Self::find_hot_pixels_in_line(&values, &coords, |values_crd, aver, value| {
                let diff = value-aver;
                if ((diff > 0.0) && (diff > border_pos)) || ((diff < 0.0) && (diff < border_neg)) { match it {
                    IterType::Cols =>
                        *result.entry(BadPixel { x: main_crd, y: values_crd }).or_insert(0) += 1,
                    IterType::Rows =>
                        *result.entry(BadPixel { x: values_crd, y: main_crd }).or_insert(0) += 1,
                    _ =>
                        unreachable!(),
                };}
            });
        };

        let mut diff_values_pos = Vec::new();
        let mut diff_values_neg = Vec::new();
        let mut coords = Vec::new();
        let mut values = Vec::new();

        let mut process_color = |cc| {
            diff_values_pos.clear();
            diff_values_neg.clear();

            for y in 0..self.data.height() {
                fill_diff_values(IterType::Rows, y, cc, &mut coords, &mut values, &mut diff_values_pos, &mut diff_values_neg);
            }

            for x in 0..self.data.width() {
                fill_diff_values(IterType::Cols, x, cc, &mut coords, &mut values, &mut diff_values_pos, &mut diff_values_neg);
            }

            if diff_values_pos.is_empty() || diff_values_neg.is_empty() {
                return;
            }

            let border_pos = {
                let pos = percentile * diff_values_pos.len() / 100;
                k * *diff_values_pos.select_nth_unstable_by(pos, cmp_f32).1
            };

            let border_neg = {
                let pos = percentile * diff_values_neg.len() / 100;
                -k * *diff_values_neg.select_nth_unstable_by(pos, cmp_f32).1
            };

            for y in 0..self.data.height() {
                fill_hot_pixels(IterType::Rows, y, cc, &mut coords, &mut values, border_pos, border_neg);
            }

            for x in 0..self.data.width() {
                fill_hot_pixels(IterType::Cols, x, cc, &mut coords, &mut values, border_pos, border_neg);
            }
        };

        process_color(CfaColor::R);
        process_color(CfaColor::G);
        process_color(CfaColor::B);
        process_color(CfaColor::Mono);

        result.into_iter()
            .filter(|(_, cnt)| *cnt >= 2)
            .map(|(crd, _)| crd)
            .collect()
    }

    fn find_hot_pixels_in_line(
        data: &[f32],
        coords: &[Crd],
        mut fun: impl FnMut(Crd, f32, f32)
    ) {
        debug_assert!(data.len() == coords.len());
        if data.len() < 5 { return; }

        let mut test = |center_value: f32, v1: f32, v2: f32, v3: f32, v4: f32, values_crd| {
            if center_value.is_infinite() { return; }
            let aver = (v1 + v2 + v3 + v4) / 4.0;
            if aver.is_infinite() { return; }
            fun(values_crd, aver, center_value);
        };

        test(data[0], data[1], data[2], data[3], data[4], coords[0]);
        test(data[1], data[0], data[2], data[3], data[4], coords[1]);
        test(data[2], data[0], data[1], data[3], data[4], coords[2]);

        for ((v1, _), (v2, _), (v, crd), (v3, _), (v4, _))
        in data.iter().zip(coords).tuple_windows() {
            test(*v, *v1, *v2, *v3, *v4, *crd);
        }

        let end = &data[data.len()-5..];
        test(end[2], end[0], end[1], end[2], end[3], coords[data.len()-3]);
        test(end[3], end[0], end[1], end[2], end[3], coords[data.len()-2]);
        test(end[4], end[0], end[1], end[2], end[3], coords[data.len()-1]);
    }

    pub fn remove_bad_pixels(&mut self, hot_pixels: &HashSet<BadPixel>) {
        if hot_pixels.is_empty() { return; }
        let mut pairs = Vec::new();
        let mut hot_pixels_index: HashSet<BadPixel> = HashSet::from_iter(hot_pixels.iter().cloned());
        for hp in hot_pixels {
            let color = self.info.cfa.get_pixel_color(hp.x, hp.y);
            for r in 1..=2 {
                let add_pair = |pairs: &mut Vec<_>, dx, dy| {
                    let x1 = hp.x + dx;
                    let y1 = hp.y + dy;
                    let x2 = hp.x - dx;
                    let y2 = hp.y - dy;
                    if color == self.info.cfa.get_pixel_color(x1, y1)
                    && color == self.info.cfa.get_pixel_color(x2, y2) {
                        if let (Some(v1), Some(v2)) = (self.data.get(x1, y1), self.data.get(x2, y2)) {
                            pairs.push((v1, v2));
                        }
                    }
                };

                pairs.clear();
                add_pair(&mut pairs, -r, r);
                add_pair(&mut pairs, 0, r);
                add_pair(&mut pairs, r, r);
                add_pair(&mut pairs, r, 0);

                let min_pair = pairs.iter().min_by_key(|p| (1000.0 * (p.0 - p.1).abs()) as i64);
                if let Some(min_pair) = min_pair {
                    self.data.set(hp.x, hp.y, (min_pair.0 + min_pair.1) / 2.0);
                    hot_pixels_index.remove(hp);
                    break;
                }
            }
        }
    }

    pub fn filter_flat_image(self) -> RawImage {
        const R: Crd = 2;
        let mut result = RawImage::new_from_info(self.info);
        for (x, y, v) in result.data.iter_crd_mut() {
            let color = result.info.cfa.get_pixel_color(x, y);
            let mut sum = 0_f32;
            let mut cnt = 0;
            for (sx, sy, sv) in self.data.iter_rect_crd(x-R, y-R, x+R, y+R) {
                if result.info.cfa.get_pixel_color(sx, sy) != color { continue; }
                sum += sv;
                cnt += 1;
            }
            if cnt != 0 { *v = sum / cnt as f32 }
            else { *v = self.data.get(x, y).unwrap(); }
        }
        result
    }

    pub fn iter_row_color(&self, y: Crd, cc: CfaColor) -> RowColorIterator {
        let pos = (y*self.data.width()) as usize;
        let data = self.data.as_slice();
        RowColorIterator {
            iter: data[pos..pos + self.data.width() as usize].iter(),
            x: 0, y, cc,
            cfa: self.info.cfa,
        }
    }

    pub fn iter_col_color(&self, x: Crd, cc: CfaColor) -> ColColorIterator {
        let data = self.data.as_slice();
        ColColorIterator {
            iter: data[x as usize..].iter().step_by(self.data.width() as usize),
            x, y: 0, cc,
            cfa: self.info.cfa,
        }
    }

    pub fn iter_cfa(&self, y: Crd) -> std::iter::Cycle<std::slice::Iter<CfaColor>> {
        match self.info.cfa {
            Cfa::Pattern(ref pattern) =>
                pattern.get_row(y).iter().cycle(),
            Cfa::Mono =>
                panic!("Not for monochrome data!"),
        }
    }

}

pub struct CalibrationData {
    pub dark_image: Option<RawImage>,
    pub flat_image: Option<RawImage>,
    pub bias_image: Option<RawImage>,
    pub hot_pixels: HashSet<BadPixel>,
}

impl CalibrationData {
    pub fn new_empty() -> CalibrationData {
        CalibrationData {
            dark_image: None,
            flat_image: None,
            bias_image: None,
            hot_pixels: HashSet::new(),
        }
    }

    pub fn load(
        master_flat: Option<&Path>,
        master_dark: Option<&Path>,
        master_bias: Option<&Path>,
    ) -> anyhow::Result<CalibrationData> {
        let bias_image = match master_bias {
            Some(file_name) => {
                log::info!(
                    "loading master bias '{}'...",
                    fs_utils::path_to_str(file_name)
                );
                Some(load_master_format_file(file_name)?)
            },
            None => None,
        };

        let (dark_image, hot_pixels) = match master_dark {
            Some(file_name) => {
                log::info!(
                    "loading master dark '{}'...",
                    fs_utils::path_to_str(file_name)
                );
                let mut image = load_master_format_file(file_name)?;
                if let Some(bias_image) = &bias_image {
                    image.data -= &bias_image.data;
                }
                let hot_pixels = image.find_hot_pixels_in_dark_file();
                log::info!("hot pixels count = {}", hot_pixels.len());
                (Some(image), hot_pixels)
            },
            None => (None, HashSet::new()),
        };

        let flat_image = match master_flat {
            Some(file_name) => {
                log::info!(
                    "loading master flat '{}'...",
                    fs_utils::path_to_str(file_name)
                );
                let mut image = load_master_format_file(file_name)?;
                image.remove_bad_pixels(&hot_pixels);
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
            bias_image,
            hot_pixels
        })
    }

    fn is_usable_for_raw(
        info:        &RawImageInfo,
        cal_info:    &RawImageInfo,
        mode:        &str,
        master_dark: bool
    ) -> anyhow::Result<()> {
        let compare = |
            item,
            raw: &dyn std::fmt::Display,
            cal: &dyn std::fmt::Display
        | -> anyhow::Result<()> {
            if raw.to_string() != cal.to_string() {
                anyhow::bail!(
                    "{} differs for {}: ('{}' != '{}')",
                    item, mode, raw, cal
                );
            }
            Ok(())
        };

        compare("Width", &info.width, &cal_info.width)?;
        compare("Height", &info.height, &cal_info.height)?;
        let raw_cam = info.camera.as_ref().map(String::as_str).unwrap_or("");
        let cal_cam = cal_info.camera.as_ref().map(String::as_str).unwrap_or("");
        if cal_cam != "" {
            compare("Camera model", &raw_cam, &cal_cam)?;
        }

        compare("Color pattern", &info.cfa, &cal_info.cfa)?;
        if master_dark {
            compare("ISO", &info.iso.unwrap_or(0), &cal_info.iso.unwrap_or(0))?;
        }

        Ok(())
    }


    pub fn is_empty(&self) -> bool {
        self.dark_image.is_none() &&
        self.flat_image.is_none() &&
        self.bias_image.is_none()
    }
}

pub struct RowColorIterator<'a> {
    iter: std::slice::Iter<'a, f32>,
    x: Crd,
    y: Crd,
    cc: CfaColor,
    cfa: Cfa,
}

impl<'a> Iterator for RowColorIterator<'a> {
    type Item = (Crd, f32);

    fn next(&mut self) -> Option<Self::Item> {
        let value = loop {
            let iter_v = self.iter.next();
            let cur_cc = self.cfa.get_pixel_color(self.x, self.y);
            match iter_v {
                Some(v) if cur_cc == self.cc => break v,
                None => return None,
                _ => { self.x += 1; continue; },
            }
        };
        let result = (self.x, *value);
        self.x += 1;
        Some(result)
    }
}

pub struct ColColorIterator<'a> {
    iter: std::iter::StepBy<std::slice::Iter<'a, f32>>,
    x: Crd,
    y: Crd,
    cc: CfaColor,
    cfa: Cfa,
}

impl<'a> Iterator for ColColorIterator<'a> {
    type Item = (Crd, f32);

    fn next(&mut self) -> Option<Self::Item> {
        let value = loop {
            let iter_v = self.iter.next();
            let cur_cc = self.cfa.get_pixel_color(self.x, self.y);
            match iter_v {
                Some(v) if cur_cc == self.cc => break v,
                None => return None,
                _ => { self.y += 1; continue; },
            }
        };
        let result = (self.y, *value);
        self.y += 1;
        Some(result)
    }
}
