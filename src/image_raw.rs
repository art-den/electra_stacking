use std::{path::*, io::*, fs::*, collections::HashSet, hash::Hash};
use itertools::{izip};
use rayon::prelude::*;
use serde::{Serialize, Deserialize};
use byteorder::*;
use bitflags::bitflags;
use crate::{image::*, fs_utils, log_utils::*, calc::*, compression::*,};
use bitstream_io::{BitWriter, BitWrite};

const CALIBR_FILE_SIG: &[u8] = b"calibr-file-2";
const MASTER_FILE_SIG: &[u8] = b"master-file-2";

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

type CfaArr = [[CfaColor; 2]; 2];

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Debug)]
pub struct CfaPattern {
    pub pattern_type: CfaType,
    start_left: Crd,
    start_top: Crd,
    arr: CfaArr,
}

impl CfaPattern {
    #[inline(always)]
    fn get_color_type(&self, x: Crd, y: Crd) -> CfaColor {
        self.arr[((y+self.start_top) & 1) as usize][((x+self.start_left) & 1) as usize]
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Debug)]
pub enum Cfa {
    Mono,
    Pattern(CfaPattern),
}

impl Cfa {
    pub fn from_str(cfa_str: &str, start_left: Crd, start_top: Crd) -> Cfa {
        use CfaColor::*;

        match cfa_str {
            "GBRG" => Cfa::Pattern(CfaPattern {
                start_left,
                start_top,
                pattern_type: CfaType::GBRG,
                arr: [[G, B], [R, G]]
            }),
            "RGGB" => Cfa::Pattern(CfaPattern {
                start_left,
                start_top,
                pattern_type: CfaType::RGGB,
                arr: [[R, G], [G, B]]
            }),
            "BGGR" => Cfa::Pattern(CfaPattern {
                start_left,
                start_top,
                pattern_type: CfaType::BGGR,
                arr: [[B, G], [G, R]]
            }),
            "GRBG" => Cfa::Pattern(CfaPattern {
                start_left,
                start_top,
                pattern_type: CfaType::GRBG,
                arr: [[G, R], [B, G]]
            }),
            _  => Cfa::Mono
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

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RawImageInfo {
    pub width: Crd,
    pub height: Crd,
    pub max_values: [f32; 4],
    pub black_values: [f32; 4],
    pub wb: [f32; 4],
    pub cfa: Cfa,
    pub exif: Exif,
}

impl RawImageInfo {
    pub fn read_from<R: Read>(src: &mut R) -> anyhow::Result<RawImageInfo> {
        let sig_len = leb128::read::unsigned(src)? as usize;
        if sig_len > 42 { anyhow::bail!("Wrong signature lentgh"); }
        let mut sig = Vec::<u8>::new();
        sig.resize(sig_len, 0);
        src.read_exact(&mut sig)?;
        if sig != CALIBR_FILE_SIG { anyhow::bail!("Wrong file format"); }
        let header_len = leb128::read::unsigned(src)? as usize;
        let mut buf = Vec::<u8>::new();
        buf.resize(header_len, 0);
        src.read_exact(&mut buf)?;
        let header_str = std::str::from_utf8(buf.as_slice())?;
        Ok(serde_json::from_str(header_str)?)
    }

    pub fn write_to<W: Write>(&self, dst: &mut W) -> anyhow::Result<()> {
        leb128::write::unsigned(dst, CALIBR_FILE_SIG.len() as u64)?;
        dst.write_all(CALIBR_FILE_SIG)?;
        let header = serde_json::to_string(&self).unwrap();
        leb128::write::unsigned(dst, header.len() as u64)?;
        dst.write_all(header.as_bytes())?;
        Ok(())
    }

    pub fn check_is_compatible(&self, other: &RawImageInfo) -> bool {
        self.width == other.width &&
        self.height == other.height &&
        self.cfa == other.cfa
    }
}

#[derive(Serialize, Deserialize, PartialEq)]
pub struct MasterFileInfo {
    pub files:     Vec<PathBuf>,
    pub calc_opts: CalcOpts,
}

impl MasterFileInfo {
    pub fn read_fro(file_name: &Path) -> anyhow::Result<MasterFileInfo> {
        let mut file = std::io::BufReader::new(std::fs::File::open(file_name)?);
        let sig_len = leb128::read::unsigned(&mut file)? as usize;
        if sig_len > 42 { anyhow::bail!("Wrong signature lentgh"); }
        let mut sig = Vec::<u8>::new();
        sig.resize(sig_len, 0);
        file.read_exact(&mut sig)?;
        if sig != MASTER_FILE_SIG { anyhow::bail!("Wrong file format"); }
        let header_len = leb128::read::unsigned(&mut file)? as usize;
        let mut buf = Vec::<u8>::new();
        buf.resize(header_len, 0);
        file.read_exact(&mut buf)?;
        let header_str = std::str::from_utf8(buf.as_slice())?;
        Ok(serde_json::from_str(header_str)?)
    }

    pub fn write_to<W: Write>(&self, dst: &mut W) -> anyhow::Result<()> {
        leb128::write::unsigned(dst, MASTER_FILE_SIG.len() as u64)?;
        dst.write_all(MASTER_FILE_SIG)?;
        let header = serde_json::to_string(&self).unwrap();
        leb128::write::unsigned(dst, header.len() as u64)?;
        dst.write_all(header.as_bytes())?;
        Ok(())
    }
}

pub struct RawImage {
    pub info: RawImageInfo,
    pub data: ImageLayerF32,
    pub overexposured: Vec<(Crd, Crd)>,
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct BadPixel {
    pub x: Crd,
    pub y: Crd,
}

bitflags! { pub struct RawLoadFlags: u32 {
    const EXTRACT_BLACK = 1;
    const STORE_OVEREXPOSURES  = 2;
}}

impl RawImage {
    pub fn new_from_info(info: RawImageInfo) -> RawImage {
        let width = info.width;
        let height = info.height;
        RawImage {
            info,
            data: ImageLayer::new(width, height),
            overexposured: Vec::new(),
        }
    }

    pub fn load_camera_raw_file(
        file_name:  &Path,
        flags:      RawLoadFlags,
    ) -> anyhow::Result<RawImage> {
        let raw = rawloader::decode_file(file_name)?;

        let crop_left = raw.crops[3] as Crd;
        let crop_top = raw.crops[0] as Crd;
        let width = raw.width as Crd - raw.crops[1] as Crd - crop_left;
        let height = raw.height as Crd - raw.crops[2] as Crd - crop_top;

        let mut exif = Exif::new_empty();
        if let Some(raw_exif) = raw.exif {
            exif.exp_time = raw_exif.get_rational(rawloader::Tag::ExposureTime);
            exif.fnumber = raw_exif.get_rational(rawloader::Tag::FNumber);
            exif.iso = raw_exif.get_uint(rawloader::Tag::ISOSpeed);
            exif.focal_len = raw_exif.get_rational(rawloader::Tag::FocalLength);
            exif.camera = raw_exif.get_str(rawloader::Tag::Model).map(|v| v.to_string());
        }

        let mut info = RawImageInfo {
            width, height,
            max_values:   [0.0; 4],
            black_values: [0.0; 4],
            wb:           [0.0; 4],
            cfa:          Cfa::from_str(&raw.cfa.name[..], crop_left, crop_top),
            exif,
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

        let extract_black = flags.contains(RawLoadFlags::EXTRACT_BLACK);

        let mut store_overexposures = flags.contains(RawLoadFlags::STORE_OVEREXPOSURES);
        let mut max_values = [0_f32; 4];
        if store_overexposures {
            let clip_value = Self::find_clip_value(data.as_slice());
            for i in 0..4 {
                let white_value = raw.whitelevels[i] as f32;
                max_values[i] = if clip_value < white_value && white_value < 1.2 * clip_value {
                    clip_value
                } else {
                    white_value
                };
                if max_values[i] < raw.blacklevels[i] as f32 {
                    store_overexposures = false;
                }
            }
        }

        let mut overexposured = Vec::new();

        if store_overexposures || extract_black {
            let mut correct_values = |color, black_level, max| {

                for (x, y, v) in data.iter_crd_mut() {
                    if info.cfa.get_pixel_color(x, y) != color {
                        continue;
                    }

                    if store_overexposures && *v > max {
                        overexposured.push((x, y));
                    }

                    if extract_black {
                        *v -= black_level;
                    }
                }
            };

            const MAX_K: f32 = 0.99;

            if let Cfa::Mono = &info.cfa {
                correct_values(CfaColor::Mono, raw.blacklevels[0] as f32, max_values[0] * MAX_K);
            } else {
                correct_values(CfaColor::R, raw.blacklevels[0] as f32, max_values[0] * MAX_K);
                correct_values(CfaColor::G, raw.blacklevels[1] as f32, max_values[1] * MAX_K);
                correct_values(CfaColor::B, raw.blacklevels[2] as f32, max_values[2] * MAX_K);
            }
        }

        for i in 0..4 {
            if extract_black {
                info.black_values[i] = 0.0;
                info.max_values[i] = raw.whitelevels[i] as f32 - raw.blacklevels[i] as f32;
            } else {
                info.black_values[i] = raw.blacklevels[i] as f32;
                info.max_values[i] = raw.whitelevels[i] as f32;
            }
            info.wb[i] = if !raw.wb_coeffs[i].is_nan() { raw.wb_coeffs[i] } else { 0.0 };
        }

        Ok(RawImage{ info, overexposured, data })
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

    pub fn save_to_calibr_format_file(&self, file_name: &Path) -> anyhow::Result<()> {
        let mut file = BufWriter::new(File::create(file_name)?);
        self.info.write_to(&mut file)?;
        let mut writer = BitWriter::endian(&mut file, bitstream_io::BigEndian);
        let mut compr = ValuesCompressor::new();
        for v in self.data.iter() {
            compr.write_f32(*v, &mut writer)?;
        }
        compr.flush(&mut writer)?;
        writer.write(32, 0)?;
        writer.flush()?;
        Ok(())
    }

    pub fn save_to_master_format_file(
        &self,
        file_name: &Path,
        master_info: &MasterFileInfo
    ) -> anyhow::Result<()> {
        let mut file = BufWriter::new(File::create(file_name)?);
        master_info.write_to(&mut file)?;
        self.info.write_to(&mut file)?;
        for v in self.data.iter() { file.write_f32::<BigEndian>(*v)?; }
        Ok(())
    }

    pub fn new_from_master_format_file(file_name: &Path) -> anyhow::Result<RawImage> {
        let mut file = std::io::BufReader::new(std::fs::File::open(file_name)?);

        // skip master file header
        let sig_len = leb128::read::unsigned(&mut file)?;
        file.seek_relative(sig_len as i64)?;
        let header_len = leb128::read::unsigned(&mut file)?;
        file.seek_relative(header_len as i64)?;

        let info = RawImageInfo::read_from(&mut file)?;
        let mut image = ImageLayerF32::new(info.width, info.height);
        for v in image.iter_mut() { *v = file.read_f32::<BigEndian>()?; }
        Ok(RawImage{
            info,
            data: image,
            overexposured: Vec::new(),
        })
    }

    pub fn demosaic_bayer_linear(&self, p: &CfaPattern, mt: bool) -> anyhow::Result<Image> {
        if self.data.is_empty() {
            anyhow::bail!("Raw image is empty");
        }

        let mut result = Image::new_color(self.info.width, self.info.height);
        let max_wb = self.info.wb
            .iter()
            .fold(0_f32, |a, v| if !v.is_nan() { a.max(*v) } else { a } );

        let mut lin_coeffs = [0_f32; 4];
        for i in 0..4 {
            lin_coeffs[i] = if self.info.max_values[i] != 0.0 {
                self.info.wb[i] * 1.0 / (self.info.max_values[i] * max_wb)
            } else {
                self.info.wb[i] / max_wb
            };
        }

        const PATH_VERT:  &[(Crd, Crd)] = &[(0, -1), (0, 1)];
        const PATH_HORIZ: &[(Crd, Crd)] = &[(-1, 0), (1, 0)];
        const PATH_DIAG:  &[(Crd, Crd)] = &[(-1, -1), (1, -1), (-1, 1), (1, 1)];
        const PATH_CROSS: &[(Crd, Crd)] = &[(0, -1), (-1, 0), (1, 0), (0, 1)];

        let demosaic_row = |y, d_row: &mut[f32], ct: CfaColor, k: f32| {
            let s_row = self.data.row(y);
            for (x, (d, s)) in d_row.iter_mut().zip(s_row).enumerate() {
                let x = x as Crd;
                let raw_ct = p.get_color_type(x, y);

                *d = k * if raw_ct == ct {
                    *s
                } else {
                    let path = match (raw_ct, ct) {
                        (_, CfaColor::G) =>
                            PATH_CROSS,
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

                    let mut sum = 0_f32;
                    let mut cnt = 0_u16;
                    for crd in path { if let Some(v) = self.data.get(x+crd.0, y+crd.1) {
                        debug_assert!(p.get_color_type(x+crd.0, y+crd.1) == ct);
                        sum += v;
                        cnt += 1;
                    }}
                    if cnt != 0 { sum / cnt as f32 } else { 0.0 }
                };
            }
        };

        if !mt {
            for y in 0..self.data.height() {
                demosaic_row(y, result.r.row_mut(y), CfaColor::R, lin_coeffs[0]);
                demosaic_row(y, result.g.row_mut(y), CfaColor::G, lin_coeffs[1]);
                demosaic_row(y, result.b.row_mut(y), CfaColor::B, lin_coeffs[2]);
            }
        } else {
            let width = self.data.width() as usize;

            result.r.as_slice_mut().par_chunks_mut(width)
                .enumerate()
                .for_each(|(y, d)| {
                    demosaic_row(y as Crd, d, CfaColor::R, lin_coeffs[0]);
                });

            result.g.as_slice_mut().par_chunks_mut(width)
                .enumerate()
                .for_each(|(y, d)| {
                    demosaic_row(y as Crd, d, CfaColor::G, lin_coeffs[1]);
                });

            result.b.as_slice_mut().par_chunks_mut(width)
                .enumerate()
                .for_each(|(y, d)| {
                    demosaic_row(y as Crd, d, CfaColor::B, lin_coeffs[2]);
                });
        }

        Ok(result)
    }

    pub fn demosaic_bayer_color_ratio(&self, p: &CfaPattern, _mt: bool) -> anyhow::Result<Image> {
        if self.data.is_empty() {
            anyhow::bail!("Raw image is empty");
        }

        // simple green by linear interpolation

        let mut result_image = Image::new_color(self.info.width, self.info.height);

        let demosaic_green_row = |y, d_row: &mut[f32]| {
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
            demosaic_green_row(y, result_image.g.row_mut(y));
        }

        // red and blue on green

        const PATH_VERT:  &[(Crd, Crd)] = &[(0, -1), (0, 1)];
        const PATH_HORIZ: &[(Crd, Crd)] = &[(-1, 0), (1, 0)];
        const PATH_DIAG:  &[(Crd, Crd)] = &[(-1, -1), (1, -1), (-1, 1), (1, 1)];

        let demosaic_red_blue_row = |y, d_row: &mut[f32], ct: CfaColor| {
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
                    let mut green_cnt = 0_u16;
                    let mut color_ratio_sum = 0_f32;
                    let mut color_ratio_cnt = 0_u16;
                    for crd in path {
                        let (sx, sy) = (x+crd.0, y+crd.1);
                        if let (Some(v), Some(green)) = (self.data.get(sx, sy), result_image.g.get(sx, sy)) {
                            debug_assert!(p.get_color_type(x+crd.0, y+crd.1) == ct);

                            green_sum += v;
                            green_cnt += 1;
                            if 4.0 * green > v {
                                let color_ratio = v / green;
                                color_ratio_sum += color_ratio;
                                color_ratio_cnt += 1;
                            }
                        }
                    }

                    if color_ratio_cnt >= 2 {
                        let color_ratio_aver = color_ratio_sum/color_ratio_cnt as f32;
                        green * color_ratio_aver
                    } else if green_cnt != 0 {
                        green_sum / green_cnt as f32
                    } else {
                        0.0
                    }
                };
            }
        };

        for y in 0..self.data.height() {
            demosaic_red_blue_row(y, result_image.r.row_mut(y), CfaColor::R);
            demosaic_red_blue_row(y, result_image.b.row_mut(y), CfaColor::B);
        }

        // green on red and blue

        let demosaic_green_row_on_red_and_blue = |y, d_row: &mut[f32]| {
            let s_row = self.data.row(y);
            for (x, (d, s)) in d_row.iter_mut().zip(s_row).enumerate() {
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

                let mut color_ratio_sum = 0_f32;
                let mut color_ratio_cnt = 0_u16;
                for (g, h) in [(g1, h1), (g2, h2), (g3, h3), (g4, h4)] {
                    if let (Some(g), Some(h)) = (g, h) {
                        if 4.0 * h > g {
                            let color_ratio = g / h;
                            color_ratio_sum += color_ratio;
                            color_ratio_cnt += 1;
                        }
                    }
                }

                if color_ratio_cnt >= 2 {
                    let color_ratio = color_ratio_sum / color_ratio_cnt as f32;
                    let hint_value = s;
                    *d = hint_value * color_ratio;
                }
            }
        };

        for y in 0..self.data.height() {
            demosaic_green_row_on_red_and_blue(y, result_image.g.row_mut(y));
        }

        // apply WB coefficients

        let max_wb = self.info.wb
            .iter()
            .fold(0_f32, |a, v| if !v.is_nan() { a.max(*v) } else { a } );

        let mut lin_coeffs = [0_f32; 4];
        for i in 0..4 {
            lin_coeffs[i] = if self.info.max_values[i] != 0.0 {
                self.info.wb[i] * 1.0 / (self.info.max_values[i] * max_wb)
            } else {
                self.info.wb[i] / max_wb
            };
        }

        let correct_color = |layer: &mut ImageLayerF32, k: f32| {
            for v in layer.iter_mut() {
                *v *= k;
            }
        };

        correct_color(&mut result_image.r, lin_coeffs[0]);
        correct_color(&mut result_image.g, lin_coeffs[1]);
        correct_color(&mut result_image.b, lin_coeffs[2]);

        Ok(result_image)
    }

    pub fn find_hot_pixels_in_dark_file(&self) -> HashSet<BadPixel> {
        const R: Crd = 2;
        let mut deviations = Vec::new();
        let mut values_for_median = Vec::new();
        for (x, y, v) in self.data.iter_crd() {
            values_for_median.clear();
            for (_, _, sv) in self.data.iter_rect_crd(x-R, y-R, x+R, y+R) {
                values_for_median.push(sv);
            }
            let median = median_f32(&mut values_for_median).unwrap_or(0.0);
            deviations.push((median - v) * (median - v));
        }

        let last_10_pos = deviations.len() - 10;
        let last_10_dev = *deviations.select_nth_unstable_by(last_10_pos, cmp_f32).1;

        let high_99_pos = 99 * deviations.len() / 100;
        let high_99_dev = *deviations.select_nth_unstable_by(high_99_pos, cmp_f32).1;

        let mut max_dev = f32::min(high_99_dev * 100.0, last_10_dev / 100.0);

        let mut result = HashSet::new();
        let max_result_size = deviations.len() / 100;
        loop {
            result.clear();
            for (x, y, v) in self.data.iter_crd() {
                values_for_median.clear();
                for (_, _, sv) in self.data.iter_rect_crd(x-R, y-R, x+R, y+R) {
                    values_for_median.push(sv);
                }
                let median = median_f32(&mut values_for_median).unwrap_or(0.0);
                let deviation = (median - v) * (median - v);
                if deviation > max_dev {
                    result.insert(BadPixel {x, y});
                    if result.len() > max_result_size { break; }
                }
            }
            log::info!("possible hot pixels count = {}", result.len());
            if result.len() < max_result_size { break; }
            max_dev *= 2.0;
        }

        result
    }

    pub fn remove_bad_pixels(&mut self, hot_pixels: &HashSet<BadPixel>) {
        if hot_pixels.is_empty() { return; }
        let mut hot_pixels_index: HashSet<BadPixel> = HashSet::from_iter(hot_pixels.iter().cloned());
        for hp in hot_pixels {
            for r in 1..=2 {
                let mut sum = 0_f32;
                let mut cnt = 0_u32;
                let color = self.info.cfa.get_pixel_color(hp.x, hp.y);
                for x in hp.x-r ..= hp.x+r { for y in hp.y-r ..= hp.y+r {
                    if color != self.info.cfa.get_pixel_color(x, y) { continue; }
                    if hot_pixels_index.contains(&BadPixel{x, y}) { continue; }
                    if let Some(v) = self.data.get(x, y) { if !v.is_infinite() {
                        sum += v;
                        cnt += 1;
                    }}
                }}
                if cnt >= 2 {
                    self.data.set(hp.x, hp.y, sum / cnt as f32);
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
                Some(RawImage::new_from_master_format_file(file_name)?)
            },
            None => None,
        };

        let (dark_image, hot_pixels) = match master_dark {
            Some(file_name) => {
                log::info!(
                    "loading master dark '{}'...",
                    fs_utils::path_to_str(file_name)
                );
                let mut image = RawImage::new_from_master_format_file(file_name)?;
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
                let mut image = RawImage::new_from_master_format_file(file_name)?;
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
}
