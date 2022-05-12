use std::{path::*, io::*, fs::*, collections::HashSet, hash::Hash};
use rayon::prelude::*;
use serde::{Serialize, Deserialize};
use byteorder::*;
use bitflags::bitflags;
use crate::{image::*, fs_utils, log_utils::*};

const RAW_FILE_SIG: &[u8] = b"raw-file-1";
const RAW_VERS: u64 = 1;

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
        let mut sig = [0u8; RAW_FILE_SIG.len()];
        src.read_exact(&mut sig)?;
        if &sig != RAW_FILE_SIG { anyhow::bail!("File is not raw one"); }
        let version = leb128::read::unsigned(src)?;
        if version != RAW_VERS { anyhow::bail!("wrong file version: {}", version); }
        let header_len = leb128::read::unsigned(src)? as usize;
        let mut buf = Vec::<u8>::new();
        buf.resize(header_len, 0);
        src.read_exact(&mut buf)?;
        let info_str = std::str::from_utf8(buf.as_slice())?;
        Ok(serde_json::from_str(info_str)?)
    }

    pub fn write_to<W: Write>(&self, dst: &mut W) -> anyhow::Result<()> {
        dst.write(RAW_FILE_SIG)?; // signature
        leb128::write::unsigned(dst, RAW_VERS)?;
        let info_header = serde_json::to_string(&self).unwrap();
        leb128::write::unsigned(dst, info_header.len() as u64)?;
        dst.write(&info_header.as_bytes())?;
        Ok(())
    }

    pub fn check_is_compatible(&self, other: &RawImageInfo) -> bool {
        self.width == other.width &&
        self.height == other.height &&
        self.cfa == other.cfa
    }
}
pub struct RawImage {
    pub info: RawImageInfo,
    pub data: ImageLayerF32,
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct HotPixel {
    x: Crd,
    y: Crd,
}

bitflags! { pub struct RawLoadFlags: u32 {
    const EXTRACT_BLACK = 1;
    const INF_OVEREXPOSURES  = 2;
}}

impl RawImage {
    pub fn new_from_info(info: RawImageInfo) -> RawImage {
        let width = info.width;
        let height = info.height;
        RawImage { info, data: ImageLayerF32::new(width, height) }
    }

    pub fn load_camera_raw_file(
        file_name:  &PathBuf,
        flags:      RawLoadFlags,
        disk_mutex: Option<&std::sync::Mutex<()>>
    ) -> anyhow::Result<RawImage> {
        let raw = if let Some(mutex) = disk_mutex {
            let lock = mutex.lock();
            let mut file = BufReader::new(File::open(file_name)?);
            let result = rawloader::decode(&mut file)?;
            drop(lock);
            result
        } else {
            let mut file = BufReader::new(File::open(file_name)?);
            rawloader::decode(&mut file)?
        };

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
            exif.camera = raw_exif.get_str(rawloader::Tag::Model).and_then(|v| Some(v.to_string()));
        }

        let mut info = RawImageInfo {
            width, height,
            max_values:   [0.0; 4],
            black_values: [0.0; 4],
            wb:           [0.0; 4],
            cfa:          Cfa::from_str(&raw.cfa.name[..], crop_left, crop_top),
            exif,
        };

        let mut data = ImageLayerF32::new(width, height);

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

        let mut inf_overexposures = flags.contains(RawLoadFlags::INF_OVEREXPOSURES);
        for i in 0..raw.whitelevels.len() {
            if raw.whitelevels[i] < raw.blacklevels[i] {
                inf_overexposures = false;
            }
        }

        let extract_black = flags.contains(RawLoadFlags::EXTRACT_BLACK);
        if inf_overexposures || extract_black {
            let mut correct_values = |color, black_level, max| {
                for (x, y, v) in data.iter_crd_mut() {
                    if info.cfa.get_pixel_color(x, y) != color { continue; }
                    if inf_overexposures && *v > max {
                        *v = f32::INFINITY;
                    } else if extract_black {
                        *v -= black_level;
                    }
                }
            };

            const MAX_K: f32 = 0.95;

            if let Cfa::Mono = &info.cfa {
                correct_values(CfaColor::Mono, raw.blacklevels[0] as f32, raw.whitelevels[0] as f32 * MAX_K);
            } else {
                correct_values(CfaColor::R, raw.blacklevels[0] as f32, raw.whitelevels[0] as f32 * MAX_K);
                correct_values(CfaColor::G, raw.blacklevels[1] as f32, raw.whitelevels[1] as f32 * MAX_K);
                correct_values(CfaColor::B, raw.blacklevels[2] as f32, raw.whitelevels[2] as f32 * MAX_K);
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

        Ok(RawImage{ info, data })
    }

    pub fn save_to_internal_format_file(&self, file_name: &Path) -> anyhow::Result<()> {
        let mut file = BufWriter::new(File::create(file_name)?);
        self.info.write_to(&mut file)?;
        for v in self.data.iter() { file.write_f32::<BigEndian>(*v)?; }
        Ok(())
    }

    pub fn new_from_internal_format_file(file_name: &Path) -> anyhow::Result<RawImage> {
        let mut file = std::io::BufReader::new(std::fs::File::open(file_name)?);
        let info = RawImageInfo::read_from(&mut file)?;
        let mut image = ImageLayerF32::new(info.width, info.height);
        for v in image.iter_mut() { *v = file.read_f32::<BigEndian>()?; }
        Ok(RawImage{ info, data: image })
    }

    pub fn demosaic_linear(&self, fast: bool) -> anyhow::Result<Image> {
        if self.data.is_empty() {
            anyhow::bail!("Raw image is empty");
        }

        let mut result = Image::new();
        match &self.info.cfa {
            Cfa::Mono => {
                result.make_grey(self.info.width, self.info.height);
                let range = 1.0 / self.info.max_values[0];
                for (d, s) in result.l.iter_mut().zip(self.data.iter()) {
                    *d = range * *s;
                }
            }
            Cfa::Pattern(p) => {
                result.make_color(self.info.width, self.info.height);
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

                const LIN_SAME:       &[(Crd, Crd)] = &[(0, 0)];
                const LIN_UP_DOWN:    &[(Crd, Crd)] = &[(0, -1), (0, 1)];
                const LIN_LEFT_RIGHT: &[(Crd, Crd)] = &[(-1, 0), (1, 0)];
                const LIN_DIAG:       &[(Crd, Crd)] = &[(-1, -1), (1, -1), (-1, 1), (1, 1)];
                const LIN_CROSS:      &[(Crd, Crd)] = &[(0, -1), (-1, 0), (1, 0), (0, 1)];

                let demosaic_row = |y, d_row: &mut[f32], ct: CfaColor, k: f32| {
                    let s_row = self.data.row(y);
                    for (x, (d, s)) in d_row.iter_mut().zip(s_row).enumerate() {
                        let x = x as Crd;
                        let raw_ct = p.get_color_type(x, y);

                        *d = k * if raw_ct == ct {
                            *s
                        } else {
                            let pixels = match (raw_ct, ct) {
                                (_, CfaColor::G) =>
                                    LIN_CROSS,
                                (CfaColor::R, CfaColor::B) =>
                                    LIN_DIAG,
                                (CfaColor::B, CfaColor::R) =>
                                    LIN_DIAG,
                                (CfaColor::G, CfaColor::R) if p.get_color_type(x+1, y) == CfaColor::R =>
                                    LIN_LEFT_RIGHT,
                                (CfaColor::G, CfaColor::R) =>
                                    LIN_UP_DOWN,
                                (CfaColor::G, CfaColor::B) if p.get_color_type(x+1, y) == CfaColor::B =>
                                    LIN_LEFT_RIGHT,
                                (CfaColor::G, CfaColor::B) =>
                                    LIN_UP_DOWN,
                                (_, _) => LIN_SAME,
                            };

                            let mut sum = 0_f32;
                            let mut cnt = 0_u16;
                            for crd in pixels { if let Some(v) = self.data.get(x+crd.0, y+crd.1) {
                                debug_assert!(p.get_color_type(x+crd.0, y+crd.1) == ct);
                                sum += v;
                                cnt += 1;
                            }}
                            if cnt != 0 { sum / cnt as f32 } else { 0.0 }
                        };
                    }
                };

                if !fast {
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
            }
        }
        Ok(result)
    }

    pub fn find_hot_pixels(&self) -> Vec<HotPixel> {
        const R: Crd = 2;
        let mut result = Vec::new();
        let max_dev = (self.info.max_values[0] * 0.005).powf(2.0);
        for (x, y, v) in self.data.iter_crd() {
            let mut min: Option<f32> = None;
            let mut max: Option<f32> = None;
            let mut sum = 0_f32;
            let mut cnt: usize = 0;
            for (_, _, sv) in self.data.iter_rect_crd(x-R, y-R, x+R, y+R) {
                if let Some(ref mut min) = min { if sv < *min { *min = sv; } }
                else { min = Some(sv); }
                if let Some(ref mut max) = max { if sv > *max { *max = sv; } }
                else { max = Some(sv); }
                sum += sv;
                cnt += 1;
            }
            if cnt < 2 { continue; }
            let mean = (sum - min.unwrap() - max.unwrap()) / ((cnt - 2) as f32);
            let pt_dev = (mean - v) * (mean - v);
            if pt_dev > max_dev { result.push(HotPixel{x, y}); }
        }
        result
    }

    pub fn remove_hot_pixels(&mut self, hot_pixels: &[HotPixel]) {
        if hot_pixels.is_empty() { return; }
        let mut top_pixels_index: HashSet<HotPixel> = HashSet::from_iter(hot_pixels.iter().cloned());
        for hp in hot_pixels {
            for r in 1..=2 {
                let mut sum = 0_f32;
                let mut cnt = 0_u32;
                let color = self.info.cfa.get_pixel_color(hp.x, hp.y);
                for x in hp.x-r ..= hp.x+r { for y in hp.y-r ..= hp.y+r {
                    if color != self.info.cfa.get_pixel_color(x, y) { continue; }
                    if top_pixels_index.contains(&HotPixel{x, y}) { continue; }
                    if let Some(v) = self.data.get(x, y) { if !v.is_infinite() {
                        sum += v;
                        cnt += 1;
                    }}
                }}
                if cnt >= 2 {
                    self.data.set(hp.x, hp.y, sum / cnt as f32);
                    top_pixels_index.remove(&hp);
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
    pub hot_pixels: Vec<HotPixel>,
}

impl CalibrationData {
    pub fn new_empty() -> CalibrationData {
        CalibrationData {
            dark_image: None,
            flat_image: None,
            bias_image: None,
            hot_pixels: Vec::new(),
        }
    }

    pub fn load(
        master_flat: &Option<PathBuf>,
        master_dark: &Option<PathBuf>,
        master_bias: &Option<PathBuf>,
    ) -> anyhow::Result<CalibrationData> {
        let bias_image = match master_bias {
            Some(file_name) => {
                log::info!(
                    "loading master bias '{}'...",
                    fs_utils::path_to_str(file_name)
                );
                Some(RawImage::new_from_internal_format_file(file_name)?)
            },
            None => None,
        };

        let (dark_image, hot_pixels) = match master_dark {
            Some(file_name) => {
                log::info!(
                    "loading master dark '{}'...",
                    fs_utils::path_to_str(file_name)
                );
                let mut image = RawImage::new_from_internal_format_file(file_name)?;
                if let Some(bias_image) = &bias_image {
                    image.data -= &bias_image.data;
                }

                let hot_pixels = image.find_hot_pixels();
                log::info!("hot pixels count = {}", hot_pixels.len());

                (Some(image), hot_pixels)
            },
            None => (None, Vec::new()),
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
            bias_image,
            hot_pixels
        })
    }
}
