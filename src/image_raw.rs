use std::{path::*, io::*, fs::*, collections::HashSet, hash::Hash};
use itertools::{izip};
use rayon::prelude::*;
use serde::{Serialize, Deserialize};
use byteorder::*;
use crate::{image::*, fs_utils, log_utils::*, calc::*, compression::*, image_formats::*};
use bitstream_io::{BitWriter, BitWrite};

const CALIBR_FILE_SIG: &[u8] = b"calibr-file-5";
const MASTER_FILE_SIG: &[u8] = b"master-file-5";

// Color coefficients for camera sensors
const SONY_IMX294_WB: [f32; 4] = [1.216, 1.000, 1.424, 0.000];
const SONY_IMX294_CCM: &[f32; 9] = &[
     1.89859, -0.78759, -0.26672,
     0.12546,  0.70569,  0.06947,
     0.01381,  0.05857,  0.50326,
];

const SONY_IMX455_WB: [f32; 4] = [1.186, 1.000, 1.347, 0.000];
const SONY_IMX455_CCM: &[f32; 9] = &[
     1.82345, -0.73083, -0.27351,
    -0.17026,  0.87025,  0.14144,
    -0.02383,  0.05566,  0.53220,
];

const SONY_IMX178_WB: [f32; 4] = [1.292, 1.000, 1.387, 0.000];
const SONY_IMX178_CCM: &[f32; 9] = &[
     1.71221, -0.66253, -0.23873,
     0.06362,  0.75193,  0.11160,
     0.03049,  0.04920,  0.53220,
];

const SONY_IMX571_WB: [f32; 4] = [1.11, 1.00, 1.25, 0.00];  // ???
const SONY_IMX071_WB: [f32; 4] = [1.11, 1.00, 1.18, 0.00];  // ???
const SONY_IMX183_WB: [f32; 4] = [1.04, 1.00, 1.25, 0.00];  // ???
const SONY_IMX533_WB: [f32; 4] = [1.11, 1.00, 1.23, 0.00];  // ???
const SONY_IMX410_WB: [f32; 4] = [1.14, 1.00, 1.25, 0.00];  // ???

// table for cameras if no params in raw FITS file
const CAMERAS_TABLE: &[(&str, [f32; 4], Option<CfaType>, Option<&[f32; 9]>)] = &[
    // camera     color balance   bayer type           color matrix
    ("asi294mc",  SONY_IMX294_WB, Some(CfaType::RGGB), Some(SONY_IMX294_CCM)),
    ("asi2600mc", SONY_IMX571_WB, Some(CfaType::RGGB), None),
    ("asi071mc",  SONY_IMX071_WB, None,                None),
    ("asi183mc",  SONY_IMX183_WB, None,                None),
    ("asi533mc",  SONY_IMX533_WB, None,                None),
    ("asi6200mc", SONY_IMX455_WB, None,                Some(SONY_IMX455_CCM)),
    ("asi2400mc", SONY_IMX410_WB, None,                None),
    ("asi178mc",  SONY_IMX178_WB, None,                Some(SONY_IMX178_CCM)),
];

pub fn find_camera_params(
    camera_name: Option<&str>
) -> Option<([f32; 4], Option<CfaType>, Option<&[f32; 9]>)> {
    if let Some(camera_name) = camera_name {
        let name_lc = camera_name.to_string().to_lowercase();
        CAMERAS_TABLE.iter()
            .find(|(c, _, _, _)| name_lc.contains(c))
            .map(|(_, wb, cfa, ccm)| (*wb, *cfa, *ccm))
    } else {
        None
    }
}

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

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct RawImageInfo {
    pub width: Crd,
    pub height: Crd,
    pub max_values: [f32; 4],
    pub black_values: [f32; 4],
    pub wb: [f32; 4],
    pub xyz_to_cam: Option<[f32; 9]>,
    pub cfa: Cfa,
    pub camera: Option<String>,
    pub exposure: Option<f32>,
    pub iso: Option<u32>,
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

    pub fn apply_wb(&mut self, image: &mut Image) {
        let apply = |img: &mut ImageLayerF32, k: f32| {
            for v in img.iter_mut() {
                *v *= k;
            }
        };

        apply(&mut image.r, self.wb[0]);
        apply(&mut image.g, self.wb[1]);
        apply(&mut image.b, self.wb[2]);
    }

    pub fn normalize_image(&self, image: &mut Image) {
        let process = |img: &mut ImageLayerF32, max: f32| {
            if max == 0.0 {
                return;
            }
            let k = 1.0 / max;
            for v in img.iter_mut() {
                *v *= k;
            }
        };

        process(&mut image.l, self.max_values[0]);
        process(&mut image.r, self.max_values[0]);
        process(&mut image.g, self.max_values[1]);
        process(&mut image.b, self.max_values[2]);
    }

    pub fn convert_color_space_to_srgb(&self, image: &mut Image) {
        if let Some(xyz_to_cam) = &self.xyz_to_cam {
            use nalgebra::*;

            let xyz_2_cam = matrix![
                xyz_to_cam[0], xyz_to_cam[1], xyz_to_cam[2];
                xyz_to_cam[3], xyz_to_cam[4], xyz_to_cam[5];
                xyz_to_cam[6], xyz_to_cam[7], xyz_to_cam[8];
            ];

            let srgb_2_xyz = matrix![
                0.4124564, 0.3575761, 0.1804375;
                0.2126729, 0.7151522, 0.0721750;
                0.0193339, 0.1191920, 0.9503041;
            ];

            let mut srgb_to_cam = xyz_2_cam * srgb_2_xyz;

            for row in 0..3 {
                let sum: f32 = srgb_to_cam.row(row).iter().sum();
                if sum != 0.0 { for v in srgb_to_cam.row_mut(row).iter_mut() {
                    *v /= sum;
                }}
            }

            let cam_to_rgb = srgb_to_cam.try_inverse().unwrap();
            let cam_to_rgb_values = cam_to_rgb.as_slice();

            for (r, g, b) in izip!(image.r.iter_mut(), image.g.iter_mut(), image.b.iter_mut()) {
                let r0 = *r;
                let g0 = *g;
                let b0 = *b;
                *r = r0*cam_to_rgb_values[0] + g0*cam_to_rgb_values[3] + b0*cam_to_rgb_values[6];
                *g = r0*cam_to_rgb_values[1] + g0*cam_to_rgb_values[4] + b0*cam_to_rgb_values[7];
                *b = r0*cam_to_rgb_values[2] + g0*cam_to_rgb_values[5] + b0*cam_to_rgb_values[8];
            }
            return;
        }
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
}

#[derive(Hash, PartialEq, Eq, Clone)]
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

        let xyz_to_cam = if !raw.xyz_to_cam[0][0].is_nan() {
            let mut matrix = [0_f32; 9];
            for i in 0..9 {
                matrix[i] = raw.xyz_to_cam[i / 3][i % 3];
            }
            Some(matrix)
        } else {
            None
        };

        let mut raw_info = RawImageInfo {
            width, height,
            max_values:   [0.0; 4],
            black_values: [0.0; 4],
            wb:           [0.0; 4],
            xyz_to_cam,
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
            max_values[i] = if clip_value < white_value && white_value < 1.2 * clip_value {
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
            data: image
        })
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
            let k = 1.0/self.info.black_values[0];
            for v in grayscale.l.iter_mut() {
                *v *= k;
            }
            Ok(grayscale)
        }
    }

    fn demosaic_bayer_linear(&self, p: &CfaPattern, mt: bool) -> anyhow::Result<Image> {
        if self.data.is_empty() {
            anyhow::bail!("Raw image is empty");
        }

        let mut result = Image::new_color(self.info.width, self.info.height);

        const PATH_VERT:  &[(Crd, Crd)] = &[(0, -1), (0, 1)];
        const PATH_HORIZ: &[(Crd, Crd)] = &[(-1, 0), (1, 0)];
        const PATH_DIAG:  &[(Crd, Crd)] = &[(-1, -1), (1, -1), (-1, 1), (1, 1)];
        const PATH_CROSS: &[(Crd, Crd)] = &[(0, -1), (-1, 0), (1, 0), (0, 1)];

        let demosaic_row = |y, d_row: &mut[f32], ct: CfaColor| {
            let s_row = self.data.row(y);
            for (x, (d, s)) in d_row.iter_mut().zip(s_row).enumerate() {
                let x = x as Crd;
                let raw_ct = p.get_color_type(x, y);

                *d = if raw_ct == ct {
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
                demosaic_row(y, result.r.row_mut(y), CfaColor::R);
                demosaic_row(y, result.g.row_mut(y), CfaColor::G);
                demosaic_row(y, result.b.row_mut(y), CfaColor::B);
            }
        } else {
            let width = self.data.width() as usize;

            result.r.as_slice_mut().par_chunks_mut(width)
                .enumerate()
                .for_each(|(y, d)| {
                    demosaic_row(y as Crd, d, CfaColor::R);
                });

            result.g.as_slice_mut().par_chunks_mut(width)
                .enumerate()
                .for_each(|(y, d)| {
                    demosaic_row(y as Crd, d, CfaColor::G);
                });

            result.b.as_slice_mut().par_chunks_mut(width)
                .enumerate()
                .for_each(|(y, d)| {
                    demosaic_row(y as Crd, d, CfaColor::B);
                });
        }

        Ok(result)
    }

    fn demosaic_bayer_color_ratio(&self, p: &CfaPattern, _mt: bool) -> anyhow::Result<Image> {
        if self.data.is_empty() {
            anyhow::bail!("Raw image is empty");
        }

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

                    if cnt >= 2 && 2.0 * green_sum > color_sum && green_sum != 0.0 {
                        let color_ratio = color_sum/green_sum;
                        green * color_ratio
                    } else {
                        color_sum / cnt as f32
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

                let mut hint_sum = 0_f32;
                let mut green_sum = 0_f32;
                let mut cnt = 0_u16;
                for (g, h) in [(g1, h1), (g2, h2), (g3, h3), (g4, h4)] {
                    if let (Some(g), Some(h)) = (g, h) {
                        if h != 0.0 {
                            hint_sum += h;
                            green_sum += g;
                            cnt += 1;
                        }
                    }
                }

                if cnt >= 2 && 2.0 * hint_sum > green_sum && hint_sum != 0.0 {
                    let color_ratio = green_sum / hint_sum;
                    *d = red_or_blue * color_ratio;
                }
            }
        };

        for y in 0..self.data.height() {
            demosaic_green_on_red_or_blue(y, result_image.g.row_mut(y));
        }

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

    fn remove_bad_pixels(&mut self, hot_pixels: &HashSet<BadPixel>) {
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
        compare("Camera model", &raw_cam, &cal_cam)?;

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
