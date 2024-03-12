use std::{path::*, io::*, fs::*};
use serde::*;
use tiff::{*, decoder::*, encoder::*};
use itertools::*;
use bitstream_io::{BigEndian, BitWriter, BitWrite, BitReader};
use chrono::prelude::*;
use fitsio::{*, images::*, hdu::*};
use regex::Regex;
use gettextrs::*;
use crate::{
    image::*,
    image_raw::*,
    fs_utils::*,
    compression::*,
    progress::*,
    calc::*,
    cameras_database::*
};

pub const FIT_EXTS: &[&str] = &["fit", "fits", "fts"];
pub const TIF_EXTS: &[&str] = &["tif", "tiff"];
pub const RAW_EXTS: &[&str] = &[
    "dng",
    "cr2", // Canon
    "nef", // Nikon
    "arw", // Sony
    "pef", // Pentax
];

fn try_to_decode_date_time_str(dt_str: &str) -> Option<DateTime<Local>> {
    if dt_str.is_empty() {
        return None;
    }

    let fmt_strings = [
        "%Y:%m:%d %H:%M:%S%.3f",
        "%Y:%m:%dT%H:%M:%S%.3f",
        "%Y-%m-%d %H:%M:%S%.3f",
        "%Y-%m-%dT%H:%M:%S%.3f",
        "%Y:%m:%d %H:%M:%S",
        "%Y-%m-%d %H:%M:%S",
        "%Y:%m:%dT%H:%M:%S",
        "%Y-%m-%dT%H:%M:%S",
    ];

    fmt_strings.into_iter()
        .filter_map(|fmt|
            NaiveDateTime::parse_from_str(dt_str, fmt).ok()
        )
        .map(|dt|
            Local.from_utc_datetime(&dt)
        )
        .next()
}

pub fn load_image_from_file(
    file_name:    &Path,
    force_as_raw: bool
) -> anyhow::Result<ImageData> {
    let ext = extract_extension(file_name);
    if is_raw_ext(ext) {
        let (raw, info) = RawImage::load(file_name)?;
        Ok(ImageData {image: RawOrImage::Raw(raw), info})
    } else if is_tiff_ext(ext) {
        if force_as_raw {
            anyhow::bail!("Image is not RAW camera file!");
        }
        load_image_from_tiff_file(file_name)
    } else if is_fits_ext(ext) {
        load_image_from_fits_file(file_name, force_as_raw)
    } else {
        err_format_not_supported(ext)
    }
}

pub fn save_image_to_file(
    image:     &Image,
    info:      &ImageInfo,
    file_name: &Path
) -> anyhow::Result<()> {
    assert!(!image.is_empty());

    let ext = extract_extension(file_name);
    if is_tiff_ext(ext) {
        save_image_to_tiff_file(image, info, file_name)
    } else if is_fits_ext(ext) {
        save_image_to_fits_file(image, info, file_name)
    } else {
        err_format_not_supported(ext)
    }
}

pub fn is_raw_ext(ext: &str) -> bool {
    RAW_EXTS.iter().any(|e| ext.eq_ignore_ascii_case(e))
}

pub fn is_tiff_ext(ext: &str) -> bool {
    TIF_EXTS.iter().any(|e| ext.eq_ignore_ascii_case(e))
}

pub fn is_fits_ext(ext: &str) -> bool {
    FIT_EXTS.iter().any(|e| ext.eq_ignore_ascii_case(e))
}

pub fn is_source_file_name(file_name: &Path) -> bool {
    let ext = extract_extension(file_name);
    is_raw_ext(ext) |
    is_tiff_ext(ext) |
    is_fits_ext(ext)
}

pub enum RawOrImage {
    Image(Image),
    Raw(RawImage),
}

pub struct ImageData {
    pub image: RawOrImage,
    pub info: ImageInfo,
}

fn err_format_not_supported<R>(ext: &str) -> anyhow::Result<R> {
    Err(anyhow::anyhow!("Image format `{}` is not supported", ext))
}

#[derive(Default, Serialize, Deserialize, Clone, Debug)]
pub struct ImageInfo {
    /// Full file name
    pub file_name: PathBuf,

    /// Width in pixels
    pub width: usize,

    /// Height in pixels
    pub height: usize,

    /// Datetime from Exif or from file tile
    pub file_time: Option<DateTime<Local>>,

    /// Type of CFA pattern
    pub cfa_type: Option<CfaType>,

    /// ISO or gain
    pub iso: Option<u32>,

    /// Exposure time in seconds
    pub exp: Option<f64>,

    /// F-number (4.0 for f/4)
    pub fnumber: Option<f32>,

    /// Focal len in millimeters
    pub focal_len: Option<f32>,

    /// Camera name
    pub camera: Option<String>,

    /// Lens or telescope
    pub lens: Option<String>,
}


fn load_src_file_info(
    file_name:    &Path,
    fn_extractor: &FromFileNameInfoExtractor
) -> anyhow::Result<ImageInfo> {
    let ext = extract_extension(file_name);
    let mut result = if is_raw_ext(ext) {
        load_src_file_info_raw(file_name)
    } else if is_tiff_ext(ext) {
        load_src_file_info_tiff(file_name)
    } else if is_fits_ext(ext) {
        load_src_file_info_fits(file_name)
    } else {
        err_format_not_supported(ext)
    }?;

    if result.exp.is_none() {
        result.exp = fn_extractor.extract_exp(file_name)
    }

    if result.iso.is_none() {
        result.iso = fn_extractor.extract_gain(file_name)
    }

    Ok(result)
}

struct FromFileNameInfoExtractor {
    gain_re: Regex,
    exp_ms_re: Regex,
}

impl FromFileNameInfoExtractor {
    fn new() -> Self {
        Self {
            gain_re: Regex::new(r"(?i)gain=(\d+)").unwrap(),
            exp_ms_re: Regex::new(r"(?i)exposure=(\d+(?:\.\d+))ms").unwrap(),
        }
    }

    fn extract_gain(&self, file_name: &Path) -> Option<u32> {
        self.gain_re.captures(file_name.to_str().unwrap_or(""))
            .and_then(|cs| cs.get(1))
            .and_then(|c| c.as_str().parse::<u32>().ok())
    }

    fn extract_exp(&self, file_name: &Path) -> Option<f64> {
        self.exp_ms_re.captures(file_name.to_str().unwrap_or(""))
            .and_then(|cs| cs.get(1))
            .and_then(|c| c.as_str().parse::<f64>().ok())
            .map(|v| v / 1000.0)
    }
}

pub fn load_src_file_info_for_files(
    file_names:   &Vec<PathBuf>,
    is_cancelled: &IsCancelledFun,
    progress:     &ProgressTs,
) -> anyhow::Result<Vec<ImageInfo>> {
    let mut result = Vec::new();
    progress.lock().unwrap().stage("Loading short file information...");
    progress.lock().unwrap().set_total(file_names.len());
    let fn_extractor = FromFileNameInfoExtractor::new();
    for file_name in file_names {
        if is_cancelled() {
            anyhow::bail!(gettext("Cancelled"));
        }
        let item = load_src_file_info(file_name, &fn_extractor)?;
        progress.lock().unwrap().progress(true, file_name.to_str().unwrap_or(""));
        result.push(item);
    }
    Ok(result)
}

/*****************************************************************************/

// RAW format

pub fn load_src_file_info_raw(file_name: &Path) -> anyhow::Result<ImageInfo> {
    let mut file = std::fs::File::open(&file_name)?;
    let raw_data = rawloader::decode_exif_only(&mut file)?;
    let mut iso = None;
    let mut exp = None;
    let mut file_time = None;
    let mut fnumber = None;
    let mut camera = None;
    let mut focal_len = None;
    let mut lens = None;
    if let Some(exif) = &raw_data.exif {
        iso = exif.get_uint(rawloader::Tag::ISOSpeed);
        exp = exif.get_rational(rawloader::Tag::ExposureTime).map(|v| v as f64);
        fnumber = exif.get_rational(rawloader::Tag::FNumber);
        camera = exif.get_str(rawloader::Tag::Model).map(|v| v.to_string());
        focal_len = exif.get_rational(rawloader::Tag::FocalLength);
        lens = exif.get_str(rawloader::Tag::LensModel).map(|v| v.to_string());
        if let Some(time_str) = exif.get_str(rawloader::Tag::DateTimeOriginal) {
            file_time = try_to_decode_date_time_str(time_str);
        }
    }

    // time from fs
    if file_time.is_none() {
        file_time = file.metadata()
            .ok()
            .and_then(|t| t.created().ok())
            .map(|st| st.into());
    }

    let cfa = Cfa::from_str(&raw_data.cropped_cfa().name);
    let cfa_type = match cfa {
        Cfa::Pattern(p) => Some(p.pattern_type),
        _ => None,
    };

    Ok(ImageInfo {
        file_name: file_name.to_path_buf(),
        file_time,
        width: raw_data.width,
        height: raw_data.height,
        iso,
        exp,
        cfa_type,
        fnumber,
        focal_len,
        camera,
        lens,
    })
}

pub fn load_raw_file(file_name: &Path) -> anyhow::Result<(RawImage, ImageInfo)> {
    let ext = extract_extension(file_name);
    if is_raw_ext(ext) {
        return RawImage::load(file_name);
    } else if is_fits_ext(ext) {
        let result = load_image_from_fits_file(file_name, true)?;
        if let ImageData { image: RawOrImage::Raw(raw), info } = result {
            return Ok((raw, info));
        }
    }

    anyhow::bail!("This file is not supported as RAW");
}

/*****************************************************************************/

// TIFF format

pub fn load_src_file_info_tiff(file_name: &Path) -> anyhow::Result<ImageInfo> {
    let mut reader = BufReader::new(File::open(file_name)?);
    let mut decoder = Decoder::new(&mut reader)?;

    let gray_or_rgb = matches!(decoder.colortype()?, ColorType::Gray(_)|ColorType::RGB(_));
    if !gray_or_rgb {
        anyhow::bail!("Color type is not supported")
    }

    let (width, height) = decoder.dimensions()?;

    let mut file_time = None;

    // 0x9003 = DateTimeOriginal
    let time_str = decoder.get_tag_ascii_string(tags::Tag::Unknown(0x9003));
    if let Ok(time_str) = time_str {
        file_time =
            NaiveDateTime::parse_from_str(&time_str, "%Y:%m:%d %H:%M:%S")
                .ok()
                .map(|dt| Local.from_local_datetime(&dt))
                .map(|res| res.single())
                .flatten();

    }

    if file_time.is_none() {
        let file = reader.into_inner();
        file_time = file.metadata()
            .ok()
            .and_then(|t| t.created().ok())
            .map(|st| st.into());
    }

    Ok(ImageInfo {
        file_name: file_name.to_path_buf(),
        file_time,
        width: width as usize,
        height: height as usize,
        .. Default::default()
    })
}

pub fn load_image_from_tiff_file(file_name: &Path) -> anyhow::Result<ImageData> {
    fn assign_img_data<S: Copy>(
        src:    &[S],
        img:    &mut Image,
        is_rgb: bool,
        cvt:    fn (from: S) -> f32
    ) -> anyhow::Result<()> {
        if is_rgb {
            for (dr, dg, db, (sr, sg, sb))
            in izip!(img.r.iter_mut(), img.g.iter_mut(), img.b.iter_mut(), src.iter().tuples()) {
                *dr = cvt(*sr);
                *dg = cvt(*sg);
                *db = cvt(*sb);
            }
        } else {
            for (d, s) in img.l.iter_mut().zip(src.iter()) {
                *d = cvt(*s);
            }
        }
        Ok(())
    }

    let file = BufReader::new(File::open(file_name)?);
    let mut decoder = Decoder::new(file)?;

    let (width, height) = decoder.dimensions()?;

    let (mut image, is_rgb) = match decoder.colortype()? {
        ColorType::Gray(_) => {
            (Image::new_grey(width as Crd, height as Crd), false)
        }
        ColorType::RGB(_) => {
            (Image::new_color(width as Crd, height as Crd), true)
        }
        ct =>
            anyhow::bail!("Color type {:?} unsupported", ct)
    };

    let img_res = decoder.read_image()?;

    match img_res {
        DecodingResult::U8(data) =>
            assign_img_data(
                &data,
                &mut image,
                is_rgb,
                |v| v as f32 / u8::MAX as f32
            ),

        DecodingResult::U16(data) =>
            assign_img_data(
                &data,
                &mut image,
                is_rgb,
                |v| (v as f64 / u16::MAX as f64) as f32
            ),

        DecodingResult::U32(data) =>
            assign_img_data(
                &data,
                &mut image,
                is_rgb,
                |v| (v as f64 / u32::MAX as f64) as f32
            ),

        DecodingResult::F32(data) =>
            assign_img_data(
                &data,
                &mut image,
                is_rgb,
                |v| v
            ),

        DecodingResult::F64(data) =>
            assign_img_data(
                &data,
                &mut image,
                is_rgb,
                |v| v as f32
            ),

        _ =>
            Err(anyhow::anyhow!("Format unsupported"))
    }?;

    Ok(ImageData{
        image: RawOrImage::Image(image),
        info: Default::default()
    })
}

pub fn save_grayscale_image_to_tiff_file(
    image:     &ImageLayerF32,
    info:      &ImageInfo,
    file_name: &Path
) -> anyhow::Result<()> {
    assert!(!image.is_empty());
    let mut file = BufWriter::new(File::create(file_name)?);
    let mut decoder = TiffEncoder::new(&mut file)?;
    let mut tiff = decoder.new_image::<colortype::Gray32Float>(
        image.width() as u32,
        image.height() as u32
    )?;
    write_info_into_tiff(tiff.encoder(), info)?;
    tiff.write_data(image.iter().as_slice())?;
    Ok(())
}

pub fn save_image_to_tiff_file(
    image:     &Image,
    info:      &ImageInfo,
    file_name: &Path
) -> anyhow::Result<()> {
    assert!(!image.is_empty());

    let mut file = BufWriter::new(File::create(file_name)?);
    let mut decoder = TiffEncoder::new(&mut file)?;
    if image.is_greyscale() {
        let mut tiff = decoder.new_image::<colortype::Gray32Float>(
            image.width() as u32,
            image.height() as u32
        )?;
        write_info_into_tiff(tiff.encoder(), info)?;
        tiff.write_data(image.l.iter().as_slice())?;
    }
    else if image.is_rgb() {
        let data: Vec<_> = izip!(image.r.iter(), image.g.iter(), image.b.iter())
            .map(|(r, g, b)| [*r, *g, *b])
            .flatten()
            .collect();
        let mut tiff = decoder.new_image::<colortype::RGB32Float>(
            image.width() as u32,
            image.height() as u32
        )?;
        write_info_into_tiff(tiff.encoder(), info)?;
        tiff.write_data(&data)?;
    } else {
        panic!("Internal error");
    }
    Ok(())
}

pub fn save_image_to_tiff16_file(
    image:     &Image,
    info:      &ImageInfo,
    file_name: &Path
) -> anyhow::Result<()> {
    assert!(!image.is_empty());

    let mut file = BufWriter::new(File::create(file_name)?);
    let mut decoder = TiffEncoder::new(&mut file)?;
    if image.is_greyscale() {
        let mut tiff = decoder.new_image::<colortype::Gray32Float>(
            image.width() as u32,
            image.height() as u32
        )?;
        write_info_into_tiff(tiff.encoder(), info)?;
        tiff.write_data(image.l.iter().as_slice())?;
    }
    else if image.is_rgb() {
        let data: Vec<_> = izip!(image.r.iter(), image.g.iter(), image.b.iter())
            .map(|(r, g, b)| [(*r * 65535.0) as u16, (*g * 65535.0) as u16, (*b * 65535.0) as u16])
            .flatten()
            .collect();
        let mut tiff = decoder.new_image::<colortype::RGB16>(
            image.width() as u32,
            image.height() as u32
        )?;
        write_info_into_tiff(tiff.encoder(), info)?;
        tiff.write_data(&data)?;
    } else {
        panic!("Internal error");
    }
    Ok(())
}


fn write_info_into_tiff<W: Write + Seek, K: TiffKind>(
    enc:  &mut tiff::encoder::DirectoryEncoder<W, K>,
    info: &ImageInfo
) -> anyhow::Result<()> {
    use tiff::tags::*;

    enc.write_tag(
        Tag::Software,
        format!(
            "{} v{}",
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION")
        ).as_str()
    )?;

    if let Some(exp_time) = info.exp {
        enc.write_tag(Tag::Unknown(0x829a), &[exp_time as u32, 1_u32][..])?;
    }
    Ok(())
}

/*****************************************************************************/

// FITS format

fn find_image_hdu(
    file: &mut FitsFile
) -> anyhow::Result<(FitsHdu, usize, usize, bool, ImageType)> {
    for hdu in file.iter() {
        if let HduInfo::ImageInfo { shape, image_type } = &hdu.info {
            let image_type = *image_type;
            match shape.as_slice() {
                &[height, width] =>
                    return Ok((hdu, width, height, false, image_type)),
                &[3, height, width] =>
                    return Ok((hdu, width, height, true, image_type)),
                _ => {},
            }
        }
    }
    anyhow::bail!("Supported image HDU not found in FITS file")
}

fn load_src_file_info_from_fits_hdu(
    fptr:      &mut FitsFile,
    hdu:       &FitsHdu,
    file_name: &Path,
    width:     usize,
    height:    usize
) -> ImageInfo {
    let exp_time = hdu.read_key(fptr, "EXPTIME").ok();
    let gain = hdu.read_key::<f32>(fptr, "GAIN").ok();
    let bayer = hdu.read_key::<String>(fptr, "BAYERPAT").ok();
    let camera =hdu.read_key::<String>(fptr, "INSTRUME")
        .or_else(|_| hdu.read_key::<String>(fptr, "CAMERA")).ok();
    let focal_len = hdu.read_key(fptr, "FOCALLEN").ok();
    let focal_ratio = hdu.read_key(fptr, "FOCRATIO").ok();
    let lens = hdu.read_key(fptr, "TELESCOP").ok();

    let file_time = hdu.read_key::<String>(fptr, "DATE-LOC")
        .or_else(|_| hdu.read_key::<String>(fptr, "DATE-OBS")).ok()
        .and_then(|v| try_to_decode_date_time_str(&v))
        .or_else(|| get_file_time(file_name).ok() );

    ImageInfo {
        file_name: file_name.to_path_buf(),
        width,
        height,
        file_time,
        iso: gain.map(|v| v as u32),
        exp: exp_time,
        cfa_type: bayer.and_then(|v| CfaType::from_string(&v)),
        fnumber: focal_ratio,
        focal_len,
        camera,
        lens,
        .. Default::default()
    }
}



#[cfg(target_os = "windows")]
mod win_fits_open {
    use std::{sync::Mutex, path::PathBuf};

    pub static OPEN_FITS_MUTEX: Mutex<()> = Mutex::new(());

    pub struct CurDirRestore {
        cur_dir: PathBuf,
    }

    impl CurDirRestore {
        pub fn new() -> anyhow::Result<Self> {
            Ok(Self {
                cur_dir: std::env::current_dir()?,
            })
        }
    }

    impl Drop for CurDirRestore {
        fn drop(&mut self) {
            _ = std::env::set_current_dir(&self.cur_dir);
        }
    }
}

#[cfg(target_os = "windows")]
fn fits_file_open_helper<F>(file_name: &Path, fun: F) -> anyhow::Result<FitsFile>
where F: Fn(&Path) -> anyhow::Result<FitsFile> {
    use win_fits_open::*;
    let result = match (file_name.parent(), file_name.file_name()) {
        (Some(file_dir), Some(rel_name)) => {
            let change_dir_locker = OPEN_FITS_MUTEX.lock();
            let cur_dir_restore = CurDirRestore::new()?;
            std::env::set_current_dir(file_dir)?;
            let rel_name = rel_name.to_str().unwrap_or("");
            let fits = fun(&Path::new(rel_name))?;
            drop(cur_dir_restore);
            drop(change_dir_locker);
            fits
        },
        _ =>
            fun(file_name)?,
    };
    Ok(result)
}

#[cfg(not(target_os = "windows"))]
fn fits_file_open_helper<F>(file_name: &Path, fun: F) -> anyhow::Result<FitsFile>
where F: Fn(&Path) -> anyhow::Result<FitsFile> {
    fun(file_name)
}

pub fn load_src_file_info_fits(file_name: &Path) -> anyhow::Result<ImageInfo> {
    let mut fptr = fits_file_open_helper(
        file_name,
        |file_name| Ok(FitsFile::open(file_name)?)
    )?;
    let (image_hdu, width, height, ..) = find_image_hdu(&mut fptr)?;
    Ok(load_src_file_info_from_fits_hdu(
        &mut fptr,
        &image_hdu,
        file_name,
        width,
        height
    ))
}

pub fn load_image_from_fits_file(
    file_name:    &Path,
    force_as_raw: bool
) -> anyhow::Result<ImageData> {
    let mut fptr = fits_file_open_helper(
        file_name,
        |file_name| Ok(FitsFile::open(file_name)?)
    )?;

    let (image_hdu, width, height, is_color_image, data_type) = find_image_hdu(&mut fptr)?;
    let info = load_src_file_info_from_fits_hdu(&mut fptr, &image_hdu, file_name, width, height);
    let camera_params = find_camera_params(info.camera.as_deref());

    if !is_color_image && (info.cfa_type.is_some() || camera_params.is_some() || force_as_raw) {
        let max = match data_type {
            ImageType::UnsignedByte => 127.0,
            ImageType::Byte  =>  255.0,
            ImageType::Short  => 32767.0,
            ImageType::UnsignedShort  => 65535.0,
            ImageType::Long  => ((1u64 << 31) - 1) as f64,
            ImageType::UnsignedLong  => ((1u64 << 32) - 1) as f64,
            ImageType::LongLong  => ((1u64 << 63) - 1) as f64,
            ImageType::Float  => 1.0,
            ImageType::Double  => 1.0,
        };

        let ct = info.cfa_type.or_else(|| camera_params.map(|(_, ct, _)| ct).flatten());
        let black = image_hdu.read_key(&mut fptr, "BLKLEVEL").unwrap_or(0.0);

        let raw_info = RawImageInfo {
            width: info.width as Crd,
            height: info.height as Crd,
            max_values: [max as f32, max as f32, max as f32, max as f32],
            black_values: [black, black, black, black],
            wb: camera_params.map(|(wb, _, _)| wb).unwrap_or([1.0, 1.0, 1.0, 1.0]),
            cam_to_rgb: camera_params.and_then(|(_, _, ccm)| ccm.map(|v| v.clone())),
            cfa: Cfa::from_cfa_type(ct),
            camera: info.camera.clone(),
            exposure: info.exp.map(|v| v as f32),
            iso: info.iso,
        };

        let data: Vec<f32> = image_hdu.read_image(&mut fptr)?;
        let image = ImageLayerF32::new_from_vec(width as Crd, height as Crd, data);

        let raw = RawImage {
            info: raw_info,
            data: image,
        };
        return Ok(ImageData{
            image: RawOrImage::Raw(raw),
            info
        })
    }

    if force_as_raw {
        anyhow::bail!("Image is not RAW camera file!");
    }

    let mut image = Image::new();

    if is_color_image {
        let r_data = image_hdu.read_section(&mut fptr, 0, width * height)?;
        let g_data = image_hdu.read_section(&mut fptr, width * height, 2 * width * height)?;
        let b_data = image_hdu.read_section(&mut fptr, 2 * width * height, 3 * width * height)?;

        image.r = ImageLayerF32::new_from_vec(width as Crd, height as Crd, r_data);
        image.g = ImageLayerF32::new_from_vec(width as Crd, height as Crd, g_data);
        image.b = ImageLayerF32::new_from_vec(width as Crd, height as Crd, b_data);
    } else {
        let data: Vec<f32> = image_hdu.read_image(&mut fptr)?;
        image.l = ImageLayerF32::new_from_vec(width as Crd, height as Crd, data);
    }

    let max = image.l.iter()
        .chain(image.r.iter())
        .chain(image.g.iter())
        .chain(image.b.iter())
        .copied()
        .max_by(cmp_f32)
        .unwrap_or(0.0);

    if max > 1.0 {
        image.mult_f32(1.0 / max);
    }

    Ok(ImageData{
        image: RawOrImage::Image(image),
        info
    })
}

pub fn save_image_to_fits_file(
    image:     &Image,
    info:      &ImageInfo,
    file_name: &Path
) -> anyhow::Result<()> {
    let width = image.width() as usize;
    let height = image.height() as usize;

    _ = std::fs::remove_file(file_name);

    let dimensions = if image.is_rgb() {
        vec![3_usize, height, width]
    } else {
        vec![height, width]
    };

    let image_description = ImageDescription {
        data_type: ImageType::Float,
        dimensions: &dimensions,
    };

    let mut fptr = fits_file_open_helper(
        file_name,
        |file_name| {
            Ok(FitsFile::create(file_name)
                .with_custom_primary(&image_description)
                .open()?)
        }
    )?;

    let hdu = fptr.primary_hdu().unwrap();

    if image.is_rgb() {
        hdu.write_region(
            &mut fptr,
            &[&(0..width), &(0..height), &(0..1)],
            image.r.as_slice()
        )?;
        hdu.write_region(
            &mut fptr,
            &[&(0..width), &(0..height), &(1..2)],
            image.g.as_slice()
        )?;
        hdu.write_region(
            &mut fptr,
            &[&(0..width), &(0..height), &(2..3)],
            image.b.as_slice()
        )?;
    } else {
        hdu.write_image(&mut fptr, image.l.as_slice())?;
    };

    if let Some(exp) = info.exp {
        hdu.write_key(&mut fptr, "EXPTIME", exp)?;
    }

    if let Some(camera) = &info.camera {
        hdu.write_key(&mut fptr, "INSTRUME", camera.as_str())?;
    }

    if let Some(lens) = &info.lens {
        hdu.write_key(&mut fptr, "TELESCOP", lens.as_str())?;
    }

    hdu.write_key(&mut fptr, "ROWORDER", "TOP-DOWN")?;

    Ok(())
}

/*****************************************************************************/

// Internal compressed format

pub fn save_image_into_internal_format(
    image:     &Image,
    file_name: &Path
) -> anyhow::Result<()> {
    assert!(!image.is_empty());

    let mut file = BufWriter::with_capacity(1024 * 256, File::create(file_name)?);
    let mut writer = BitWriter::endian(&mut file, BigEndian);

    if image.is_rgb() {
        let mut r_writer = ValuesCompressor::new();
        let mut g_writer = ValuesCompressor::new();
        let mut b_writer = ValuesCompressor::new();

        for (_, _, r, g, b) in image.iter_rgb_crd() {
            r_writer.write_f32(r, &mut writer)?;
            g_writer.write_f32(g, &mut writer)?;
            b_writer.write_f32(b, &mut writer)?;
        }

        r_writer.flush(&mut writer)?;
        g_writer.flush(&mut writer)?;
        b_writer.flush(&mut writer)?;

        writer.write(32, 0)?;
        writer.flush()?;
    } else {
        let mut l_writer = ValuesCompressor::new();
        for l in image.l.iter() {
            l_writer.write_f32(*l, &mut writer)?;
        }
        l_writer.flush(&mut writer)?;
    }

    Ok(())
}

pub struct InternalFormatReader {
    reader: BitReader<BufReader<File>, BigEndian>,
    r: ValuesDecompressor,
    g: ValuesDecompressor,
    b: ValuesDecompressor,
    l: ValuesDecompressor
}

impl InternalFormatReader {
    pub fn new(file_name: &Path) -> anyhow::Result<InternalFormatReader> {
        let file = BufReader::with_capacity(1024*256, File::open(file_name)?);
        let reader = BitReader::endian(file, BigEndian);
        Ok(InternalFormatReader {
            reader,
            r: ValuesDecompressor::new(),
            g: ValuesDecompressor::new(),
            b: ValuesDecompressor::new(),
            l: ValuesDecompressor::new(),
        })
    }

    pub fn get_rgb(&mut self) -> anyhow::Result<(f32, f32, f32)> {
        Ok((
            self.r.read_f32(&mut self.reader)?,
            self.g.read_f32(&mut self.reader)?,
            self.b.read_f32(&mut self.reader)?,
        ))
    }

    pub fn get_l(&mut self) -> anyhow::Result<f32> {
        Ok(self.l.read_f32(&mut self.reader)?)
    }
}
