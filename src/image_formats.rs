use std::{path::*, io::*, fs::*, sync::atomic::*, sync::Arc};
use serde::*;
use tiff::{*, decoder::*, encoder::*};
use fitrs::*;
use itertools::*;
use bitstream_io::{BigEndian, BitWriter, BitWrite, BitReader};
use chrono::prelude::*;
use crate::{image::*, image_raw::*, fs_utils::*, compression::*, progress::*};

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
        "%Y:%m:%d %H:%M:%S",
        "%Y-%m-%d %H:%M:%S",
        "%Y:%m:%dT%H:%M:%S",
        "%Y-%m-%dT%H:%M:%S",
        "%Y:%m:%d %H:%M:%S%.3f",
        "%Y:%m:%dT%H:%M:%S%.3f",
        "%Y-%m-%d %H:%M:%S%.3f",
        "%Y-%m-%dT%H:%M:%S%.3f",
    ];

    for fmt_string in fmt_strings {
        let result = Local.datetime_from_str(dt_str, fmt_string).ok();
        if result.is_some() {
            return result;
        }
    }

    None
}

pub fn load_image_from_file(file_name: &Path) -> anyhow::Result<ImageData> {
    let ext = extract_extension(file_name);
    if is_raw_ext(ext) {
        load_raw(file_name)
    } else if is_tiff_ext(ext) {
        load_image_from_tiff_file(file_name)
    } else if is_fits_ext(ext) {
        load_image_from_fits_file(file_name)
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

pub fn is_image_file_name(file_name: &Path) -> bool {
    let ext = extract_extension(file_name);
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
    pub exp: Option<f32>,

    /// F-number (4.0 for f/4)
    pub fnumber: Option<f32>,

    /// Focal len in millimeters
    pub focal_len: Option<f32>,

    /// Camera name
    pub camera: Option<String>,

    /// Lens or telescope
    pub lens: Option<String>,
}


pub fn load_src_file_info(file_name: &Path) -> anyhow::Result<ImageInfo> {
    let ext = extract_extension(file_name);
    if is_raw_ext(ext) {
        load_src_file_info_raw(file_name)
    } else if is_tiff_ext(ext) {
        load_src_file_info_tiff(file_name)
    } else if is_fits_ext(ext) {
        load_src_file_info_fits(file_name)
    } else {
        err_format_not_supported(ext)
    }
}

pub fn load_src_file_info_for_files(
    file_names:  &Vec<PathBuf>,
    cancel_flag: &Arc<AtomicBool>,
    progress:    &ProgressTs,
) -> anyhow::Result<Vec<ImageInfo>> {
    let mut result = Vec::new();
    progress.lock().unwrap().stage("Loading short file information...");
    progress.lock().unwrap().set_total(file_names.len());
    for file_name in file_names {
        if cancel_flag.load(Ordering::Relaxed) { break; }
        let item = load_src_file_info(file_name)?;
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
    if let Some(exif) = raw_data.exif {
        iso = exif.get_uint(rawloader::Tag::ISOSpeed);
        exp = exif.get_rational(rawloader::Tag::ExposureTime);
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

    let cfa = Cfa::from_str(
        &raw_data.cfa.name[..],
        raw_data.crops[3] as Crd, // crop_left
        raw_data.crops[0] as Crd  // crop_top
    );

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

fn load_raw(file_name: &Path) -> anyhow::Result<ImageData> {
    let (raw, info) = RawImage::load(file_name)?;
    Ok(ImageData {image: RawOrImage::Raw(raw), info})
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
        file_time = Local.datetime_from_str(&time_str, "%Y:%m:%d %H:%M:%S").ok();
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

fn get_u32_fits_value(h: &Hdu, key: &str) -> Option<u32> {
    h.value(key)
        .and_then(|v| {
            match v {
                HeaderValue::IntegerNumber(v) => Some(*v as u32),
                HeaderValue::RealFloatingNumber(v) => Some(*v as u32),
                _ => None,
            }
        })
}

fn get_f32_fits_value(h: &Hdu, key: &str) -> Option<f32> {
    h.value(key)
        .and_then(|v| {
            match v {
                HeaderValue::IntegerNumber(v) => Some(*v as f32),
                HeaderValue::RealFloatingNumber(v) => Some(*v as f32),
                _ => None,
            }
        })
}

fn get_str_fits_value<'a>(h: &'a Hdu, key: &str) -> Option<&'a str> {
    h.value(key)
        .and_then(|v| {
            match v {
                HeaderValue::CharacterString(v) => Some(v.as_str()),
                _ => None,
            }
        })
}

fn fits_hdu_to_info(file_name: &Path, h: &Hdu) -> ImageInfo {
    let dt_str = get_str_fits_value(&h, "DATE-LOC");
    let exp_time = get_f32_fits_value(&h, "EXPTIME");
    let gain = get_u32_fits_value(&h, "GAIN");
    let bayer = get_str_fits_value(&h, "BAYERPAT");
    let camera = get_str_fits_value(&h, "INSTRUME");
    let focal_len = get_f32_fits_value(&h, "FOCALLEN");
    let focal_ratio = get_f32_fits_value(&h, "FOCRATIO");
    let lens = get_str_fits_value(&h, "TELESCOP");

    dbg!(dt_str, exp_time, gain, bayer, camera);

    let file_time =
        try_to_decode_date_time_str(dt_str.unwrap_or(""))
        .or_else(|| {
            get_file_time(file_name).ok()
        });

    ImageInfo {
        file_name: file_name.to_path_buf(),
        file_time,
        iso: gain,
        exp: exp_time,
        cfa_type: CfaType::from_string(bayer.unwrap_or("")),
        fnumber: focal_ratio,
        focal_len,
        camera: camera.map(ToString::to_string),
        lens: lens.map(ToString::to_string),
        .. Default::default()
    }
}

pub fn load_src_file_info_fits(file_name: &Path) -> anyhow::Result<ImageInfo> {
    let fits = Fits::open(file_name)?;

    for h in fits.iter() {
        let width = h.value("NAXIS1");
        let height = h.value("NAXIS2");
        let depth = h.value("NAXIS3").unwrap_or(&HeaderValue::IntegerNumber(1));

        if let (Some(&HeaderValue::IntegerNumber(width)),
                Some(&HeaderValue::IntegerNumber(height)),
                &HeaderValue::IntegerNumber(depth))
            = (width, height, depth)
        {
            if depth == 1 || depth == 3 {
                return Ok(ImageInfo {
                    width: width as usize,
                    height: height as usize,
                    .. fits_hdu_to_info(file_name, &h)
                })
            }
        }
    }

    anyhow::bail!("This FITS file is not supported")
}

pub fn load_image_from_fits_file(file_name: &Path) -> anyhow::Result<ImageData> {
    let fits = Fits::open(file_name)?;

    let is_mono_shape = |shape: &Vec<usize>| -> bool {
        shape.len() == 2 ||
        (shape.len() == 3 && shape[2] == 1)
    };

    let is_color_shape = |shape: &Vec<usize>| -> bool {
        shape.len() == 3 && shape[2] == 3
    };

    let mut image: Option<Image> = None;
    let mut info: Option<ImageInfo> = None;

    for h in fits.iter() {
        let data = h.read_data();

        match data {
            FitsData::FloatingPoint32(d)
            if is_mono_shape(&d.shape) || is_color_shape(&d.shape) =>
                image = read_fits_data(
                    d.shape[0],
                    d.shape[1],
                    &d.data,
                    !is_mono_shape(&d.shape),
                    |v| v
                ),

            FitsData::FloatingPoint64(d)
            if is_mono_shape(&d.shape) || is_color_shape(&d.shape) =>
                image = read_fits_data(
                    d.shape[0],
                    d.shape[1],
                    &d.data,
                    !is_mono_shape(&d.shape),
                    |v| v as f32
                ),

            FitsData::IntegersI32(d)
            if is_mono_shape(&d.shape) || is_color_shape(&d.shape) =>
                image = read_fits_data_int(
                    d.shape[0],
                    d.shape[1],
                    &d.data,
                    !is_mono_shape(&d.shape),
                    &h,
                    |v| v as f64
                ),

            FitsData::IntegersU32(d)
            if is_mono_shape(&d.shape) || is_color_shape(&d.shape) =>
                image = read_fits_data_int(
                    d.shape[0],
                    d.shape[1],
                    &d.data,
                    !is_mono_shape(&d.shape),
                    &h,
                    |v| v as f64
                ),

            _ => (),
        }

        if image.is_some() {
            info = Some(fits_hdu_to_info(file_name, &h));
            break;
        }
    }

    match (image, info) {
        (Some(image), Some(info)) =>
            Ok(ImageData{
                image: RawOrImage::Image(image),
                info
            }),
        _ =>
            Err(anyhow::anyhow!(
                "Can't find image in file of format is not supported"
            )),
    }
}

fn read_fits_data<T: num::Num + Copy>(
    width:  usize,
    height: usize,
    data:   &[T],
    is_rgb: bool,
    cvt:    fn (T) -> f32
) -> Option<Image> {
    let page_size = width * height;
    let read_page = |dst: &mut ImageLayerF32, src: &[T]| {
        for (d, s) in dst.iter_mut().zip(src) {
            *d = cvt(*s);
        }
    };
    if is_rgb {
        if data.len() != 3 * page_size { return None; }
        let mut image = Image::new_color(width as Crd, height as Crd);
        read_page(&mut image.r, &data[..page_size]);
        read_page(&mut image.g, &data[page_size..2*page_size]);
        read_page(&mut image.b, &data[2*page_size..]);
        Some(image)
    } else {
        if data.len() != page_size { return None; }
        let mut image = Image::new_grey(width as Crd, height as Crd);
        read_page(&mut image.l, data);
        Some(image)
    }
}

fn find_max_value_for_fits(hdu: &Hdu, is_signed: bool) -> Option<f64> {
    hdu
        .value("BITPIX")
        .and_then(|v| {
            if let HeaderValue::IntegerNumber(mut v) = v {
                if is_signed { v -= 1; }
                Some(((1 << v) - 1) as f64)
            } else {
                None
            }
        })
}

trait IntInfo {
    const MAX: Self;
    const IS_SIGNED: bool;
}
impl IntInfo for i32 {
    const MAX: Self = i32::MAX;
    const IS_SIGNED: bool = true;
}
impl IntInfo for u32 {
    const MAX: Self = u32::MAX;
    const IS_SIGNED: bool = false;
}

fn read_fits_data_int<T: Copy + IntInfo + Into::<f64>>(
    width:  usize,
    height: usize,
    data:   &[Option<T>],
    is_rgb: bool,
    hdu:    &Hdu,
    cvt:    fn (T) -> f64
) -> Option<Image> {
    let page_size = width * height;
    let max = find_max_value_for_fits(hdu, T::IS_SIGNED).unwrap_or(T::MAX.into());
    let read_page = |dst: &mut ImageLayerF32, src: &[Option<T>]| {
        for (d, s) in dst.iter_mut().zip(src) {
            if let Some(s) = s {
                *d = (cvt(*s) / max) as f32;
            } else {
                *d = NO_VALUE_F32;
            }
        }
    };
    if is_rgb {
        if data.len() != 3 * page_size { return None; }
        let mut image = Image::new_color(width as Crd, height as Crd);
        read_page(&mut image.r, &data[..page_size]);
        read_page(&mut image.g, &data[page_size..2*page_size]);
        read_page(&mut image.b, &data[2*page_size..]);
        Some(image)
    } else {
        if data.len() != page_size { return None; }
        let mut image = Image::new_grey(width as Crd, height as Crd);
        read_page(&mut image.l, data);
        Some(image)
    }
}

pub fn save_image_to_fits_file(
    image:     &Image,
    _info:     &ImageInfo, // TODO: implement exif support
    file_name: &Path
) -> anyhow::Result<()> {
    assert!(!image.is_empty());
    let data: Vec<_> = if image.is_rgb() {
        image.r
            .iter()
            .chain(image.g.iter())
            .chain(image.b.iter())
            .copied()
            .collect()
    } else {
        image.l.iter()
            .copied()
            .collect()
    };
    let dims = [
        image.width() as usize,
        image.height() as usize,
        if image.is_rgb() { 3 } else { 1 }
    ];
    let prim_hdu = Hdu::new(&dims, data);
    Fits::create(file_name, prim_hdu)?;
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
