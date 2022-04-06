use std::{path::*, io::*, fs::*};
use tiff::encoder::*;
use fitrs::*;
use itertools::*;
use bitstream_io::{BigEndian, BitWriter, BitWrite, BitReader};
use crate::{image::*, fs_utils::*, compression::*};

pub fn load_image_from_file(file_name: &PathBuf) -> anyhow::Result<SrcImageData> {
    let ext = extract_extension(file_name);
    if is_tiff_ext(ext) {
        load_image_from_tiff_file(file_name)
    } else if is_fits_ext(ext) {
        load_image_from_fits_file(file_name)
    } else {
        err_format_not_supported(ext)
    }
}

pub fn save_image_to_file(image: &Image, exif: &Exif, file_name: &PathBuf) -> anyhow::Result<()> {
    let ext = extract_extension(file_name);
    if is_tiff_ext(ext) {
        save_image_to_tiff_file(image, exif, file_name)
    } else if is_fits_ext(ext) {
        save_image_to_fits_file(image, exif, file_name)
    } else {
        err_format_not_supported(ext)
    }
}

pub fn is_tiff_ext(ext: &str) -> bool {
    ext.eq_ignore_ascii_case("tiff") |
    ext.eq_ignore_ascii_case("tif")
}

pub fn is_fits_ext(ext: &str) -> bool {
    ext.eq_ignore_ascii_case("fit") |
    ext.eq_ignore_ascii_case("fits") |
    ext.eq_ignore_ascii_case("fts")
}

pub fn is_image_file_name(file_name: &PathBuf) -> bool {
    let ext = extract_extension(file_name);
    is_tiff_ext(ext) |
    is_fits_ext(ext)
}

pub struct SrcImageData {
    pub image: Image,
    pub exif:  Exif,
}

fn err_format_not_supported<R>(ext: &str) -> anyhow::Result<R> {
    Err(anyhow::anyhow!("Image format `{}` is not supported", ext))
}

/*****************************************************************************/

// TIFF format

pub fn load_image_from_tiff_file(file_name: &PathBuf) -> anyhow::Result<SrcImageData> {
    use tiff::{*, decoder::*};

    fn assign_img_data<S: Copy>(
        src:    &Vec<S>,
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

    Ok(SrcImageData{
        image,
        exif: Exif::new_empty(),
    })
}

pub fn save_image_to_tiff_file(
    image:     &Image,
    exif:      &Exif,
    file_name: &PathBuf
) -> anyhow::Result<()> {
    if image.is_empty() { anyhow::bail!("Image is empty"); }

    let mut file = BufWriter::new(File::create(file_name)?);
    let mut decoder = TiffEncoder::new(&mut file)?;

    if image.is_greyscale() {
        let mut tiff = decoder.new_image::<colortype::Gray32Float>(
            image.width() as u32,
            image.height() as u32
        )?;

        write_exif_into_tiff(tiff.encoder(), exif)?;
        tiff.write_data(image.l.iter().as_slice())?;
    }
    else if image.is_rgb() {
        let mut data = Vec::with_capacity((3 * image.width() * image.height()) as usize);
        for (r, g, b) in izip!(image.r.iter(), image.g.iter(), image.b.iter()) {
            data.push(*r);
            data.push(*g);
            data.push(*b);
        }

        let mut tiff = decoder.new_image::<colortype::RGB32Float>(
            image.width() as u32,
            image.height() as u32
        )?;

        write_exif_into_tiff(tiff.encoder(), exif)?;
        tiff.write_data(&data)?;
    } else {
        panic!("Internal error");
    }

    Ok(())
}

fn write_exif_into_tiff<W: Write + Seek, K: TiffKind>(
    enc:  &mut tiff::encoder::DirectoryEncoder<W, K>,
    exif: &Exif
) -> anyhow::Result<()> {
    use tiff::tags::*;

    enc.write_tag(Tag::Software, "astro_utils (https://github.com/art-den/astro_utils)")?;
    if let Some(exp_time) = exif.exp_time {
        enc.write_tag(Tag::Unknown(0x829a), &[exp_time as u32, 1_u32][..])?;
    }
    Ok(())
}

/*****************************************************************************/

// FITS format

pub fn load_image_from_fits_file(file_name: &PathBuf) -> anyhow::Result<SrcImageData> {
    let fits = Fits::open(file_name)?;

    let is_mono_shape = |shape: &Vec<usize>| -> bool {
        shape.len() == 2 ||
        (shape.len() == 3 && shape[2] == 1)
    };

    let is_color_shape = |shape: &Vec<usize>| -> bool {
        shape.len() == 3 && shape[2] == 3
    };

    let mut image: Option<Image> = None;

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

        if image.is_some() { break; }
    }

    match image {
        Some(image) =>
            Ok(SrcImageData{
                image,
                exif: Exif::new_empty(),
            }),
        None =>
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
        read_page(&mut image.l, &data);
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
        read_page(&mut image.l, &data);
        Some(image)
    }
}


pub fn save_image_to_fits_file(
    image:     &Image,
    _exif:     &Exif, // TODO: implement exif support
    file_name: &PathBuf
) -> anyhow::Result<()> {
    let pages = if image.is_rgb() { 3 } else { 1 };
    let mut data = Vec::new();
    if image.is_rgb() {
        data.reserve((3 * image.width() * image.height()) as usize);
        for v in image.r.iter() { data.push(*v); }
        for v in image.g.iter() { data.push(*v); }
        for v in image.b.iter() { data.push(*v); }
    } else {
        data.reserve((image.width() * image.height()) as usize);
        for v in image.l.iter() { data.push(*v); }
    }
    let dims = [image.width() as usize, image.height() as usize, pages as usize];
    let prim_hdu = Hdu::new(&dims, data);
    Fits::create(file_name, prim_hdu)?;
    Ok(())
}

/*****************************************************************************/

// Internal compressed format

pub fn save_image_into_internal_format(
    image:     &Image,
    file_name: &PathBuf
) -> anyhow::Result<()> {
    if image.is_empty() { anyhow::bail!("Image is empty"); }

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
    pub fn new(file_name: &PathBuf) -> anyhow::Result<InternalFormatReader> {
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
