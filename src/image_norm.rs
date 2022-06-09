use std::{collections::HashMap, path::*};
use crate::{image::*, image_raw::*, calc::*, light_file::*, stars::*, log_utils::*};

pub struct NormResult {
    pub range_factor: f32,
}

fn mask_stars(mask: &mut ImageMask, width: Crd, height: Crd, stars: &Stars) {
        mask.resize_and_clear(width, height);
        for star in stars.iter() {
            for pt in star.points.iter() {
                let rad = Crd::max(star.radius as Crd, 1);
                for dx in -rad..=rad { for dy in -rad..=rad {
                    mask.set_safe(pt.x + dx, pt.y + dy, true);
                }}
            }
        }
    }

pub fn normalize(
    ref_data:   &RefBgData,
    light_file: &mut LightFile
) -> anyhow::Result<NormResult> {
    let mut grey_image = light_file.grey.clone();
    let mut mask = ImageMask::new_empty();

    mask_stars(
        &mut mask,
        ref_data.image.image.width(),
        ref_data.image.image.height(),
        &ref_data.image.stars
    );

    for (m, v) in mask.iter_mut().zip(grey_image.iter()) {
        if v.is_infinite() || *v == NO_VALUE_F32 { *m = true; }
    }

    for (m, v) in mask.iter_mut().zip(ref_data.grey.iter()) {
        if v.is_infinite() || *v == NO_VALUE_F32 { *m = true; }
    }

    let calc_log = TimeLogger::start();
    let image_bg = calc_image_bg(&light_file.image, &mask)?;
    calc_log.log("calc bg");

    let gs_bg = calc_image_layer_bg(&grey_image, &mask)?;
    gs_bg.apply_to_image(&mut grey_image, true)?;

    let range_log = TimeLogger::start();
    let range = calc_range(&ref_data.grey, &grey_image, &mask);
    range_log.log("range calc");
    log::info!("range = {:.3}", range);

    let appl_log = TimeLogger::start();
    image_bg.apply_to_image(&mut light_file.image, true)?; // remove image bg
    light_file.image.mult_f32(range as f32); // scale image to ref. range
    ref_data.bg.apply_to_image(&mut light_file.image, false)?; // apply ref. image bg
    appl_log.log("apply bg to image");

    Ok(NormResult { range_factor: range as f32 })
}

fn calc_range(ref_img: &ImageLayerF32, image: &ImageLayerF32, mask: &ImageMask) -> f64 {
    const SIZE: Crd = 16;
    struct RangeArea {
        range: f64,
        ref_value: f64,
        img_value: f64,
    }
    let mut ref_values = Vec::new();
    let mut img_values = Vec::new();
    let mut ranges = Vec::new();
    for (_, _, area)
    in RectAreaIterator::new(ref_img.width(), ref_img.width()/SIZE, ref_img.height(), ref_img.height()/SIZE) {
        ref_values.clear();
        img_values.clear();
        for ((.., v), (.., m)) in ref_img.iter_area_crd(&area).zip(mask.iter_area_crd(&area)) {
            if !m && !v.is_infinite() { ref_values.push(v as f64); }
        }
        for ((.., v), (.., m)) in image.iter_area_crd(&area).zip(mask.iter_area_crd(&area)) {
            if !m && !v.is_infinite() { img_values.push(v as f64); }
        }
        if ref_values.len() < (SIZE*SIZE/2) as usize { continue; }

        let ref_value = mean_f64(&ref_values);
        let img_value = mean_f64(&img_values);

        let bord1 = ref_values.len()/8;
        let bord2 = ref_values.len() - ref_values.len()/8;
        ref_values.select_nth_unstable_by(bord1, cmp_f64);
        ref_values[bord1..].select_nth_unstable_by(bord2-bord1, cmp_f64);
        let ref_diff = mean_f64(&ref_values[bord2..]) - mean_f64(&ref_values[..bord1]);
        img_values.select_nth_unstable_by(bord1, cmp_f64);
        img_values[bord1..].select_nth_unstable_by(bord2-bord1, cmp_f64);
        let img_diff = mean_f64(&img_values[bord2..]) - mean_f64(&img_values[..bord1]);
        if ref_diff != 0.0 {
            let range = img_diff / ref_diff;
            ranges.push(RangeArea { range, ref_value, img_value });
        }
    }

    // skip 1/8 of ranges with low RangeArea::range
    // and 1/8 of ranges with high RangeArea::range
    let border1 = ranges.len()/8;
    let border2 = 7*ranges.len()/8;

    ranges.select_nth_unstable_by(border1, |r1, r2| cmp_f64(&r1.range, &r2.range));
    ranges[border1..].select_nth_unstable_by(border2-border1, |r1, r2| cmp_f64(&r1.range, &r2.range));

    let ref_values: Vec<_> = ranges[border1..border2].iter().map(|r| CalcValue::new(r.ref_value)).collect();
    let img_values: Vec<_> = ranges[border1..border2].iter().map(|r| CalcValue::new(r.img_value)).collect();

    let (ref_mean, ref_dev) = mean_and_std_dev(&ref_values).unwrap_or((1.0, 1.0));
    let (img_mean, img_dev) = mean_and_std_dev(&img_values).unwrap_or((1.0, 1.0));

    log::info!("ref_mean = {}, ref_dev = {}", ref_mean, ref_dev);
    log::info!("img_mean = {}, img_dev = {}", img_mean, img_dev);

    if img_dev != 0.0 {
        ref_dev / img_dev
    } else {
        1.0
    }
}

pub fn calc_image_bg(
    image: &Image,
    mask: &ImageMask
) -> anyhow::Result<ImageBg> {
    let l =
        if image.l.is_empty() { None }
        else { Some(calc_image_layer_bg(&image.l, mask)?) };
    let r =
        if image.r.is_empty() { None }
        else { Some(calc_image_layer_bg(&image.r, mask)?) };
    let g =
        if image.g.is_empty() { None }
        else { Some(calc_image_layer_bg(&image.g, mask)?) };
    let b =
        if image.b.is_empty() { None }
        else { Some(calc_image_layer_bg(&image.b, mask)?) };
    Ok(ImageBg { l, r, g, b })
}

pub fn calc_image_layer_bg(image: &ImageLayerF32, mask: &ImageMask) -> anyhow::Result<BgCols> {
    let mut values = Vec::new();
    let mut bg_values = HashMap::<i64, Vec::<(f64, i64, i64, usize)>>::new();
    let mut cnt_values = Vec::new();
    let mut x_values = Vec::new();
    let mut y_values = Vec::new();
    for (i, _j, area) in RectAreaIterator::new(image.width(), 16, image.height(), 16) {
        values.clear();
        let mut y_sum: i64 = 0;
        let mut x_sum: i64 = 0;

        if mask.is_empty() {
            for (x, y, v) in image.iter_area_crd(&area) {
                if v.is_infinite() { continue; }
                values.push(v as f64);
                x_sum += x as i64;
                y_sum += y as i64;
            }
        } else {
            for ((x, y, v), (.., m)) in image.iter_area_crd(&area).zip(mask.iter_area_crd(&area)) {
                if m || v.is_infinite() { continue; }
                values.push(v as f64);
                x_sum += x as i64;
                y_sum += y as i64;
            }
            if values.len() < 256 { continue; }
        }

        let bord1 = values.len()/8;
        let bord2 = values.len() - values.len()/8;

        values.select_nth_unstable_by(bord1, cmp_f64);
        values[bord1..].select_nth_unstable_by(bord2-bord1, cmp_f64);

        let value = mean_f64(&values[bord1..bord2]);
        let x = x_sum / values.len() as i64;
        let y = y_sum / values.len() as i64;
        match bg_values.get_mut(&i) {
            Some(v) => { v.push((value, x, y, values.len())); },
            None    => { bg_values.insert(i, vec![(value, x, y, values.len())]); },
        };
    }
    let mut cols = Vec::new();
    for col_items in bg_values.values() {
        if col_items.len() < 3 { continue; }
        cnt_values.clear();
        for (.., len) in col_items.iter() { cnt_values.push(*len as f64); }
        let median_len = median_f64(&mut cnt_values).unwrap_or(0.0) as usize;
        if median_len == 0 { continue; }
        let len_border = 2 * median_len / 3;
        x_values.clear();
        y_values.clear();
        let mut x_sum: i64 = 0;
        for (value, x, y, len) in col_items.iter() {
            if *len < len_border { continue; }
            x_values.push(*y as f64);
            y_values.push(*value);
            x_sum += x;
        }
        if x_values.len() < 3 { continue; }

        let x = x_sum / x_values.len() as i64;
        let col_coeffs = square_ls(&x_values, &y_values).ok_or_else(|| anyhow::anyhow!("cubic_ls"))?;
        cols.push(BgCol { coeffs: col_coeffs, x: x as f64 });
    }

    Ok(BgCols::new(cols))
}

struct BgCol {
    coeffs: SquareCoeffs,
    x: f64,
}

pub struct BgCols {
    cols: Vec<BgCol>,
}

impl BgCols {
    fn new(cols: Vec<BgCol>) -> BgCols {
        BgCols { cols }
    }

    pub fn apply_to_image(&self, img: &mut ImageLayerF32, neg: bool) -> anyhow::Result<()> {
        let mut x_values = Vec::new();
        let mut y_values = Vec::new();
        for y in 0..img.height() {
            x_values.clear();
            y_values.clear();
            for col in self.cols.iter() {
                x_values.push(col.x);
                y_values.push(col.coeffs.calc(y as f64));
            }
            let coeffs = square_ls(&x_values, &y_values)
                .ok_or_else(|| anyhow::anyhow!("cubic_ls"))?;
            for (x, v) in img.iter_row_mut(y).enumerate() {
                let bg = coeffs.calc(x as f64) as f32;
                if *v == NO_VALUE_F32 { continue; }
                if neg { *v -= bg; } else { *v += bg; }
            }
        }
        Ok(())
    }
}

pub struct ImageBg {
    l: Option<BgCols>,
    r: Option<BgCols>,
    g: Option<BgCols>,
    b: Option<BgCols>,
}

impl ImageBg {
    pub fn apply_to_image(&self, image: &mut Image, neg: bool) -> anyhow::Result<()> {
        let try_apply_layer = |cols: &Option<BgCols>, layer: &mut ImageLayerF32| -> anyhow::Result<()> {
            match cols {
                Some(cols) if !layer.is_empty() => {
                    cols.apply_to_image(layer, neg)?;
                    Ok(())
                },
                None if layer.is_empty() => {
                    Ok(())
                },
                _ =>
                    anyhow::bail!("Internal error"),
            }
        };

        try_apply_layer(&self.l, &mut image.l)?;
        try_apply_layer(&self.r, &mut image.r)?;
        try_apply_layer(&self.g, &mut image.g)?;
        try_apply_layer(&self.b, &mut image.b)?;

        Ok(())
    }
}

pub struct RefBgData {
    pub image: LightFile,
    pub grey:  ImageLayerF32, // greyscale image minus background
    pub bg:    ImageBg,
}

impl RefBgData {
    pub fn new(
        ref_file_name: &Path,
        cal_data:      &CalibrationData,
        bin:           usize
    ) -> anyhow::Result<RefBgData> {
        let image = LightFile::load_and_calc_params(
            ref_file_name,
            cal_data,
            None,
            LoadLightFlags::STARS,
            bin
        )?;

        let mut mask = ImageMask::new_empty();
        mask_stars(&mut mask, image.image.width(), image.image.height(), &image.stars);

        let mut grey = image.grey.clone();
        let grey_bg = calc_image_layer_bg(&grey, &mask)?;
        grey_bg.apply_to_image(&mut grey, true)?;

        let bg = calc_image_bg(&image.image, &mask)?;

        Ok(RefBgData { image, grey, bg })
    }
}
