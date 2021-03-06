use std::collections::HashSet;
use itertools::*;
use crate::{image::*, calc::*};
use std::f64::consts::PI;

pub const MAX_STAR_DIAMETER: Crd = 25; // in pixels
pub const STAR_BG_BORDER: f32 = 0.001;

pub struct StarPoint {
    pub x: Crd,
    pub y: Crd,
}

type StarPoints = Vec<StarPoint>;

pub struct Star {
    pub x:              f64,
    pub y:              f64,
    pub background:     f32,
    pub max_value:      f32,
    pub width:          Crd,
    pub height:         Crd,
    pub brightness:     f64,
    pub radius:         f32,
    pub radius_std_dev: f32,
    pub overexposured:  bool,
    pub points:         StarPoints,
}

impl Star {
    fn dist(&self, other: &Star) -> f64 {
        let dist_x = self.x - other.x;
        let dist_y = self.y - other.y;
        (dist_x * dist_x + dist_y * dist_y).sqrt()
    }
}

pub type Stars = Vec<Star>;

pub fn find_stars_on_image(
    img:                &ImageLayerF32,
    orig_image:         Option<&Image>,
    noise:              Option<f32>,
    remove_wrong_stars: bool,
) -> anyhow::Result<Stars> {
    let border = match noise {
        Some(noise)
        if noise != 0.0 =>
            (noise * 20.0).max(STAR_BG_BORDER),
        _ =>
            STAR_BG_BORDER,
    };

    let mut stars = Stars::new();

    // Find possible stars centers

    let mut filtered: Vec<f32> = Vec::new();
    let mut possible_stars = Vec::new();
    let mut heavy_filter = IirFilter::new_gauss(42.0);
    let mut heavy_filtered: Vec<f32> = Vec::new();
    heavy_filtered.resize(img.width() as usize, 0.0);
    img.foreach_row_and_col(IterType::Rows, |values, y, _| {
        filtered.clear();
        filtered.push(values[0]);
        for (&i1, &i2, &i3) in values.iter().tuple_windows() {
            filtered.push(0.333333 * (i1 + i2 + i3));
        }
        filtered.push(values[values.len()-1]);
        iir_filter(&mut heavy_filter, values, &mut heavy_filtered);
        for (i, (&prev, &cur, &next)) in filtered.iter().tuple_windows().enumerate() {
            let index = i + 1;
            if prev < cur && cur > next && cur > heavy_filtered[index]+border {
                let i1 = i32::max(0, index as i32 - MAX_STAR_DIAMETER as i32/2) as usize;
                let i2 = i32::min(index as i32 + MAX_STAR_DIAMETER as i32/2, img.width() as i32) as usize;
                let min = filtered[i1..i2].iter().copied().min_by(cmp_f32);
                if let Some(min) = min { if cur > min + border {
                    possible_stars.push((i as Crd + 1, y, cur));
                }}
            }
        }
    });

    // Sort possible star centers by brightness. Larger is first

    possible_stars.sort_by(|s1, s2| cmp_f32(&s1.2, &s2.2).reverse());

    type TmpPt = (Crd, Crd);
    let mut all_stars_points: HashSet<TmpPt> = HashSet::new();
    let mut star_bg_values = Vec::new();
    let mut flood_filler = FloodFiller::new();

    for &(mut x, mut y, posibble_max_value) in possible_stars.iter() {
        if all_stars_points.contains(&(x, y)) { continue };

        // goto maximum
        loop {
            let try_maximum = |x, y, dx, dy| -> bool {
                let cur_value = img.get(x, y).unwrap_or(0.0);
                if all_stars_points.contains(&(x+dx, y+dy)) {
                    return false;
                }
                let new_value = img.get(x+dx, y+dy).unwrap_or(0.0);
                new_value > cur_value
            };

            let mut found = false;
            'outer: for dy in -1..=1 { for dx in -1..=1 {
                if try_maximum(x, y, dx, dy) {
                    x += dx;
                    y += dy;
                    found = true;
                    break 'outer;
                }
            }}

            if !found { break; }
        }

        // calculate star background

        use MAX_STAR_DIAMETER as MSD;
        star_bg_values.clear();
        for (_, _, v) in img.iter_rect_crd(x-MSD/2, y-MSD/2, x+MSD/2, y+MSD/2) {
            star_bg_values.push(v);
        }
        let star_bg_index = star_bg_values.len() / 4;
        let star_bg = *star_bg_values.select_nth_unstable_by(star_bg_index, cmp_f32).1;

        if (posibble_max_value - star_bg) < border { continue };

        // Find all points of star. Border is as 1/2 of brightness of star center
        let border_value = (posibble_max_value + star_bg) / 2.0;
        let mut star_points = HashSet::new();

        flood_filler.fill(
            x, y,
            |x, y| {
                if star_points.len() as i64 > MSD*MSD { return false; }
                if all_stars_points.contains(&(x, y)) { return false; }
                if star_points.contains(&(x, y)) { return false; }
                if let Some(v) = img.get(x, y) { if v < border_value {
                    return false;
                }} else {
                    return false;
                }
                star_points.insert((x, y));
                true
            }
        );

        // test that found object is star

        if star_points.len() as i64 > MSD*MSD { continue; }

        let (center_x, center_y, br, radius, radius_dev) = calc_center_brightness_radius_and_deviation(
            &img,
            star_bg,
            &star_points
        );

        if radius_dev > 2.0 { continue; }

        let mut star_is_overexposured = false;
        if let Some(orig_image) = orig_image {
            for &(sx, sy) in &star_points {
                if let Some(v) = orig_image.l.get(sx, sy) { if v.is_infinite() {
                    star_is_overexposured = true;
                }}
                if let Some(v) = orig_image.r.get(sx, sy) { if v.is_infinite() {
                    star_is_overexposured = true;
                }}
                if let Some(v) = orig_image.g.get(sx, sy) { if v.is_infinite() {
                    star_is_overexposured = true;
                }}
                if let Some(v) = orig_image.b.get(sx, sy) { if v.is_infinite() {
                    star_is_overexposured = true;
                }}
                if star_is_overexposured { break; }
            }
        }

        // add star into result list

        for &p in star_points.iter() { all_stars_points.insert(p); };

        let min_x = star_points.iter().map(|p| p.0).min().unwrap_or(x);
        let max_x = star_points.iter().map(|p| p.0).max().unwrap_or(x);
        let min_y = star_points.iter().map(|p| p.1).min().unwrap_or(y);
        let max_y = star_points.iter().map(|p| p.1).max().unwrap_or(y);

        let points: StarPoints = star_points
            .iter()
            .map(|pt| StarPoint { x: pt.0, y: pt.1, })
            .collect();

        let max_star_value = points
            .iter()
            .map(|pt| img.get(pt.x, pt.y).unwrap_or(0.0))
            .max_by(cmp_f32)
            .unwrap_or(0.0);

        stars.push(Star {
            x:              center_x,
            y:              center_y,
            background:     star_bg,
            max_value:      max_star_value,
            width:          max_x - min_x + 1,
            height:         max_y - min_y + 1,
            brightness:     br,
            radius:         radius as f32,
            radius_std_dev: radius_dev as f32,
            overexposured:  star_is_overexposured,
            points,
        });
    }

    if remove_wrong_stars {
        // remove strange stars by radius
        stars.retain(|s| {
            let by_area_r = f64::sqrt(s.points.len() as f64 / PI);
            let ratio = by_area_r / s.radius as f64;
            ratio < 1.2 && ratio > 0.8
        });

        // remove strange stars by radius deviation
        let mut star_r_devs = Vec::new();
        for _ in 0..10 {
            star_r_devs.clear();
            for star in &stars {
                star_r_devs.push(CalcValue::new(star.radius_std_dev as f64));
            }
            if let Some((mean, dev)) = mean_and_std_dev(&star_r_devs) {
                let max = mean + dev*2.5;
                stars.retain(|s| (s.radius_std_dev as f64) < max);
            } else {
                anyhow::bail!("No stars");
            }
        }

        // remove strange stars by area
        for _ in 0..10 {
            star_r_devs.clear();
            for star in &stars {
                star_r_devs.push(CalcValue::new((star.points.len() as f64).sqrt()));
            }
            if let Some((mean, dev)) = mean_and_std_dev(&star_r_devs) {
                let max = mean + dev * 8.0;
                stars.retain(|s| (s.points.len() as f64).sqrt() < max);
            } else {
                anyhow::bail!("No stars");
            }
        }

    }

    stars.sort_by(|s1, s2| cmp_f64(&s1.brightness, &s2.brightness).reverse());
    // common star

    Ok(stars)
}

fn calc_center_brightness_radius_and_deviation(
    img:         &ImageLayerF32,
    star_bg:     f32,
    star_points: &HashSet<(Crd, Crd)>
) -> (f64, f64, f64, f64, f64) {
    const BORD: f64 = 0.5;

    // center of star and brightness

    let (x_sum, y_sum, br) = star_points.iter().fold(
        (0_f64, 0_f64, 0_f64),
        |(x_sum, y_sum, br), &(x, y)| {
            let v = (img.get(x, y).unwrap_or(0.0) - star_bg) as f64;
            (x_sum + v * x as f64, y_sum + v * y as f64, br + v)
        }
    );

    let center_x = x_sum/br;
    let center_y = y_sum/br;

    // radius

    let is_border_point = |x, y| {
        !star_points.contains(&(x+1, y)) ||
        !star_points.contains(&(x-1, y)) ||
        !star_points.contains(&(x, y+1)) ||
        !star_points.contains(&(x, y-1))
    };

    let mut radius_summ = 0_f64;
    let mut bord_pt_cnt = 0_u32;
    for &(sx, sy) in star_points.iter().filter(|(x, y)| is_border_point(*x, *y) ) {
        let dx = sx as f64 - center_x;
        let dy = sy as f64 - center_y;
        let r = (dx*dx + dy*dy).sqrt() + BORD;
        radius_summ += r;
        bord_pt_cnt += 1;
    }

    let radius = radius_summ / bord_pt_cnt as f64;

    // radius devation

    let mut deviation_summ = 0_f64;
    for &(sx, sy) in star_points.iter().filter(|(x, y)| is_border_point(*x, *y) ) {
        let dx = sx as f64 - center_x;
        let dy = sy as f64 - center_y;
        let r = (dx*dx + dy*dy).sqrt() + BORD;
        let diff = (r - radius).abs();
        let diff = if diff < BORD { 0.0 } else { diff - BORD };
        deviation_summ += diff * diff;
    }

    let radius_dev = (deviation_summ / bord_pt_cnt as f64).sqrt() / radius;

    (center_x, center_y, br, radius, radius_dev)
}

#[derive(Debug)]
pub struct ImageOffset {
    pub offset_x: f64,
    pub offset_y: f64,
    pub angle:    f64,
}

pub fn calc_image_offset_by_stars(
    ref_stars:  &Stars,
    stars:      &Stars,
    img_width:  f64,
    img_height: f64
) -> Option<ImageOffset> {
    let image_size = (img_width + img_height) / 2.0;

    const MAX_STARS: usize = 50;
    let ref_stars = if ref_stars.len() < MAX_STARS { ref_stars } else { &ref_stars[..MAX_STARS] };
    let stars     = if stars.len()     < MAX_STARS { stars }     else { &stars[..MAX_STARS] };

    let ref_triangles = get_stars_triangles(ref_stars, image_size / 5.0);
    let triangles = get_stars_triangles(stars, image_size / 5.0);

    log::info!("Align parameters calculation");
    log::info!("ref_triangles.len() = {}", ref_triangles.len());
    log::info!("triangles.len() = {}", triangles.len());

    struct Corr<'a> {
        ref_triangle: &'a StarsTriangle<'a>,
        triangle: StarsTriangle<'a>,
        angle: f64,
        len: f64,
        used: bool,
    }

    let mut corr_items = Vec::new();
    for ref_tri in ref_triangles.iter() {
        if let Some(triangle) = find_same_triangle(&triangles, ref_tri) {
            corr_items.push(Corr {
                ref_triangle: ref_tri,
                len: triangle.len,
                angle: ref_tri.calc_angle(&triangle),
                triangle,
                used: true,
            });
        }
    }

    log::info!("corr_items.len() = {}", corr_items.len());

    let mut angles = Vec::new();
    let mut x_offsets = Vec::new();
    let mut y_offsets = Vec::new();

    let mut angle = 0_f64;
    let mut offset_x = 0_f64;
    let mut offset_y = 0_f64;

    for _ in 0..30 {
        angles.clear();
        for item in corr_items.iter() {
            angles.push(CalcValue::new_weighted(item.angle, item.len));
        }
        angle = cappa_sigma_weighted_result(&mut angles, 2.0, 10, true, true)?.result;

        for (i, a) in angles.iter().enumerate() {
            if !a.used { corr_items[i].used = false; }
        }

        let size_before_retain = corr_items.len();
        corr_items.retain(|v| v.used);

        let center_x = (img_width - 1.0) / 2.0;
        let center_y = (img_height - 1.0) / 2.0;
        x_offsets.clear();
        y_offsets.clear();
        for item in corr_items.iter() {
            let ref_stars = item.ref_triangle.stars;
            let ref_tri_center_x = (ref_stars[0].x + ref_stars[1].x + ref_stars[2].x) / 3.0;
            let ref_tri_center_y = (ref_stars[0].y + ref_stars[1].y + ref_stars[2].y) / 3.0;
            let stars = item.triangle.stars;
            let x = (stars[0].x + stars[1].x + stars[2].x) / 3.0;
            let y = (stars[0].y + stars[1].y + stars[2].y) / 3.0;
            let (on_ref_x, on_ref_y) = rotate_point(x, y, center_x, center_y, -angle);
            x_offsets.push(CalcValue::new(on_ref_x - ref_tri_center_x));
            y_offsets.push(CalcValue::new(on_ref_y - ref_tri_center_y));
        }

        offset_x = cappa_sigma_weighted_result(&mut x_offsets, 2.0, 10, true, true)?.result;
        offset_y = cappa_sigma_weighted_result(&mut y_offsets, 2.0, 10, true, true)?.result;

        for (i, (x, y)) in x_offsets.iter().zip(y_offsets.iter()).enumerate() {
            if !x.used || !y.used { corr_items[i].used = false; }
        }
        corr_items.retain(|v| v.used);

        if size_before_retain == corr_items.len() {
            break;
        }
    }

    log::info!("(used for calculatiuon) corr_items.len() = {}", corr_items.len());

    Some(ImageOffset { angle, offset_x, offset_y, })
}

#[derive(Clone)]
struct StarsTriangle<'a> {
    stars: [&'a Star; 3],
    len:   f64,
}

fn correct_angle(angle: f64) -> f64 {
    if      angle < -PI { angle + 2.0 * PI }
    else if angle > PI  { angle - 2.0 * PI }
    else                { angle }
}


impl<'a> StarsTriangle<'a> {
    fn new(star1: &'a Star, star2: &'a Star, star3: &'a Star) -> StarsTriangle<'a> {
        let x = (star1.x + star2.x + star3.x) / 3.0;
        let y = (star1.y + star2.y + star3.y) / 3.0;
        let a1 = f64::atan2(star1.y - y, star1.x - x);
        let a2 = f64::atan2(star2.y - y, star2.x - x);
        let a3 = f64::atan2(star3.y - y, star3.x - x);
        let mut sorted = [ (a1, star1), (a2, star2), (a3, star3), ];
        sorted.sort_by(|s1, s2| cmp_f64(&s1.0, &s2.0));

        StarsTriangle {
            stars: [sorted[0].1, sorted[1].1, sorted[2].1 ],
            len:   star1.dist(star2) + star2.dist(star3) + star3.dist(star1)
        }
    }

    fn compare(&self, other: &StarsTriangle) -> (f64, [usize; 3]) {
        let cmp_fun = |i1: usize, i2: usize, i3: usize| {
            let s1 = self.stars[i1];
            let s2 = self.stars[i2];
            let s3 = self.stars[i3];
            let o1 = other.stars[0];
            let o2 = other.stars[1];
            let o3 = other.stars[2];
            let diff1 = s1.dist(s2) - o1.dist(o2);
            let diff2 = s2.dist(s3) - o2.dist(o3);
            let diff3 = s3.dist(s1) - o3.dist(o1);
            diff1 * diff1 + diff2 * diff2 + diff3 * diff3
        };

        let e1 = cmp_fun(0, 1, 2);
        let e2 = cmp_fun(1, 2, 0);
        let e3 = cmp_fun(2, 0, 1);

        if      e1 < e2 && e1 < e3 { (e1, [0, 1, 2]) }
        else if e2 < e1 && e2 < e3 { (e2, [1, 2, 0]) }
        else                       { (e3, [2, 0, 1]) }
    }

    fn calc_angle(&self, other: &StarsTriangle) -> f64 {
        let calc_angle = |s1: usize, s2: usize| -> f64 {
            let self_star1 = self.stars[s1];
            let self_star2 = self.stars[s2];
            let other_star1 = other.stars[s1];
            let other_star2 = other.stars[s2];
            let self_angle = correct_angle(f64::atan2(self_star2.y - self_star1.y, self_star2.x - self_star1.x));
            let other_angle = correct_angle(f64::atan2(other_star2.y - other_star1.y, other_star2.x - other_star1.x));
            correct_angle(other_angle - self_angle)
        };

        let a1 = calc_angle(0, 1);
        let a2 = calc_angle(1, 2);
        let a3 = calc_angle(2, 0);

        (a1 + a2 + a3) / 3.0
    }
}

type StarsTriangles<'a> = Vec<StarsTriangle<'a>>;

fn get_stars_triangles(stars: &[Star], min_len: f64) -> StarsTriangles {
    let mut result = StarsTriangles::new();
    for i in 0..stars.len() { for j in i+1..stars.len() { for k in j+1..stars.len() {
        let tr = StarsTriangle::new(&stars[i], &stars[j], &stars[k]);
        if tr.len > min_len { result.push(tr); }
    }}}
    result.sort_by(|t1, t2| cmp_f64(&t1.len, &t2.len));
    result
}

fn find_same_triangle<'a>(
    triangles: &'a StarsTriangles,
    triangle:  &StarsTriangle
) -> Option<StarsTriangle<'a>> {
    const ERR_RANGE: f64 = 3.0;

    let lower_res = triangles.binary_search_by(|t| cmp_f64(&t.len, &(triangle.len-ERR_RANGE)));
    let upper_res = triangles.binary_search_by(|t| cmp_f64(&t.len, &(triangle.len+ERR_RANGE)));

    let lower_index = match lower_res { Ok(v) => v, Err(v) => v };
    let upper_index = match upper_res { Ok(v) => v, Err(v) => v };

    let mut cmp_res: Option<((f64, [usize; 3]), &StarsTriangle)> = None;
    for test_triangle in &triangles[lower_index..upper_index] {
        let (test_len_diff, ref test_ind) = test_triangle.compare(triangle);
        if let Some(((len_diff, ind), tr)) = &mut cmp_res { if test_len_diff < *len_diff {
            *len_diff = test_len_diff;
            *ind = *test_ind;
            *tr = test_triangle;
        }} else {
            cmp_res = Some(((test_len_diff, *test_ind), test_triangle));
        }
    }

    if let Some(cmp_res) = cmp_res {
        let ((len_diff, ref res_ind), res_tr) = cmp_res;
        if len_diff > ERR_RANGE {
            return None;
        }
        Some(StarsTriangle {
            stars: [ res_tr.stars[res_ind[0]], res_tr.stars[res_ind[1]], res_tr.stars[res_ind[2]] ],
            len: res_tr.len,
        })
    } else {
        None
    }
}


pub struct StarsStat {
    pub fwhm: f32,
    pub aver_r_dev: f32,
    pub common_stars_img: ImageLayerF32,
}

fn create_common_star_image(
    stars: &Stars,
    image: &ImageLayerF32,
    mag:   Crd,
) -> anyhow::Result<ImageLayerF32> {
    let max_width = stars.iter()
        .filter(|s| !s.overexposured)
        .map(|s| s.width)
        .max()
        .unwrap_or(0);

    let max_height = stars.iter()
        .filter(|s| !s.overexposured)
        .map(|s| s.height)
        .max()
        .unwrap_or(0);

    let max_range = stars.iter()
        .filter(|s| !s.overexposured)
        .map(|s| s.max_value - s.background)
        .max_by(cmp_f32)
        .unwrap_or(0.0);

    let range_border = max_range / 10.0;

    let img_width = max_width * mag * 2 + 1;
    let img_height = max_height * mag * 2 + 1;

    let mut result = ImageLayerF32::new(img_width, img_height);

    let mut pt_values = Vec::new();

    for (x, y, v) in result.iter_crd_mut() {
        pt_values.clear();
        for star in stars {
            if star.overexposured { continue; }
            let range = star.max_value - star.background;
            if range < range_border { continue; }

            let ox = (x - mag * max_width) as f64;
            let oy = (y - mag * max_height) as f64;

            let star_x = ox / mag as f64 + star.x;
            let star_y = oy / mag as f64 + star.y;

            let norm_star_values = if let Some(v) = image.get_f64_crd(star_x, star_y) {
                (v - star.background) / (star.max_value - star.background)
            } else {
                0.0
            };
            pt_values.push(CalcValue::new(norm_star_values as f64));
        }
        if let Some(filtered_v) = median_result(&mut pt_values) {
            *v = (filtered_v.result as f32).min(1.0);
        }
    }

    Ok(result)
}

pub fn calc_stars_stat(stars: &Stars, image: &ImageLayerF32) -> anyhow::Result<StarsStat> {
    const MAG: Crd = 8;
    let common_stars_img = create_common_star_image(stars, image, MAG)?;
    let points = common_stars_img.iter_crd()
        .filter(|(_, _, v)| *v > 0.25)
        .map(|(x, y, _)| (x, y))
        .collect();
    let (_, _, _, _, deviation) =
        calc_center_brightness_radius_and_deviation(image, 0.0, &points);
    let over_0_5_cnt = common_stars_img
        .as_slice()
        .iter()
        .filter(|&v| *v > 0.5)
        .count();
    Ok(StarsStat {
        fwhm:       over_0_5_cnt as f32 / (MAG * MAG) as f32,
        aver_r_dev: deviation as f32,
        common_stars_img
    })
}
