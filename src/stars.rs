use std::collections::HashSet;
use itertools::*;
use rayon::prelude::*;
use crate::{image::*, calc::*, log_utils::*};
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
    noise:              Option<f32>,
    remove_wrong_stars: bool,
    return_no_error:    bool,
) -> anyhow::Result<Stars> {
    let max_img_value = img.iter()
        .copied()
        .filter(|v| !v.is_infinite())
        .max_by(cmp_f32)
        .unwrap_or(1.0);
    let border = match noise {
        Some(noise) if noise != 0.0 =>
            (noise * 20.0).max(STAR_BG_BORDER * max_img_value),
        _ => {
            STAR_BG_BORDER * max_img_value
        },
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

    let mut star_calc = StarCalulator::new();

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

        if star_points.is_empty() || star_points.len() as i64 > MSD*MSD {
            continue;
        }

        let (center_x, center_y, br, radius, radius_dev) = star_calc.calc_center_brightness_radius_and_deviation(
            &img,
            star_bg,
            &star_points,
            true
        );

        if radius_dev > 2.0 { continue; }

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
            overexposured:  false,
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
            } else if return_no_error {
                return Ok(Vec::new());
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
            } else if return_no_error {
                return Ok(Vec::new());
            } else {
                anyhow::bail!("No stars");
            }
        }
    }

    stars.sort_by(|s1, s2| cmp_f64(&s1.brightness, &s2.brightness).reverse());

    Ok(stars)
}

pub fn set_stars_overexposured_flag(
    stars: &mut Stars,
    img:   &Image
) {
    for star in stars {
        let mut star_is_overexposured = false;
        for &StarPoint{x, y} in &star.points {
            if let Some(v) = img.l.get(x, y) { if v.is_infinite() {
                star_is_overexposured = true;
            }}
            if let Some(v) = img.r.get(x, y) { if v.is_infinite() {
                star_is_overexposured = true;
            }}
            if let Some(v) = img.g.get(x, y) { if v.is_infinite() {
                star_is_overexposured = true;
            }}
            if let Some(v) = img.b.get(x, y) { if v.is_infinite() {
                star_is_overexposured = true;
            }}
            if star_is_overexposured { break; }
        }
        star.overexposured = star_is_overexposured;
    }
}

struct StarCalulator {
    r: Vec<f64>,
}

impl StarCalulator {
    fn new() -> Self {
        Self {
            r: Vec::new(),
        }
    }

    #[inline(never)]
    fn calc_center_brightness_radius_and_deviation(
        &mut self,
        img:         &ImageLayerF32,
        star_bg:     f32,
        star_points: &HashSet<(Crd, Crd)>,
        use_image_for_center_calc: bool,
    ) -> (f64, f64, f64, f64, f64) {
        const BORD: f64 = 0.5;

        // center of star and brightness

        let (x_sum, y_sum, br) = star_points.iter().fold(
            (0_f64, 0_f64, 0_f64),
            |(x_sum, y_sum, br), &(x, y)| {
                let v = if use_image_for_center_calc {
                    (img.get(x, y).unwrap_or(0.0) - star_bg) as f64
                } else {
                    1.0
                };
                (x_sum + v * x as f64, y_sum + v * y as f64, br + v)
            }
        );

        let center_x = x_sum/br;
        let center_y = y_sum/br;

        // radius and ovality

        let is_border_point = |x, y| {
            !star_points.contains(&(x+1, y)) ||
            !star_points.contains(&(x-1, y)) ||
            !star_points.contains(&(x, y+1)) ||
            !star_points.contains(&(x, y-1))
        };

        self.r.clear();
        let mut radius_summ = 0_f64;
        let mut bord_pt_cnt = 0_u32;
        for &(sx, sy) in star_points.iter().filter(|(x, y)| is_border_point(*x, *y) ) {
            let dx = sx as f64 - center_x;
            let dy = sy as f64 - center_y;
            let r = (dx*dx + dy*dy).sqrt() + BORD;
            radius_summ += r;
            bord_pt_cnt += 1;
            self.r.push(r);
        }

        let radius = radius_summ / bord_pt_cnt as f64;

        self.r.sort_by(cmp_f64);
        let low_bound = self.r.len() / 10;
        let hi_bound = self.r.len() - low_bound - 1;
        let small_r = self.r[low_bound];
        let large_r = self.r[hi_bound];
        let diff_r = (small_r-large_r).abs();
        let radius_dev = diff_r / radius;

        (center_x, center_y, br, radius, radius_dev)
    }

}


#[derive(Debug)]
pub struct ImageOffset {
    pub offset_x: f64,
    pub offset_y: f64,
    pub angle:    f64,
    pub ratio:    f64,
}

pub fn calc_image_offset_by_stars(
    ref_stars:             &Stars,
    stars:                 &Stars,
    img_width:             f64,
    img_height:            f64,
    max_stars:             usize,
    find_triangle_max_err: f64,
    add_triangulation:     bool
) -> Option<ImageOffset> {
    let image_size = (img_width + img_height) / 2.0;

    let ref_triangles = get_stars_triangles(ref_stars, image_size / 5.0, max_stars, add_triangulation);
    let triangles = get_stars_triangles(stars, image_size / 5.0, max_stars, add_triangulation);

    log::info!(
        "-*= Align parameters calculation. max_stars={}, add_triangulation={} =*-",
        max_stars, add_triangulation
    );
    log::info!("ref_triangles.len() = {}", ref_triangles.len());
    log::info!("triangles.len() = {}", triangles.len());

    struct Corr<'a> {
        triangle: StarsTriangle<'a>,
        ref_triangle: &'a StarsTriangle<'a>,
        angle: f64,
        used: bool,
    }

    let mut corr_items: Vec<_> = ref_triangles.iter()
        .filter_map(|ref_tri| {
            let triangle = find_similar_triangle(
                &triangles,
                ref_tri,
                find_triangle_max_err
            );
            triangle.map(|triangle| (triangle, ref_tri))
        })
        .map(|(triangle, ref_tri)| {
            Corr {
                angle: ref_tri.calc_angle(&triangle),
                ref_triangle: ref_tri,
                triangle,
                used: true,
            }
        })
        .collect();

    log::info!("corr_items.len() = {}", corr_items.len());
    if corr_items.len() < 10 { return None; }

    let approximate_angle = {
        struct Cluster {
            start_index: usize,
            end_index: usize,
        }
        let mut clusters = Vec::new();
        const MIN_ANGLE_DIFF: f64 = 1.0 * PI / 360.0;
        corr_items.sort_by_key(|ci| (1_000_000.0 * ci.angle) as i64);
        let mut start_index = 0_usize;
        for (i, (c1, c2)) in corr_items.iter().tuple_windows().enumerate() {
            let angle_diff = c2.angle - c1.angle;
            if angle_diff > MIN_ANGLE_DIFF {
                if i != start_index {
                    clusters.push( Cluster { start_index, end_index: i } );
                }
                start_index = i+1;
            }
        }

        if start_index != corr_items.len()-1 {
            clusters.push(Cluster {
                start_index,
                end_index: corr_items.len()-1
            });
        }

        let (start_index, end_index) = if !clusters.is_empty() {
            let largest_cluster = clusters.iter().max_by_key(|cl| cl.end_index - cl.start_index).unwrap();
            log::info!("largest_cluster size = {}", largest_cluster.end_index - largest_cluster.start_index + 1);
            (largest_cluster.start_index, largest_cluster.end_index)
        } else {
            log::info!("no angle clusters");
            (0, corr_items.len()-1)
        };

        let mut angles: Vec<_> = corr_items[start_index..=end_index]
            .iter()
            .map(|c| c.angle)
            .collect();
        median_f64(&mut angles)?
    };

    log::info!("approximate_angle for filtering = {}", approximate_angle);
    let min_angle = approximate_angle - PI * 2.0 / 360.0;
    let max_angle = approximate_angle + PI * 2.0 / 360.0;
    corr_items.retain(|c| c.angle > min_angle && c.angle < max_angle);
    log::info!("corr_items.len() = {} after angle filter", corr_items.len());
    if corr_items.len() < 10 { return None; }

    let mut angles = Vec::new();
    let mut x_offsets = Vec::new();
    let mut y_offsets = Vec::new();

    let mut angle = 0_f64;
    let mut offset_x = 0_f64;
    let mut offset_y = 0_f64;

    for _ in 0..30 {
        angles.clear();
        for item in corr_items.iter() {
            angles.push(CalcValue::new_weighted(item.angle, item.triangle.len));
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

    let (_, x_dev) = mean_and_std_dev(&x_offsets)?;
    let (_, y_dev) = mean_and_std_dev(&y_offsets)?;
    let (_, a_dev) = mean_and_std_dev(&angles)?;

    log::info!("x_dev = {:.1}, y_dev = {:.1}, a_dev = {:.1}", x_dev, y_dev, 360.0 * a_dev / PI);

    if x_dev > 5.0 || y_dev > 5.0 || 360.0 * a_dev / PI > 5.0 {
        log::info!("angle={}, offset_x={}, offset_y={}", angle, offset_x, offset_y);
        return None;
    }

    let ref_sum: f64 = corr_items.iter().map(|item| item.ref_triangle.len).sum();
    let sum: f64 = corr_items.iter().map(|item| item.triangle.len).sum();
    let ratio = sum/ref_sum;

    log::info!("(used for calculation) corr_items.len() = {}", corr_items.len());

    Some(ImageOffset {
        angle,
        offset_x,
        offset_y,
        ratio,
    })
}

#[derive(Clone)]
struct StarsTriangle<'a> {
    stars: [&'a Star; 3],
    edges: [f64; 3],
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

        let sorted_star1 = sorted[0].1;
        let sorted_star2 = sorted[1].1;
        let sorted_star3 = sorted[2].1;

        let edge1 = sorted_star1.dist(sorted_star2);
        let edge2 = sorted_star2.dist(sorted_star3);
        let edge3 = sorted_star3.dist(sorted_star1);

        StarsTriangle {
            stars: [sorted_star1, sorted_star2, sorted_star3 ],
            edges: [edge1, edge2, edge3],
            len:   edge1 + edge2 + edge3,
        }
    }

    fn compare(&self, other: &StarsTriangle) -> (f64, [usize; 3]) {
        let cmp_fun = |i1: usize, i2: usize, i3: usize| {
            let diff1 = self.edges[i1] - other.edges[0];
            let diff2 = self.edges[i2] - other.edges[1];
            let diff3 = self.edges[i3] - other.edges[2];
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

fn get_stars_triangles<'a>(
    stars:             &'a [Star],
    min_len:           f64,
    mut max_stars:     usize,
    add_triangulation: bool
) -> StarsTriangles<'a> {
    use delaunator::*;

    let mut result = StarsTriangles::new();
    if max_stars > stars.len() { max_stars = stars.len(); }
    for i in 0..max_stars { for j in i+1..max_stars { for k in j+1..max_stars {
        let tr = StarsTriangle::new(&stars[i], &stars[j], &stars[k]);
        if tr.len < min_len { continue; }
        let min_edge = tr.len / 20.0;
        if tr.edges[0] < min_edge || tr.edges[1] < min_edge || tr.edges[2] < min_edge { continue; }
        result.push(tr);
    }}}

    if add_triangulation {
        let other_stars = &stars[max_stars..];
        let points_to_triangulate: Vec<_> = other_stars
            .iter()
            .map(|s| Point {x: s.x as f64, y: s.y as f64})
            .collect();

        let triagulation = triangulate(&points_to_triangulate);
        for (&i, &j, &k) in triagulation.triangles.iter().tuples() {
            let tr = StarsTriangle::new(&other_stars[i], &other_stars[j], &other_stars[k]);
            let min_edge = tr.len / 20.0;
            if tr.edges[0] < min_edge || tr.edges[1] < min_edge || tr.edges[2] < min_edge { continue; }
            result.push(tr);
        }
    }

    result.sort_by(|t1, t2| cmp_f64(&t1.len, &t2.len));
    result
}

fn find_similar_triangle<'a>(
    triangles:             &'a StarsTriangles,
    triangle:              &'a StarsTriangle,
    find_triangle_max_err: f64
) -> Option<StarsTriangle<'a>> {
    let lower_res = triangles.binary_search_by(|t| cmp_f64(&t.len, &(triangle.len-find_triangle_max_err)));
    let upper_res = triangles.binary_search_by(|t| cmp_f64(&t.len, &(triangle.len+find_triangle_max_err)));

    let lower_index = match lower_res { Ok(v) => v, Err(v) => v };
    let upper_index = match upper_res { Ok(v) => v, Err(v) => v };

    triangles[lower_index..upper_index]
        .iter()
        .map(|test_triangle|
            (test_triangle, test_triangle.compare(triangle))
        )
        .filter(|(_, (err, _))|
            *err < find_triangle_max_err
        )
        .min_by_key(|(_, (err, _))|
            (err * 1000.0) as i64
        )
        .map(|(tr, (_, res_ind))| {
            let star1 = tr.stars[res_ind[0]];
            let star2 = tr.stars[res_ind[1]];
            let star3 = tr.stars[res_ind[2]];
            let edge1 = star1.dist(star2);
            let edge2 = star2.dist(star3);
            let edge3 = star3.dist(star1);

            StarsTriangle {
                stars: [ star1, star2, star3 ],
                edges: [ edge1, edge2, edge3 ],
                len: edge1 + edge2 + edge3,
            }
        })
}


pub struct StarsStat {
    pub fwhm: f32,
    pub aver_r_dev: f32,
    pub common_stars_img: ImageLayerF32,
}

pub fn create_common_star_image(
    stars: &Stars,
    image: &ImageLayerF32,
    mag:   Crd,
    fast:  bool,
) -> anyhow::Result<ImageLayerF32> {
    let not_overexposured_stars = || {
        stars.iter().filter(|s| !s.overexposured)
    };

    let mut star_widths: Vec<_> = not_overexposured_stars()
        .map(|s| s.width)
        .collect();

    let mut star_heights: Vec<_> = not_overexposured_stars()
        .map(|s| s.height)
        .collect();

    let max_width = median_i64(&mut star_widths).unwrap_or(0);
    let max_height = median_i64(&mut star_heights).unwrap_or(0);

    let img_width = max_width * mag * 2 + 1;
    let img_height = max_height * mag * 2 + 1;

    let mut result = ImageLayerF32::new(img_width, img_height);

    let img_center_x = (image.width() / 2) as f64;
    let img_center_y = (image.height() / 2) as f64;
    let max_dist_to_center = (Crd::max(image.width(), image.height()) / 2) as f64;

    let mut used_stars_cnt = not_overexposured_stars().count() / 2;

    const MAX_USED_STARS_COUNT: usize = 200;
    const MIN_USED_STARS_COUNT: usize = 30;

    if fast && used_stars_cnt > MAX_USED_STARS_COUNT {
        used_stars_cnt = MAX_USED_STARS_COUNT;
    } else if used_stars_cnt < MIN_USED_STARS_COUNT {
        used_stars_cnt = MIN_USED_STARS_COUNT;
    }

    const MARGIN_WEIGHT: f64 = 0.2; // weight of stars on image margin

    let stars_for_image = || { stars
        .iter()
        .filter(|s| !s.overexposured)
        .take(used_stars_cnt)
    };

    let star_weights: Vec<f64> = stars_for_image()
        .map(|star| {
            f64::sqrt(
                (star.x - img_center_x) * (star.x - img_center_x) +
                (star.y - img_center_y) * (star.y - img_center_y))
            }
        )
        .map(|dist_to_center| {
            linear_interpol(dist_to_center, 0.0, max_dist_to_center, 1.0, MARGIN_WEIGHT)
        })
        .collect();

    let max_mul_max_width = mag * max_width;
    let mag_mul_max_height = mag * max_height;
    let div_mag = 1.0 / mag as f64;
    let result_width = result.width() as usize;
    let result_height = result.width() as usize;

    let calc_one_common_star_value = |v: &mut f32, x, y, pt_values: &mut Vec<CalcValue>| {
        for (star, &star_weight) in izip!(stars_for_image(), &star_weights) {
            let ox = (x - max_mul_max_width) as f64;
            let oy = (y - mag_mul_max_height) as f64;
            let src_x = ox * div_mag + star.x;
            let src_y = oy * div_mag + star.y;
            if let Some(v) = image.get_f64_crd(src_x, src_y) {
                let norm_star_values = linear_interpol(
                    v as f64,
                    star.background as f64,
                    star.max_value as f64,
                    0.0,
                    1.0
                );
                if star_weight > 0.0 {
                    pt_values.push(CalcValue::new_weighted(norm_star_values as f64, star_weight));
                }
            }
        }

        if let Some(filtered_v) = cappa_sigma_weighted_result(pt_values, 1.8, 10, true, true) {
            *v = (filtered_v.result as f32).min(1.0).max(0.0);
        }
    };

    if fast { // multi-threaded
        result.as_slice_mut().par_iter_mut().enumerate().for_each(|(i, v)| {
            let x = (i % result_width) as Crd;
            let y = (i / result_height) as Crd;
            let mut pt_values = Vec::new();
            calc_one_common_star_value(v, x, y, &mut pt_values);
        });
    } else { // single-threaded
        let mut pt_values = Vec::new();
        for (x, y, v) in result.iter_crd_mut() {
            pt_values.clear();
            calc_one_common_star_value(v, x, y, &mut pt_values);
        }
    }

    Ok(result)
}

pub fn calc_stars_stat(stars: &Stars, image: &ImageLayerF32, fast: bool) -> anyhow::Result<StarsStat> {
    let mag = if !fast { 8 } else { 8 };
    log::info!("calc_stars_stat: stars_count = {}, fast = {}", stars.len(), fast);
    let profiler = TimeLogger::start();
    let common_stars_img = create_common_star_image(stars, image, mag, fast)?;
    profiler.log("create_common_star_image");
    let points = common_stars_img.iter_crd()
        .filter(|(_, _, v)| *v > 0.5)
        .map(|(x, y, _)| (x, y))
        .collect();
    let mut star_calc = StarCalulator::new();
    let (_, _, _, _, deviation) =
        star_calc.calc_center_brightness_radius_and_deviation(image, 0.0, &points, false);
    let over_0_5_cnt = common_stars_img
        .as_slice()
        .iter()
        .filter(|&v| *v > 0.5)
        .count();
    Ok(StarsStat {
        fwhm:       over_0_5_cnt as f32 / (mag * mag) as f32,
        aver_r_dev: deviation as f32,
        common_stars_img
    })
}
