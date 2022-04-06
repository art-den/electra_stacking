use std::collections::{VecDeque,HashSet};
use serde::{Serialize, Deserialize};
use crate::calc::*;

pub const NO_VALUE_F32: f32 = -999.0;

pub type Crd = i64;

#[derive(PartialEq)]
pub enum IterType {
    Cols,
    Rows,
    Both,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Exif {
    pub iso:       Option<u32>,
    pub exp_time:  Option<f32>,
    pub focal_len: Option<f32>,
    pub fnumber:   Option<f32>,
    pub camera:    Option<String>,
}

impl Exif {
    pub fn new_empty() -> Exif {
        Exif {
            iso: None,
            exp_time: None,
            focal_len: None,
            fnumber: None,
            camera: None,
        }
    }
}

/*****************************************************************************/

/* Image layer */

#[derive(Clone)]
pub struct ImageLayer<T>
where
    T: Copy + Clone + ImgLayerDefValue<Type = T>
{
    data: Vec<T>,
    width: Crd,
    height: Crd,
}

pub type ImageLayerF32 = ImageLayer::<f32>;
pub type ImageMask = ImageLayer::<bool>;

impl<T> ImageLayer<T>
where T: Copy + Clone + ImgLayerDefValue<Type = T> {
    pub fn new(width: Crd, height: Crd) -> ImageLayer<T> {
        let mut result = ImageLayer::<T> { data: vec![], width, height };
        result.data.resize((width * height) as usize, T::DEF_VALUE);
        result
    }

    pub fn new_empty() -> ImageLayer<T> {
        ImageLayer { data: vec![], width: 0, height: 0 }
    }

    pub fn resize_and_clear(&mut self, width: Crd, height: Crd) {
        let prev_size = self.data.len();
        let new_size = (width * height) as usize;
        self.data.resize((width * height) as usize, T::DEF_VALUE);
        self.data[0..prev_size.min(new_size)].fill(T::DEF_VALUE);
        self.width = width;
        self.height = height;
    }

    pub fn width(&self) -> Crd {
        self.width
    }

    pub fn height(&self) -> Crd {
        self.height
    }

    fn make_empty(&mut self) {
        self.data.clear();
        self.width = 0;
        self.height = 0;
    }

    pub fn is_empty(&self) -> bool {
        (self.width == 0) && self.data.is_empty()
    }

    #[inline(always)]
    pub fn get(&self, x: Crd, y: Crd) -> Option<T> {
        if x < 0 || x >= self.width || y < 0 {
            return None;
        }
        let index = (y * self.width + x) as usize;
        if index >= self.data.len() { return None; }
        Some(self.data[index])
    }

    #[inline(always)]
    pub fn set(&mut self, x: Crd, y: Crd, value: T) {
        if x < 0 || x >= self.width || y < 0 || y >= self.height {
            panic!("Internal error")
        }
        self.data[(y*self.width+x) as usize] = value;
    }

    #[inline(always)]
    pub fn set_safe(&mut self, x: Crd, y: Crd, value: T) {
        if x < 0 || x >= self.width || y < 0 || y >= self.height {
            return;
        }
        self.data[(y*self.width+x) as usize] = value;
    }

    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, T> {
        self.data.iter()
    }

    pub fn iter_mut<'a>(&'a mut self) -> std::slice::IterMut<'a, T> {
        self.data.iter_mut()
    }

    pub fn iter_row<'a>(&'a self, y: Crd) -> std::slice::Iter<'a, T> {
        let pos = (y*self.width) as usize;
        self.data[pos..pos + self.width as usize].iter()
    }

    pub fn iter_row_mut<'a>(&'a mut self, y: Crd) -> std::slice::IterMut<'a, T> {
        let pos = (y*self.width) as usize;
        self.data[pos..pos + self.width as usize].iter_mut()
    }

    pub fn iter_col<'a>(&'a self, x: Crd) -> std::iter::StepBy<std::slice::Iter<'a, T>> {
        self.data[x as usize..].iter().step_by(self.width as usize)
    }

    pub fn iter_col_mut<'a>(&'a mut self, x: Crd) -> std::iter::StepBy<std::slice::IterMut<'a, T>> {
        self.data[x as usize..].iter_mut().step_by(self.width as usize)
    }

    pub fn iter_crd<'a>(&'a self) -> ImageLayerIter1::<'a, T> {
        ImageLayerIter1::<'a> {
            iter1: self.data.iter(),
            x: 0,
            y: 0,
            width: self.width,
        }
    }

    pub fn iter_crd_mut<'a>(&'a mut self) -> ImageLayerMutIter1::<'a, T> {
        ImageLayerMutIter1::<'a> {
            iter1: self.data.iter_mut(),
            x: 0,
            y: 0,
            width: self.width,
        }
    }

    pub fn iter_crd3<'a>(
        l1: &'a ImageLayer<T>,
        l2: &'a ImageLayer<T>,
        l3: &'a ImageLayer<T>
    ) -> ImageLayerIter3::<'a, T> {
        assert!(l1.width == l2.width);
        assert!(l1.data.len() == l2.data.len());
        assert!(l1.width == l3.width);
        assert!(l1.data.len() == l3.data.len());

        ImageLayerIter3::<'a> {
            iter1: l1.data.iter(),
            iter2: l2.data.iter(),
            iter3: l3.data.iter(),
            x: 0,
            y: 0,
            width: l1.width,
        }
    }

    pub fn iter_crd_mut3<'a>(
        l1: &'a mut ImageLayer<T>,
        l2: &'a mut ImageLayer<T>,
        l3: &'a mut ImageLayer<T>
    ) -> ImageLayerMutIter3::<'a, T> {
        assert!(l1.width == l2.width);
        assert!(l1.data.len() == l2.data.len());
        assert!(l1.width == l3.width);
        assert!(l1.data.len() == l3.data.len());

        ImageLayerMutIter3::<'a> {
            iter1: l1.data.iter_mut(),
            iter2: l2.data.iter_mut(),
            iter3: l3.data.iter_mut(),
            x: 0,
            y: 0,
            width: l1.width,
        }
    }

    pub fn foreach_row_and_col(
        &self,
        desired_types: IterType,
        mut fun:       impl FnMut(&[T], Crd, IterType))
    {
        if desired_types == IterType::Rows || desired_types == IterType::Both {
            let uwidth = self.width as usize;
            let mut index: usize = 0;
            for y in 0..self.height {
                fun(&self.data[index..index+self.width as usize], y, IterType::Rows);
                index += uwidth;
            }
        }

        if desired_types == IterType::Cols || desired_types == IterType::Both {
            let mut col: Vec<T> = Vec::new();
            col.resize(self.height as usize, T::DEF_VALUE);
            for x in 0..self.width {
                for (s, d) in self.iter_col(x).zip(col.iter_mut()) { *d = *s; }
                fun(&col, x as Crd, IterType::Cols);
            }
        }
    }

    pub fn foreach_row_and_col_mut(
        &mut self,
        desired_types: IterType,
        mut fun:       impl FnMut(&mut [T], Crd, IterType) -> bool)
    {
        if desired_types == IterType::Rows || desired_types == IterType::Both {
            let uwidth = self.width as usize;
            let mut index: usize = 0;
            for y in 0..self.height {
                fun(&mut self.data[index..index+self.width as usize], y, IterType::Rows);
                index += uwidth;
            }
        }

        if desired_types == IterType::Cols || desired_types == IterType::Both {
            let mut col: Vec<T> = Vec::new();
            col.resize(self.height as usize, T::DEF_VALUE);
            for x in 0..self.width {
                for (s, d) in self.iter_col(x).zip(col.iter_mut()) { *d = *s; }
                let changed = fun(&mut col, x as Crd, IterType::Cols);
                if changed {
                    for (s, d) in col.iter().zip(self.iter_col_mut(x)) { *d = *s; }
                }
            }
        }
    }

    pub fn as_slice(&self) -> &[T] {
        &self.data
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        &mut self.data
    }

    pub fn iter_rect_crd(&self, x1: Crd, y1: Crd, x2: Crd, y2: Crd) -> RectIterCrd<T> {
        RectIterCrd::new(self, x1, y1, x2, y2)
    }

    pub fn iter_area_crd(&self, area: &RectArea) -> RectIterCrd<T> {
        RectIterCrd::new(self, area.x1, area.y1, area.x2, area.y2)
    }
}

impl ImageLayer<f32> {
    pub fn get_f64_crd(&self, x: f64, y: f64) -> Option<f32> {
        let mut ix = x as Crd;
        let mut dx = (x - ix as f64) as f32;
        if x.is_sign_negative() {
            ix -= 1;
            dx = 1.0 - dx;
        }

        let mut iy = y as Crd;
        let mut dy = (y - iy as f64) as f32;
        if y.is_sign_negative() {
            iy -= 1;
            dy = 1.0 - dy;
        }

        let p11 = self.get(ix, iy);
        let p21 = self.get(ix+1, iy);
        let p12 = self.get(ix, iy+1);
        let p22 = self.get(ix+1, iy+1);

        let s11 = if p11.is_some() { (1.0 - dx) * (1.0 - dy) } else { 0.0 };
        let s21 = if p21.is_some() { dx * (1.0 - dy) } else { 0.0 };
        let s12 = if p12.is_some() { (1.0 - dx) * dy } else { 0.0 };
        let s22 = if p22.is_some() { dx * dy } else { 0.0 };

        let mut sum =
            s11 * p11.unwrap_or(0.0) +
            s21 * p21.unwrap_or(0.0) +
            s12 * p12.unwrap_or(0.0) +
            s22 * p22.unwrap_or(0.0);

        if sum.is_nan() { // can be if 0*INF (sXX * pXX). Recalc more carefully
            const MIN_S: f32 = 1e-10;
            sum = 0.0;
            if s11 > MIN_S { sum += s11 * p11.unwrap_or(0.0); }
            if s21 > MIN_S { sum += s21 * p21.unwrap_or(0.0); }
            if s12 > MIN_S { sum += s12 * p12.unwrap_or(0.0); }
            if s22 > MIN_S { sum += s22 * p22.unwrap_or(0.0); }
        }

        if p11.is_some() && p21.is_some() && p12.is_some() && p22.is_some() {
            Some(sum)
        } else if !p11.is_some() && !p21.is_some() && !p12.is_some() && !p22.is_some() {
            return None;
        } else {
            Some(sum / (s11 + s21 + s12 + s22))
        }
    }

    fn rotated_and_translated(
        &self,
        angle:         f64,
        transl_x:      f64,
        transl_y:      f64,
        default_value: f32,
        result_width:  Crd,
        result_height: Crd
    ) -> ImageLayerF32 {
        if self.is_empty() { return ImageLayerF32::new_empty(); }
        let mut result = ImageLayerF32::new(result_width, result_height);
        let center_x = (result_width as f64 - 1.0) / 2.0;
        let center_y = (result_height as f64 - 1.0) / 2.0;
        let cos_a = f64::cos(-angle);
        let sin_a = f64::sin(-angle);
        for (x, y, v) in result.iter_crd_mut() {
            let x = x as f64 - transl_x;
            let y = y as f64 - transl_y;
            let dx = x - center_x;
            let dy = y - center_y;
            let rot_x = center_x + dx * cos_a - dy * sin_a;
            let rot_y = center_y + dy * cos_a + dx * sin_a;
            *v = self.get_f64_crd(rot_x, rot_y).unwrap_or(default_value);
        }
        result
    }

    pub fn substract(&mut self, other: &ImageLayerF32) {
        assert!(self.width == other.width);
        assert!(self.height == other.height);
        for (s, o) in self.data.iter_mut().zip(other.iter()) {
            *s -= *o;
        }
    }

    pub fn multiply(&mut self, other: &ImageLayerF32) {
        assert!(self.width == other.width);
        assert!(self.height == other.height);
        for (s, o) in self.data.iter_mut().zip(other.iter()) {
            *s *= *o;
        }
    }

    pub fn mult_f32(&mut self, value: f32) {
        for v in self.data.iter_mut() {
            if *v == NO_VALUE_F32 { continue; }
            *v *= value;
        }
    }

    pub fn fill_inf_areas(&mut self) {
        if self.is_empty() { return; }

        let mut flood_filler = FloodFiller::new();
        let mut inf_area_points: HashSet<(Crd, Crd)> = HashSet::new();
        let mut around_area_points: HashSet<(Crd, Crd)> = HashSet::new();
        let mut cur_pos: usize = 0;
        while let Some(pos) = self.data[cur_pos..].iter().position(|v| v.is_infinite()) {
            cur_pos += pos;
            let x = cur_pos as Crd % self.width;
            let y = cur_pos as Crd / self.width;
            debug_assert!(self.get(x, y).unwrap_or(0.0).is_infinite());

            inf_area_points.clear();
            flood_filler.fill(
                x, y,
                |x, y| {
                    if inf_area_points.contains(&(x, y)) { return false; }
                    if let Some(v) = self.get(x, y) { if !v.is_infinite() {
                        return false;
                    }} else {
                        return false;
                    }
                    inf_area_points.insert((x, y));
                    true
                }
            );
            debug_assert!(!inf_area_points.is_empty());

            around_area_points.clear();
            for &(x, y) in inf_area_points.iter() {
                let mut try_outer_pt = |x, y| {
                    if x < 0 || x >= self.width { return; }
                    if y < 0 || y >= self.height { return; }
                    if inf_area_points.contains(&(x, y)) { return; }
                    around_area_points.insert((x, y));
                };
                try_outer_pt(x+1, y);
                try_outer_pt(x-1, y);
                try_outer_pt(x, y+1);
                try_outer_pt(x, y-1);
            }

            for &(dst_x, dst_y) in inf_area_points.iter() {
                let mut sum = 0_f64;
                let mut sum_w = 0_f64;
                for &(x, y) in around_area_points.iter() {
                    let dist = f64::sqrt(((x - dst_x) * (x - dst_x) + (y - dst_y) * (y - dst_y)) as f64);
                    let w = 1.0 / dist;
                    sum += w * self.get(x, y).unwrap_or(0.0) as f64;
                    sum_w += w;
                }
                self.set(dst_x, dst_y, (sum / sum_w) as f32);
            }
        }
    }

    pub fn check_contains_inf_or_nan(&self) -> anyhow::Result<()> {
        for v in self.data.iter() {
            if v.is_infinite() { anyhow::bail!("Image contain INF"); }
            if v.is_nan() { anyhow::bail!("Image contain NAN"); }
        }
        Ok(())
    }

    pub fn decrease_2x(&self) -> ImageLayerF32 {
        let res_width = self.width / 2;
        let res_height = self.height / 2;
        let mut result = ImageLayerF32::new(res_width, res_height);
        for y in 0..res_height {
            let mut it1 = self.iter_row(2*y);
            let mut it2 = self.iter_row(2*y+1);
            for d in result.iter_row_mut(y) {
                *d = 0.25 * (
                    it1.next().unwrap_or(&0.0) + it1.next().unwrap_or(&0.0) +
                    it2.next().unwrap_or(&0.0) + it2.next().unwrap_or(&0.0)
                );
            }
        }
        result
    }
}

impl std::ops::SubAssign<&ImageLayerF32> for ImageLayerF32 {
    fn sub_assign(&mut self, rhs: &ImageLayerF32) {
        self.substract(rhs);
    }
}

impl std::ops::MulAssign<&ImageLayerF32> for ImageLayerF32 {
    fn mul_assign(&mut self, rhs: &ImageLayerF32) {
        self.multiply(rhs);
    }
}

pub trait ImgLayerDefValue {
    type Type;
    const DEF_VALUE: Self::Type;
}

impl ImgLayerDefValue for f32 {
    type Type = f32;
    const DEF_VALUE: Self::Type = 0.0;
}

impl ImgLayerDefValue for bool {
    type Type = bool;
    const DEF_VALUE: Self::Type = false;
}

pub struct ImageLayerIter1<'a, T: Copy + Clone> {
    iter1: std::slice::Iter<'a, T>,
    x: Crd,
    y: Crd,
    width: Crd,
}

impl<'a, T: Copy + Clone> Iterator for ImageLayerIter1<'a, T> {
    type Item = (Crd, Crd, T);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_val1) = self.iter1.next() {
            let ret = (self.x, self.y, *next_val1);
            self.x += 1;
            if self.x == self.width { self.x = 0; self.y += 1; }
            Some(ret)
        } else {
            None
        }
    }
}

pub struct ImageLayerMutIter1<'a, T: Copy + Clone> {
    iter1: std::slice::IterMut<'a, T>,
    x:     Crd,
    y:     Crd,
    width: Crd,
}

impl<'a, T: Copy + Clone> Iterator for ImageLayerMutIter1<'a, T> {
    type Item = (Crd, Crd, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_val1) = self.iter1.next() {
            let ret = (self.x, self.y, next_val1);
            self.x += 1;
            if self.x == self.width { self.x = 0; self.y += 1; }
            Some(ret)
        } else {
            None
        }
    }
}

pub struct ImageLayerIter3<'a, T: Copy + Clone> {
    iter1: std::slice::Iter<'a, T>,
    iter2: std::slice::Iter<'a, T>,
    iter3: std::slice::Iter<'a, T>,
    x:     Crd,
    y:     Crd,
    width: Crd,
}

impl<'a, T: Copy + Clone> Iterator for ImageLayerIter3<'a, T> {
    type Item = (Crd, Crd, T, T, T);

    fn next(&mut self) -> Option<Self::Item> {
        match (self.iter1.next(), self.iter2.next(), self.iter3.next()) {
            (Some(v1), Some(v2), Some(v3)) => {
                let ret = (self.x, self.y, *v1, *v2, *v3);
                self.x += 1;
                if self.x == self.width { self.x = 0; self.y += 1; }
                Some(ret)
            },
            _ => None,
        }
    }
}

pub struct ImageLayerMutIter3<'a, T: Copy + Clone> {
    iter1: std::slice::IterMut<'a, T>,
    iter2: std::slice::IterMut<'a, T>,
    iter3: std::slice::IterMut<'a, T>,
    x:     Crd,
    y:     Crd,
    width: Crd,
}

impl<'a, T: Copy + Clone> Iterator for ImageLayerMutIter3<'a, T> {
    type Item = (Crd, Crd, &'a mut T, &'a mut T, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        match (self.iter1.next(), self.iter2.next(), self.iter3.next()) {
            (Some(v1), Some(v2), Some(v3)) => {
                let ret = (self.x, self.y, v1, v2, v3);
                self.x += 1;
                if self.x == self.width { self.x = 0; self.y += 1; }
                Some(ret)
            },
            _ => None,
        }
    }
}

pub struct RectIterCrd<'a, T: Copy + Clone + ImgLayerDefValue<Type = T>> {
    img: &'a ImageLayer<T>,
    x1: Crd,
    y1: Crd,
    x2: Crd,
    y2: Crd,
    x: Crd,
    y: Crd,
    iter: std::slice::Iter<'a, T>,
}

impl<'a, T: Copy + Clone + ImgLayerDefValue<Type = T>> RectIterCrd<'a, T> {
    fn new(
        img:    &'a ImageLayer<T>,
        mut x1: Crd,
        mut y1: Crd,
        mut x2: Crd,
        mut y2: Crd
    ) -> RectIterCrd<'a, T> {
        if x1 < 0 { x1 = 0; }
        if y1 < 0 { y1 = 0; }
        if x2 >= img.width { x2 = img.width-1; }
        if y2 >= img.height { y2 = img.height-1; }
        RectIterCrd {
            img, x1, y1, x2, y2, x: x1, y: y1,
            iter: RectIterCrd::create_iter(img, x1, x2, y1)
        }
    }

    fn create_iter(
        img: &'a ImageLayer<T>,
        x1:  Crd,
        x2:  Crd,
        y:   Crd
    ) -> std::slice::Iter<'a, T> {
        let line_start = y*img.width;
        (&img.data[(line_start+x1) as usize ..= (line_start+x2) as usize]).iter()
    }
}

impl<'a, T: Copy + Clone + ImgLayerDefValue<Type = T>> Iterator
for RectIterCrd<'a, T> {
    type Item = (Crd, Crd, T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(v) = self.iter.next() {
                let result = (self.x, self.y, *v);
                self.x += 1;
                return Some(result);
            } else {
                self.y += 1;
                if self.y > self.y2 { return None; }
                self.x = self.x1;
                self.iter = RectIterCrd::create_iter(self.img, self.x1, self.x2, self.y);
            }
        }
    }
}

/*****************************************************************************/

/* Image */

pub struct Image {
    pub r: ImageLayerF32,
    pub g: ImageLayerF32,
    pub b: ImageLayerF32,
    pub l: ImageLayerF32,
}

impl Image {
    pub fn new() -> Image {
        Image {
            r: ImageLayerF32::new_empty(),
            g: ImageLayerF32::new_empty(),
            b: ImageLayerF32::new_empty(),
            l: ImageLayerF32::new_empty(),
        }
    }

    pub fn new_grey(width: Crd, height: Crd) -> Image {
        Image {
            r: ImageLayerF32::new_empty(),
            g: ImageLayerF32::new_empty(),
            b: ImageLayerF32::new_empty(),
            l: ImageLayerF32::new(width, height),
        }
    }

    pub fn new_color(width: Crd, height: Crd) -> Image {
        Image {
            r: ImageLayerF32::new(width, height),
            g: ImageLayerF32::new(width, height),
            b: ImageLayerF32::new(width, height),
            l: ImageLayerF32::new_empty(),
        }
    }

    pub fn resize(&mut self, width: Crd, height: Crd) {
        if !self.r.is_empty() { self.r.resize_and_clear(width, height); }
        if !self.g.is_empty() { self.g.resize_and_clear(width, height); }
        if !self.b.is_empty() { self.b.resize_and_clear(width, height); }
        if !self.l.is_empty() { self.l.resize_and_clear(width, height); }
    }

    pub fn make_grey(&mut self, width: Crd, height: Crd) {
        self.r.make_empty();
        self.g.make_empty();
        self.b.make_empty();
        self.l.resize_and_clear(width, height);
    }

    pub fn make_color(&mut self, width: Crd, height: Crd) {
        self.r.resize_and_clear(width, height);
        self.g.resize_and_clear(width, height);
        self.b.resize_and_clear(width, height);
        self.l.make_empty();
    }

    pub fn width(&self) -> Crd {
        if      self.is_greyscale() { self.l.width }
        else if self.is_rgb()       { self.r.width }
        else                        { 0 }
    }

    pub fn height(&self) -> Crd {
        if      self.is_greyscale() { self.l.height }
        else if self.is_rgb()       { self.r.height }
        else                        { 0 }
    }

    pub fn is_empty(&self) -> bool {
        self.r.is_empty() &&
        self.g.is_empty() &&
        self.b.is_empty() &&
        self.l.is_empty()
    }

    pub fn is_greyscale(&self) -> bool {
        !self.l.is_empty()
    }

    pub fn is_rgb(&self) -> bool {
        !self.r.is_empty() && !self.g.is_empty() && !self.b.is_empty()
    }

    pub fn set_rgb(&mut self, x: Crd, y: Crd, r: f32, g: f32, b: f32) {
        self.r.set(x, y, r);
        self.g.set(x, y, g);
        self.b.set(x, y, b);
    }

    pub fn to_greyscale(&self, result: &mut ImageLayerF32) {
        result.resize_and_clear(self.width(), self.height());
        if self.is_greyscale() {
            for (d, s) in result.iter_mut().zip(self.l.iter()) {
                *d = *s;
            }
        } else {
            for (d, (_, _, r, g, b))
            in result.iter_mut().zip(self.iter_rgb_crd()) {
                if r != NO_VALUE_F32
                && g != NO_VALUE_F32
                && b != NO_VALUE_F32 {
                    *d = 0.2126 * r + 0.7152 * g + 0.0722 * b
                } else {
                    *d = NO_VALUE_F32;
                }
            }
        }
    }

    pub fn create_greyscale_layer(&self) -> ImageLayerF32 {
        assert!(!self.is_empty());
        if self.is_rgb() {
            let mut result = ImageLayerF32::new_empty();
            self.to_greyscale(&mut result);
            result
        } else {
            self.l.clone()
        }
    }

    pub fn iter_l_crd<'a>(&'a self) -> ImageLayerIter1<'a, f32> {
        self.l.iter_crd()
    }

    pub fn iter_l_crd_mut<'a>(&'a mut self) -> ImageLayerMutIter1<'a, f32> {
        self.l.iter_crd_mut()
    }

    pub fn iter_rgb_crd<'a>(&'a self) -> ImageLayerIter3<'a, f32> {
        ImageLayerF32::iter_crd3(&self.r, &self.g, &self.b)
    }

    pub fn iter_rgb_crd_mut<'a>(&'a mut self) -> ImageLayerMutIter3<'a, f32> {
        ImageLayerF32::iter_crd_mut3(&mut self.r, &mut self.g, &mut self.b)
    }

    pub fn rotated_and_translated(
        &self,
        angle:         f64,
        transl_x:      f64,
        transl_y:      f64,
        default_value: f32,
        result_width:  Crd,
        result_height: Crd
    ) -> Image {
        Image {
            l: self.l.rotated_and_translated(angle, transl_x, transl_y, default_value, result_width, result_height),
            r: self.r.rotated_and_translated(angle, transl_x, transl_y, default_value, result_width, result_height),
            g: self.g.rotated_and_translated(angle, transl_x, transl_y, default_value, result_width, result_height),
            b: self.b.rotated_and_translated(angle, transl_x, transl_y, default_value, result_width, result_height),
        }
    }

    pub fn mult_f32(&mut self, value: f32) {
        self.l.mult_f32(value);
        self.r.mult_f32(value);
        self.g.mult_f32(value);
        self.b.mult_f32(value);
    }

    pub fn normalize_if_greater_1(&mut self) {
        let max_l = self.l.iter().copied().max_by(cmp_f32).unwrap_or(0.0);
        let max_r = self.r.iter().copied().max_by(cmp_f32).unwrap_or(0.0);
        let max_g = self.g.iter().copied().max_by(cmp_f32).unwrap_or(0.0);
        let max_b = self.b.iter().copied().max_by(cmp_f32).unwrap_or(0.0);
        let max = [max_l, max_r, max_g, max_b].iter().copied().max_by(cmp_f32).unwrap_or(0.0);
        if max < 1.0 { return; }

        let do_norm = |k, img: &mut ImageLayerF32| {
            for v in img.iter_mut() {
                *v *= k;
            }
        };

        let k = 1.0 / max;
        do_norm(k, &mut self.l);
        do_norm(k, &mut self.r);
        do_norm(k, &mut self.g);
        do_norm(k, &mut self.b);
    }

    pub fn check_contains_inf_or_nan(&self) -> anyhow::Result<()> {
        self.l.check_contains_inf_or_nan()?;
        self.r.check_contains_inf_or_nan()?;
        self.g.check_contains_inf_or_nan()?;
        self.b.check_contains_inf_or_nan()?;
        Ok(())
    }

    pub fn decrease_2x(&self) -> Image {
        Image {
            r: self.r.decrease_2x(),
            g: self.g.decrease_2x(),
            b: self.b.decrease_2x(),
            l: self.l.decrease_2x(),
        }
    }
}

/*****************************************************************************/

/* AreaIterator */

pub struct RectAreaIterator {
    i: i64,
    width: Crd,
    x_div_cnt: i64,
    x: Crd,
    x_rem: Crd,
    j: i64,
    height: Crd,
    y_div_cnt: i64,
    y: Crd,
    y_rem: Crd,
    active: bool,
}

impl RectAreaIterator {
    pub fn new(width: Crd, x_div_cnt: i64, height: Crd, y_div_cnt: i64) -> RectAreaIterator {
        RectAreaIterator {
            i: 0,
            width,
            x: 0,
            x_rem: x_div_cnt/2,
            x_div_cnt,
            j: 0,
            height,
            y: 0,
            y_rem: y_div_cnt/2,
            y_div_cnt,
            active: true,
        }
    }
}

pub struct RectArea {
    pub x1: Crd,
    pub y1: Crd,
    pub x2: Crd,
    pub y2: Crd,
}

impl Iterator for RectAreaIterator {
    type Item = (i64, i64, RectArea);

    fn next(&mut self) -> Option<Self::Item> {
        if self.active {
            let x_dividend = self.width + self.x_rem;
            let w = x_dividend / self.x_div_cnt;
            let y_dividend = self.height + self.y_rem;
            let h = y_dividend / self.y_div_cnt;
            let result = (self.i, self.j, RectArea {
                x1: self.x,
                x2: self.x + w - 1,
                y1: self.y,
                y2: self.y + h - 1,
            });
            self.i += 1;
            if self.i < self.x_div_cnt {
                self.x += w;
                self.x_rem = x_dividend % self.x_div_cnt;
            } else {
                self.i = 0;
                self.x = 0;
                self.j += 1;
                if self.j >= self.y_div_cnt {
                    self.active = false;
                } else {
                    self.y += h;
                    self.x_rem = self.x_div_cnt/2;
                    self.y_rem = y_dividend % self.y_div_cnt;
                }
            }
            Some(result)
        } else {
            None
        }
    }
}

pub struct FloodFiller {
    visited: VecDeque<(Crd, Crd)>,
}

impl FloodFiller {
    pub fn new() -> FloodFiller {
        FloodFiller {
            visited: VecDeque::new(),
        }
    }

    pub fn fill<SetFilled: FnMut(Crd, Crd) -> bool>(
        &mut self,
        x: Crd,
        y: Crd,
        mut try_set_filled: SetFilled
    ) {
        if !try_set_filled(x, y) { return; }

        self.visited.clear();
        self.visited.push_back((x, y));

        while let Some((pt_x, pt_y)) = self.visited.pop_front() {
            let mut check_neibour = |x, y| {
                if !try_set_filled(x, y) { return; }
                self.visited.push_back((x, y));
            };
            check_neibour(pt_x-1, pt_y);
            check_neibour(pt_x+1, pt_y);
            check_neibour(pt_x, pt_y-1);
            check_neibour(pt_x, pt_y+1);
        }
    }
}
