use std::collections::{HashSet, VecDeque};
use itertools::izip;
use rayon::prelude::*;
use crate::calc::*;

pub const NO_VALUE_F32: f32 = -999.0;

pub type Crd = i64;

#[derive(PartialEq, Clone, Copy)]
pub enum IterType {
    Cols,
    Rows,
    Both,
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

    pub fn new_from_vec(width: Crd, height: Crd, data: Vec<T>) -> ImageLayer<T> {
        assert!((width * height) as usize == data.len());
        ImageLayer::<T>{
            data,
            width,
            height
        }
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

    pub fn fill(&mut self, value: T) {
        self.data.fill(value);
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
        self.data.get(index).copied()
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

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.data.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> {
        self.data.iter_mut()
    }

    pub fn row(&self, y: Crd) -> &[T]{
        let pos = (y*self.width) as usize;
        &self.data[pos..pos + self.width as usize]
    }

    pub fn row_mut(&mut self, y: Crd) -> &mut [T]{
        let pos = (y*self.width) as usize;
        &mut self.data[pos..pos + self.width as usize]
    }

    pub fn iter_row(&self, y: Crd) -> std::slice::Iter<T> {
        let pos = (y*self.width) as usize;
        self.data[pos..pos + self.width as usize].iter()
    }

    pub fn iter_row_mut(&mut self, y: Crd) -> std::slice::IterMut<T> {
        let pos = (y*self.width) as usize;
        self.data[pos..pos + self.width as usize].iter_mut()
    }

    pub fn iter_col(&self, x: Crd) -> std::iter::StepBy<std::slice::Iter<T>> {
        self.data[x as usize..].iter().step_by(self.width as usize)
    }

    pub fn iter_col_mut(&mut self, x: Crd) -> std::iter::StepBy<std::slice::IterMut<T>> {
        self.data[x as usize..].iter_mut().step_by(self.width as usize)
    }

    fn get_end_for_from_top_iter_diag1(&self, x: Crd) -> usize {
        let size_diff = self.width - self.height;
        if x > size_diff {
            (self.width * (self.height - (x - size_diff)) - 1) as usize
        } else {
            self.data.len() - 1
        }
    }

    pub fn from_top_iter_diag1(&self, x: Crd) -> std::iter::StepBy<std::slice::Iter<T>> {
        let end = self.get_end_for_from_top_iter_diag1(x);
        self.data[x as usize ..= end]
            .iter()
            .step_by(self.width as usize + 1)
    }

    pub fn from_top_iter_diag1_mut(&mut self, x: Crd) -> std::iter::StepBy<std::slice::IterMut<T>> {
        let end = self.get_end_for_from_top_iter_diag1(x);
        self.data[x as usize ..= end]
            .iter_mut()
            .step_by(self.width as usize + 1)
    }

    fn get_end_for_from_left_diag1_iter(&self, y: Crd) -> usize {
        let size_diff = self.height - self.width;
        if y < size_diff {
            (self.width * (self.height - (size_diff - y)) - 1) as usize
        } else {
            self.data.len() - 1
        }
    }

    pub fn from_left_iter_diag1(&self, y: Crd) -> std::iter::StepBy<std::slice::Iter<T>> {
        let end = self.get_end_for_from_left_diag1_iter(y);
        self.data[(y*self.width) as usize ..= end]
            .iter()
            .step_by(self.width as usize + 1)
    }

    pub fn from_left_iter_diag1_mut(&mut self, y: Crd) -> std::iter::StepBy<std::slice::IterMut<T>> {
        let end = self.get_end_for_from_left_diag1_iter(y);
        self.data[(y*self.width) as usize ..= end]
            .iter_mut()
            .step_by(self.width as usize + 1)
    }

    fn get_end_for_from_top_iter_diag2(&self, x: Crd) -> usize {
        if x < self.height {
            (self.width * x) as usize
        } else {
            self.data.len() - 1
        }
    }

    pub fn from_top_iter_diag2(&self, x: Crd) -> std::iter::StepBy<std::slice::Iter<T>> {
        let end = self.get_end_for_from_top_iter_diag2(x);
        self.data[x as usize ..= end]
            .iter()
            .step_by(self.width as usize - 1)
    }

    pub fn from_top_iter_diag2_mut(&mut self, x: Crd) -> std::iter::StepBy<std::slice::IterMut<T>> {
        let end = self.get_end_for_from_top_iter_diag2(x);
        self.data[x as usize ..= end]
            .iter_mut()
            .step_by(self.width as usize - 1)
    }

    fn get_end_for_from_right_iter_diag2(&self, y: Crd) -> usize {
        if y <= self.height - self.width {
            (self.width * (y + self.width-1)) as usize
        } else {
            self.data.len() - 1
        }
    }

    pub fn from_right_iter_diag2(&self, y: Crd) -> std::iter::StepBy<std::slice::Iter<T>> {
        let end = self.get_end_for_from_right_iter_diag2(y);
        self.data[(y*self.width + self.width - 1) as usize ..= end]
            .iter()
            .step_by(self.width as usize - 1)
    }

    pub fn from_right_iter_diag2_mut(&mut self, y: Crd) -> std::iter::StepBy<std::slice::IterMut<T>> {
        let end = self.get_end_for_from_right_iter_diag2(y);
        self.data[(y*self.width + self.width - 1) as usize ..= end]
            .iter_mut()
            .step_by(self.width as usize - 1)
    }

    pub fn iter_crd(&self) -> ImageLayerIter1::<T> {
        ImageLayerIter1 {
            iter1: self.data.iter(),
            x: 0,
            y: 0,
            width: self.width,
        }
    }

    pub fn iter_crd_mut(&mut self) -> ImageLayerMutIter1::<T> {
        ImageLayerMutIter1 {
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

    /// Iterate rectangle area
    pub fn iter_rect_crd(&self, x1: Crd, y1: Crd, x2: Crd, y2: Crd) -> RectIterCrd<T> {
        RectIterCrd::new(self, x1, y1, x2, y2)
    }

    pub fn iter_area_crd(&self, area: &RectArea) -> RectIterCrd<T> {
        RectIterCrd::new(self, area.x1, area.y1, area.x2, area.y2)
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.height = 0;
        self.width = 0;
    }
}

pub trait PixelsSource {
    fn get_int_crd(&self, x: Crd, y: Crd) -> Option<f32>;

    fn get_f64_crd(&self, x: f64, y: f64) -> Option<f32> {
        let mut ix = x as Crd;
        let mut dx = (x - ix as f64) as f32;
        if x.is_sign_negative() {
            ix -= 1;
            dx = 1.0+dx;
        }

        let mut iy = y as Crd;
        let mut dy = (y - iy as f64) as f32;
        if y.is_sign_negative() {
            iy -= 1;
            dy = 1.0+dy;
        }

        let p11 = self.get_int_crd(ix, iy);
        let p21 = self.get_int_crd(ix+1, iy);
        let p12 = self.get_int_crd(ix, iy+1);
        let p22 = self.get_int_crd(ix+1, iy+1);

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

        let s_sum = s11 + s21 + s12 + s22;
        if s_sum >= 0.9999 && !sum.is_nan() {
            Some(sum)
        } else if p11.is_none() && p21.is_none() && p12.is_none() && p22.is_none() {
            None
        } else {
            let result = sum / s_sum;
            if !result.is_nan() { Some(result) } else { None }
        }
    }}

pub fn rotated_and_translated(
    source:        &impl PixelsSource,
    angle:         f64,
    transl_x:      f64,
    transl_y:      f64,
    default_value: f32,
    result_width:  Crd,
    result_height: Crd
) -> ImageLayerF32 {
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
        *v = source.get_f64_crd(rot_x, rot_y).unwrap_or(default_value);
    }
    result
}

impl PixelsSource for ImageLayer<f32> {
    fn get_int_crd(&self, x: Crd, y: Crd) -> Option<f32> {
        self.get(x, y)
    }
}

impl ImageLayer<f32> {
    pub fn rotated_and_translated(
        &self,
        angle:         f64,
        transl_x:      f64,
        transl_y:      f64,
        default_value: f32,
        result_width:  Crd,
        result_height: Crd
    ) -> ImageLayerF32 {
        if self.is_empty() { return ImageLayerF32::new_empty(); }
        rotated_and_translated(self, angle, transl_x, transl_y, default_value, result_width, result_height)
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

    pub fn set_novalue_as_median(&mut self) {
        if self.data.is_empty() {
            return;
        }
        let mut data_copy = Vec::new();
        data_copy.extend_from_slice(&self.data);
        data_copy.retain(|v| *v != NO_VALUE_F32);
        let len2 = data_copy.len() / 2;
        let median = *data_copy.select_nth_unstable_by(len2, cmp_f32).1;
        for v in &mut self.data {
            if *v == NO_VALUE_F32 {
                *v = median;
            }
        }
    }

    pub fn fill_with_one_by_mask(&mut self, mask: &ImageLayer<bool>) {
        for (v, m) in self.data.iter_mut().zip(mask.iter()) {
            if *m { *v = 1.0; }
        }
    }

    pub fn check_contains_inf_or_nan(
        &self,
        check_inf: bool,
        check_nan: bool,
        name:      &str
    ) -> anyhow::Result<()> {
        for (x, y, v) in self.iter_crd() {
            if check_inf && v.is_infinite() {
                anyhow::bail!("Image contain INF in {} layer at ({}, {}) point", name, x, y);
            }
            if check_nan && v.is_nan() {
                anyhow::bail!("Image contain NAN in {} layer at ({}, {}) point", name, x, y);
            }
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

    pub fn mark_overexposures(&mut self, overexposures: &Vec<(Crd, Crd)>) {
        for &(x, y) in overexposures {
            self.set_safe(x, y, f32::INFINITY);
        }
    }

    pub fn to_rgb_bytes(&self, min: f32, range: f32, gamma: f32) -> Vec<u8> {
        let gamma_div = 1.0/gamma as f64;
        self.data.par_iter()
            .map(|l| {
                if !l.is_infinite() {
                    let mut l = (256.0 * fast_pow(((l-min)*range) as f64, gamma_div)) as i32;
                    if l < 0 { l = 0; }
                    if l > 255 { l = 255; }
                    let l = l as u8;
                    [l, l, l]
                } else {
                    [0, 255, 0]
                }
            })
            .flatten_iter()
            .collect()
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
            img, x1, x2, y2, x: x1, y: y1,
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

pub struct ToRgbBytesParams {
    l_min: f32,
    r_min: f32,
    g_min: f32,
    b_min: f32,
    range: f32,
}

impl ToRgbBytesParams {
    pub fn new() -> Self {
        Self {
            l_min: 0.0,
            r_min: 0.0,
            g_min: 0.0,
            b_min: 0.0,
            range: 0.0
        }
    }
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

    pub fn iter_l_crd(&self) -> ImageLayerIter1<f32> {
        self.l.iter_crd()
    }

    pub fn iter_l_crd_mut(&mut self) -> ImageLayerMutIter1<f32> {
        self.l.iter_crd_mut()
    }

    pub fn iter_rgb_crd(&self) -> ImageLayerIter3<f32> {
        ImageLayerF32::iter_crd3(&self.r, &self.g, &self.b)
    }

    pub fn iter_rgb_crd_mut(&mut self) -> ImageLayerMutIter3<f32> {
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
            l: self.l.rotated_and_translated(
                angle, transl_x, transl_y,
                default_value,
                result_width, result_height
            ),
            r: self.r.rotated_and_translated(
                angle, transl_x, transl_y,
                default_value,
                result_width, result_height
            ),
            g: self.g.rotated_and_translated(
                angle, transl_x, transl_y,
                default_value,
                result_width, result_height
            ),
            b: self.b.rotated_and_translated(
                angle, transl_x, transl_y,
                default_value,
                result_width, result_height
            ),
        }
    }

    pub fn normalize_to_1(&mut self, if_greater_1: bool) {
        let max = self.l
            .iter()
            .chain(self.r.iter())
            .chain(self.g.iter())
            .chain(self.b.iter())
            .copied()
            .filter(|v| !v.is_infinite())
            .max_by(cmp_f32)
            .unwrap_or(0.0);

        const MAX: f32 = 0.999;

        if max < MAX && if_greater_1 { return; }

        let do_norm = |k, img: &mut ImageLayerF32| {
            for v in img.iter_mut() {
                *v *= k;
            }
        };

        let k = MAX / max;
        do_norm(k, &mut self.l);
        do_norm(k, &mut self.r);
        do_norm(k, &mut self.g);
        do_norm(k, &mut self.b);
    }

    pub fn check_contains_inf_or_nan(&self,
        check_inf: bool,
        check_nan: bool,
    ) -> anyhow::Result<()> {
        self.l.check_contains_inf_or_nan(check_inf, check_nan, "L")?;
        self.r.check_contains_inf_or_nan(check_inf, check_nan, "R")?;
        self.g.check_contains_inf_or_nan(check_inf, check_nan, "G")?;
        self.b.check_contains_inf_or_nan(check_inf, check_nan, "B")?;
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

    pub fn mark_overexposures(&mut self, overexposures: &Vec<(Crd, Crd)>) {
        self.l.mark_overexposures(overexposures);
        self.r.mark_overexposures(overexposures);
        self.g.mark_overexposures(overexposures);
        self.b.mark_overexposures(overexposures);
    }

    pub fn mult_f32(&mut self, value: f32) {
        self.l.mult_f32(value);
        self.r.mult_f32(value);
        self.g.mult_f32(value);
        self.b.mult_f32(value);
    }

    pub fn fill_inf_areas(&mut self) {
        self.l.fill_inf_areas();
        self.r.fill_inf_areas();
        self.g.fill_inf_areas();
        self.b.fill_inf_areas();
    }

    pub fn set_novalue_as_median(&mut self) {
        self.l.set_novalue_as_median();
        self.r.set_novalue_as_median();
        self.g.set_novalue_as_median();
        self.b.set_novalue_as_median();
    }

    pub fn fill_inf_areas_with_one(&mut self) {
        let mut inf_mask = ImageLayer::<bool>::new(
            self.width(),
            self.height()
        );

        for (m, v) in inf_mask.iter_mut().zip(self.l.iter_mut()) {
            if v.is_infinite() { *m = true; }
        }

        for (m, v) in inf_mask.iter_mut().zip(self.r.iter_mut()) {
            if v.is_infinite() { *m = true; }
        }

        for (m, v) in inf_mask.iter_mut().zip(self.g.iter_mut()) {
            if v.is_infinite() { *m = true; }
        }

        for (m, v) in inf_mask.iter_mut().zip(self.b.iter_mut()) {
            if v.is_infinite() { *m = true; }
        }

        self.l.fill_with_one_by_mask(&inf_mask);
        self.r.fill_with_one_by_mask(&inf_mask);
        self.g.fill_with_one_by_mask(&inf_mask);
        self.b.fill_with_one_by_mask(&inf_mask);

        let mut enlarged_inf_mask = ImageLayer::<bool>::new(
            self.width(),
            self.height()
        );

        for (x, y, d) in enlarged_inf_mask.iter_crd_mut() {
            *d = inf_mask
                .iter_rect_crd(x-1, y-1, x+1, y+1)
                .any(|(_, _, v)| { v });
        }

        let process_layer = |img: &mut ImageLayerF32| {
            if img.is_empty() { return; }
            let mut new_img = ImageLayerF32::new(img.width(), img.height());
            for ((x, y, m), s, d) in
            izip!(enlarged_inf_mask.iter_crd(), img.iter(), new_img.iter_mut()) {
                if !m {
                    *d = *s;
                } else {
                    let (sum, cnt) = img
                        .iter_rect_crd(x-1, y-1, x+1, y+1)
                        .fold((0_f64, 0_f64), |(summ, cnt), (_, _, v)| {
                            (summ + v as f64, cnt + 1.0)
                        });

                    *d = if cnt != 0.0 {(sum / cnt) as f32} else { *s };
                }
            }
            img.data = new_img.data;
        };

        process_layer(&mut self.l);
        process_layer(&mut self.r);
        process_layer(&mut self.g);
        process_layer(&mut self.b);

    }

    pub fn clear(&mut self) {
        self.l.clear();
        self.r.clear();
        self.g.clear();
        self.b.clear();
    }

    pub fn calc_to_bytes_params(
        &self,
        auto_minimum: bool,
        auto_wb:      bool
    ) -> ToRgbBytesParams {
        let (l_min, r_min, g_min, b_min, range) = if self.is_rgb() {
            if auto_minimum || auto_wb {
                let get_thinned_out_values = |img_values: &[f32]| -> Vec<f32> {
                    img_values
                        .par_iter()
                        .step_by(42)
                        .filter(|&v| *v > 0.0 && !v.is_infinite())
                        .copied()
                        .collect()
                };
                let mut r_thinned_out = get_thinned_out_values(self.r.as_slice());
                let mut g_thinned_out = get_thinned_out_values(self.g.as_slice());
                let mut b_thinned_out = get_thinned_out_values(self.b.as_slice());

                let get_min = |values: &mut [f32]| -> f32 {
                    if !values.is_empty() {
                        let pos = values.len() / 100;
                        values.select_nth_unstable_by(pos, cmp_f32).1.max(0.0)
                    } else {
                        42.0
                    }
                };

                let min = [
                    get_min(&mut r_thinned_out),
                    get_min(&mut g_thinned_out),
                    get_min(&mut b_thinned_out)
                    ].into_iter()
                    .filter(|v| *v != NO_VALUE_F32)
                    .min_by(cmp_f32)
                    .unwrap_or(0.0);

                let mut r_min = min;
                let mut g_min = min;
                let mut b_min = min;
                let range = 1.0/(1.0 - min);
                if auto_wb {
                    let get_bg = |values: &mut [f32]| -> f32 {
                        if !values.is_empty() {
                            let pos = values.len() / 4;
                            values.select_nth_unstable_by(pos, cmp_f32).1.max(0.0)
                        } else {
                            0.0
                        }
                    };
                    let r_bg = get_bg(&mut r_thinned_out);
                    let g_bg = get_bg(&mut g_thinned_out);
                    let b_bg = get_bg(&mut b_thinned_out);
                    if auto_minimum {
                        let min_bg = [r_bg, g_bg, b_bg]
                            .into_iter()
                            .min_by(cmp_f32)
                            .unwrap_or(0.0);
                        r_min += r_bg-min_bg;
                        g_min += g_bg-min_bg;
                        b_min += b_bg-min_bg;
                    } else {
                        let aver_bg = (r_bg + g_bg + b_bg) / 3.0;
                        r_min += r_bg-aver_bg;
                        g_min += g_bg-aver_bg;
                        b_min += b_bg-aver_bg;
                    }
                }
                (0.0, r_min, g_min, b_min, range)
            } else {
                (0.0, 0.0, 0.0, 0.0, 1.0)
            }
        } else if auto_minimum {
            let mut thinned_out_values: Vec<_> = self.l.as_slice()
                .par_iter()
                .step_by(42)
                .filter(|&v| *v > 0.0 && !v.is_infinite())
                .copied()
                .collect();
            let pos = thinned_out_values.len()/100;
            let min = thinned_out_values.select_nth_unstable_by(pos, cmp_f32).1.max(0.0);
            (min, 0.0, 0.0, 0.0, 1.0/(1.0-min))
        } else {
            (0.0, 0.0, 0.0, 0.0, 1.0)
        };

        ToRgbBytesParams {
            l_min,
            r_min,
            g_min,
            b_min,
            range
        }
    }

    pub fn to_rgb_bytes(&self, params: &ToRgbBytesParams, gamma: f32) -> Vec<u8> {
        let gamma_div = 1.0/gamma as f64;
        if self.is_rgb() {
            self.r.as_slice().par_iter()
                .zip_eq(self.g.as_slice().par_iter())
                .zip_eq(self.b.as_slice().par_iter())
                .map(|((&r, &g), &b)| {
                    if !r.is_infinite() && !g.is_infinite() && !b.is_infinite() {
                        let mut r = (256.0 * fast_pow(((r-params.r_min)*params.range) as f64, gamma_div)) as i32;
                        if r < 0 { r = 0; }
                        if r > 255 { r = 255; }
                        let mut g = (256.0 * fast_pow(((g-params.g_min)*params.range) as f64, gamma_div)) as i32;
                        if g < 0 { g = 0; }
                        if g > 255 { g = 255; }
                        let mut b = (256.0 * fast_pow(((b-params.b_min)*params.range) as f64, gamma_div)) as i32;
                        if b < 0 { b = 0; }
                        if b > 255 { b = 255; }
                        [r as u8, g as u8, b as u8]
                    } else {
                        [0, 255, 0]
                    }
                })
                .flatten_iter()
                .collect()
        } else {
            self.l.to_rgb_bytes(params.l_min, params.range, gamma)
        }
    }

    pub fn apply_wb(&mut self, wb: &[f32; 4]) {
        assert!(!self.is_greyscale());

        let apply = |img: &mut ImageLayerF32, k: f32| {
            for v in img.iter_mut() {
                *v *= k;
            }
        };

        apply(&mut self.r, wb[0]);
        apply(&mut self.g, wb[1]);
        apply(&mut self.b, wb[2]);
    }

    pub fn normalize(&mut self, max_values: &[f32; 4]) {
        let process = |img: &mut ImageLayerF32, max: f32| {
            if max == 0.0 {
                return;
            }
            let k = 1.0 / max;
            for v in img.iter_mut() {
                *v *= k;
            }
        };

        if self.is_greyscale() {
            process(&mut self.l, max_values[0]);
        } else {
            process(&mut self.r, max_values[0]);
            process(&mut self.g, max_values[1]);
            process(&mut self.b, max_values[2]);
        }
    }

    pub fn convert_color_space_to_srgb(&mut self, cam_to_rgb: &Option<[f32; 9]>) {
        if self.is_greyscale() {
            return;
        }

        if let Some(cam_to_rgb) = cam_to_rgb {
            for (r, g, b) in izip!(self.r.iter_mut(), self.g.iter_mut(), self.b.iter_mut()) {
                let r0 = *r;
                let g0 = *g;
                let b0 = *b;
                *r = r0*cam_to_rgb[0] + g0*cam_to_rgb[3] + b0*cam_to_rgb[6];
                *g = r0*cam_to_rgb[1] + g0*cam_to_rgb[4] + b0*cam_to_rgb[7];
                *b = r0*cam_to_rgb[2] + g0*cam_to_rgb[5] + b0*cam_to_rgb[8];
            }
            return;
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
