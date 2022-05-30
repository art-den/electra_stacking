use structopt::*;
use clap::arg_enum;
use serde::{Serialize, Deserialize};

arg_enum! {
    #[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
    pub enum CalcMode {
        CappaSigma,
        Median,
        Mean,
    }
}

#[derive(StructOpt, Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct CalcOpts {
    /// Calculation mode
    #[structopt(
        long,
        default_value = "CappaSigma",
        possible_values = &CalcMode::variants(),
        case_insensitive = true
    )]
    pub mode: CalcMode,

    /// Kappa for cappa-sigma mode
    #[structopt(long, default_value = "2.5")]
    pub kappa: f32,

    /// repeats count for cappa-sigma mode
    #[structopt(long, default_value = "5")]
    pub repeats: u32
}

impl Default for CalcOpts {
    fn default() -> Self {
        Self {
            mode: CalcMode::CappaSigma,
            kappa: 2.5,
            repeats: 5,
        }
    }
}

#[inline]
pub fn cmp_f32(v1: &f32, v2: &f32) -> core::cmp::Ordering {
    if *v1 < *v2 { core::cmp::Ordering::Less }
    else if *v1 > *v2 { core::cmp::Ordering::Greater }
    else { core::cmp::Ordering::Equal }
}

#[inline]
pub fn cmp_f64(v1: &f64, v2: &f64) -> core::cmp::Ordering {
    if *v1 < *v2 { core::cmp::Ordering::Less }
    else if *v1 > *v2 { core::cmp::Ordering::Greater }
    else { core::cmp::Ordering::Equal }
}

#[derive(Debug)]
pub struct CalcValue {
    pub value: f64,
    pub weight: f64,
    pub used: bool, // for internal use
}

impl CalcValue {
    pub fn new(value: f64) -> CalcValue {
        CalcValue {value, weight: 1.0, used: true}
    }

    pub fn new_weighted(value: f64, weight: f64) -> CalcValue {
        CalcValue {value, weight, used: true}
    }
}

pub struct CalcResult {
    pub result: f64,
    pub discarded: u64,
}

pub fn mean(values: &[CalcValue]) -> Option<f64> {
    if values.is_empty() { return None; }
    let mut sum: f64 = 0.0;
    let mut cnt: f64 = 0.0;
    for v in values.iter() {
        if !v.used { continue; }
        sum += v.value * v.weight;
        cnt += v.weight;
    }
    Some(sum/cnt)
}

pub fn mean_result(values: &[CalcValue]) -> Option<CalcResult> {
    mean(values)
        .map(|v| CalcResult{ result: v, discarded: 0 })
}

pub fn mean_and_std_dev(values: &[CalcValue]) -> Option<(f64, f64)> {
    if values.is_empty() { return None; }
    if let Some(m) = mean(values) {
        let dev = values.iter().fold(0_f64, |acc, v| acc + (v.value-m) * (v.value-m));
        let std_dev = f64::sqrt(dev / values.len() as f64);
        Some((m, std_dev))
    } else {
        None
    }
}

pub fn mean_f64(values: &[f64]) -> f64 {
    let sum: f64 = values.iter().sum();
    sum / values.len() as f64
}

pub fn median_f64(values: &mut [f64]) -> Option<f64> {
    if values.is_empty() { return None; }
    let median_index = values.len() / 2;
    values.select_nth_unstable_by(median_index, cmp_f64);
    Some(values[median_index])
}

pub fn median_result(values: &mut [CalcValue]) -> Option<CalcResult> {
    if values.is_empty() { return None; }

    let median_index = values.len() / 2;
    values.select_nth_unstable_by( median_index, |a, b| cmp_f64(&a.value, &b.value));

    Some(CalcResult{
        result: values[median_index].value,
        discarded: 0,
    })
}

pub fn cappa_sigma_weighted_result(
    values:  &mut [CalcValue],
    kappa:   f32,
    repeats: u32,
    low:     bool,
    high:    bool
) -> Option<CalcResult> {
    if values.is_empty() { return None; }
    if values.len() == 1 { return Some(CalcResult { result: values[0].value, discarded: 0, }); }
    let kappa = kappa as f64;

    for v in values.iter_mut() { v.used = true; }

    for _ in 0..repeats {
        if let Some((mean, std_dev)) = mean_and_std_dev(values) {
            let max = mean + kappa * std_dev;
            let min = mean - kappa * std_dev;
            let mut changed = false;
            for v in values.iter_mut() {
                if !v.used { continue; }
                if (low && v.value < min) || (high && v.value > max) {
                    v.used = false;
                    changed = true;
                }
            }
            if !changed { break; }
        } else {
            return None;
        }
    }

    let mut sum: f64 = 0.0;
    let mut cnt: f64 = 0.0;
    let mut discarded_cnt: u64 = 0;
    for v in values.iter() {
        if v.used {
            sum += v.value * v.weight;
            cnt += v.weight;
        } else {
            discarded_cnt += 1;
        }
    }
    if cnt == 0.0 { return None; }

    Some(CalcResult { result: sum/cnt, discarded: discarded_cnt })
}

pub fn calc(values: &mut [CalcValue], opts: &CalcOpts) -> Option<CalcResult> {
    match opts.mode {
        CalcMode::Median =>
            median_result(values),

        CalcMode::CappaSigma =>
            cappa_sigma_weighted_result(values, opts.kappa, opts.repeats, true, true),

        CalcMode::Mean =>
            mean_result(values),
    }
}

pub struct IirFilter {
    a0: f32,
    b0: f32,
    b1: f32,
    b2: f32,

    y0: f32,
    y1: f32,
    y2: f32,
    first_time: bool,
}

impl IirFilter {
    pub fn new_gauss(sigma: f32) -> IirFilter {
        let q = if sigma >= 2.5 {
            0.98711 * sigma - 0.96330
        } else if (0.5..2.5).contains(&sigma) {
            3.97156 - 4.14554 * (1.0 - 0.26891 * sigma).sqrt()
        } else {
            0.1147705018520355224609375
        };

        let q2 = q * q;
        let q3 = q * q2;

        let  b0 = 1.0 / (1.57825 + (2.44413 * q) + (1.4281 * q2) + (0.422205 * q3));
        let  b1 = ((2.44413 * q) + (2.85619 * q2) + (1.26661 * q3)) * b0;
        let  b2 = (-((1.4281 * q2) + (1.26661 * q3))) * b0;
        let  b3 = (0.422205 * q3) * b0;
        let  a = 1.0 - (b1 + b2 + b3);

        IirFilter {
            a0: a,
            b0: b1,
            b1: b2,
            b2: b3,
            y0: 0.0,
            y1: 0.0,
            y2: 0.0,
            first_time: true
        }
    }

    pub fn init(&mut self) {
        self.first_time = true;
    }

    pub fn filter(&mut self, x: f32) -> f32 {
        if self.first_time {
            self.first_time = false;
            self.y0 = x;
            self.y1 = x;
            self.y2 = x;
        }
        let result =
            self.a0 * x +
            self.b0 * self.y0 +
            self.b1 * self.y1 +
            self.b2 * self.y2;
        self.y2 = self.y1;
        self.y1 = self.y0;
        self.y0 = result;
        result
    }
}

pub fn iir_filter(filter: &mut IirFilter, src: &[f32], dst: &mut [f32]) {
    filter.init();
    for (s, d) in src.iter().zip(dst.iter_mut()) {
        *d = filter.filter(*s);
    }

    filter.init();
    for (s, d) in src.iter().zip(dst.iter_mut()).rev() {
        *d = filter.filter(*s);
    }
}

pub fn rotate_point(x: f64, y: f64, x0: f64, y0: f64, angle: f64) -> (f64, f64) {
    let dx = x - x0;
    let dy = y - y0;
    let cos_a = f64::cos(angle);
    let sin_a = f64::sin(angle);
    (x0 + dx * cos_a - dy * sin_a,
     y0 + dy * cos_a + dx * sin_a)
}

fn det2(
    a11: f64, a12: f64,
    a21: f64, a22: f64
) -> f64 {
    a11 * a22 - a12 * a21
}

fn det3(
    a11: f64, a12: f64, a13: f64,
    a21: f64, a22: f64, a23: f64,
    a31: f64, a32: f64, a33: f64
) -> f64 {
    a11 * det2(a22, a23, a32, a33) -
    a12 * det2(a21, a23, a31, a33) +
    a13 * det2(a21, a22, a31, a32)
}

fn linear_solve3(
    a11: f64, a12: f64, a13: f64, b1: f64,
    a21: f64, a22: f64, a23: f64, b2: f64,
    a31: f64, a32: f64, a33: f64, b3: f64
) -> Option<(f64, f64, f64)> {
    let det = det3(
        a11, a12, a13,
        a21, a22, a23,
        a31, a32, a33
    );

    if det == 0.0 {
        return None;
    }

    let det1 = det3(
        b1, a12, a13,
        b2, a22, a23,
        b3, a32, a33
    );

    let det2 = det3(
        a11, b1, a13,
        a21, b2, a23,
        a31, b3, a33
    );

    let det3 = det3(
        a11, a12, b1,
        a21, a22, b2,
        a31, a32, b3
    );

    Some((det1/det, det2/det, det3/det))
}

pub struct Cubic {
    pub a2: f64,
    pub a1: f64,
    pub a0: f64,
}

impl Cubic {
    pub fn calc(&self, x: f64) -> f64 {
        self.a2*x*x + self.a1*x + self.a0
    }
}

pub fn cubic_ls(x_values: &[f64], y_values: &[f64]) -> Option<Cubic> {
    assert!(x_values.len() == y_values.len());
    if x_values.len() < 3 { return None; }

    let mut sum_x = 0_f64;
    let mut sum_x2 = 0_f64;
    let mut sum_x3 = 0_f64;
    let mut sum_x4 = 0_f64;
    let mut sum_y = 0_f64;
    let mut sum_xy = 0_f64;
    let mut sum_x2y = 0_f64;

    for (&x, &y) in x_values.iter().zip(y_values) {
        let x2 = x * x;
        let x3 = x2 * x;
        let x4 = x3 * x;

        sum_x += x;
        sum_x2 += x2;
        sum_x3 += x3;
        sum_x4 += x4;
        sum_y += y;
        sum_xy += x * y;
        sum_x2y += x2 * y;
    }

    linear_solve3(
        x_values.len() as f64, sum_x,  sum_x2, sum_y,
        sum_x,                 sum_x2, sum_x3, sum_xy,
        sum_x2,                sum_x3, sum_x4, sum_x2y,
    ).map(|coeffs| {
        Cubic {a0: coeffs.0, a1: coeffs.1, a2: coeffs.2,}
    })
}

pub fn linear_interpol(x: f64, x1: f64, x2: f64, y1: f64, y2: f64) -> f64 {
    (x - x1) * (y2 - y1) / (x2 - x1) + y1
}

struct InterpolTableItem {
    x: f32,
    a: f32,
    b: f32,
}

impl InterpolTableItem {
    fn get(&self, x: f32) -> f32 {
        self.a * x + self.b
    }
}

pub struct InterpolTable {
    data: Vec<(f32, f32)>,
    items: Vec<InterpolTableItem>,
    min_x: f32,
}

impl InterpolTable {
    pub fn new() -> InterpolTable {
        InterpolTable {
            data: Vec::new(),
            items: Vec::new(),
            min_x: 0.0,
        }
    }

    pub fn add(&mut self, x: f32, y: f32) {
        self.data.push((x, y));
    }

    pub fn prepare(&mut self) {
        self.data.sort_by(|i1, i2| cmp_f32(&i1.0, &i2.0));

        self.items.clear();
        for i in 0..self.data.len()-1 {
            let i1 = &self.data[i];
            let i2 = &self.data[i+1];

            let a = (i2.1 - i1.1) / (i2.0 - i1.0);
            let b = i1.1 - a * i1.0;

            self.items.push(InterpolTableItem {
                a, b, x: i2.0
            });
        }

        self.min_x = self.items[0].x;
    }

    #[inline(always)]
    pub fn get(&self, x: f32) -> f32 {
        if x < self.min_x {
            return self.items[0].get(x);
        }
        for item in self.items.iter() {
            if x <= item.x {
                return item.get(x);
            }
        }
        self.items[self.items.len()-1].get(x)
    }
}
