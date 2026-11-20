use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum CalcMode {
    CappaSigma,
    Median,
    Mean,
}

/// Hard lower bound for sigma-clip `kappa` (`.ui` hint labels show recommended ranges).
pub const MIN_KAPPA: f32 = 1.0;

/// Hard lower bound for sigma-clip repeats count.
pub const MIN_REPEATS: u32 = 1;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct CalcOpts {
    /// Calculation mode
    pub mode: CalcMode,

    /// Kappa for cappa-sigma mode (>= `MIN_KAPPA`)
    pub kappa: f32,

    /// repeats count for cappa-sigma mode
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

impl CalcOpts {
    pub fn to_short_str(&self) -> String {
        match self.mode {
            CalcMode::CappaSigma =>
                format!("sigma_clip{:.1}_{}", self.kappa, self.repeats),
            CalcMode::Median =>
                "median".to_string(),
            CalcMode::Mean =>
                "mean".to_string(),
        }
    }
}

/// Total order via `f32::total_cmp`: sign-bit-clear NaN sorts last,
/// sign-bit-set NaN first; `-0.0` before `+0.0`.
#[inline]
pub fn cmp_f32(v1: &f32, v2: &f32) -> core::cmp::Ordering {
    f32::total_cmp(v1, v2)
}

/// Same as `cmp_f32`, for `f64`.
#[inline]
pub fn cmp_f64(v1: &f64, v2: &f64) -> core::cmp::Ordering {
    f64::total_cmp(v1, v2)
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
    let mut cnt = 0_usize;
    let mut sum = 0_f64;
    for v in values {
        if !v.used { continue; }
        sum += v.value;
        cnt += 1;
    }
    if cnt != 0 {
        Some(sum / cnt as f64)
    } else {
        None
    }
}

pub fn mean_weighted(values: &[CalcValue]) -> Option<f64> {
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

pub fn mean_weighted_result(values: &[CalcValue]) -> Option<CalcResult> {
    mean_weighted(values)
        .map(|v| CalcResult{ result: v, discarded: 0 })
}

pub fn mean_and_std_dev(values: &[CalcValue]) -> Option<(f64, f64)> {
    if values.is_empty() { return None; }
    if let Some(m) = mean(values) {
        let mut sum = 0_f64;
        let mut cnt = 0_usize;
        for v in values.iter() {
            if !v.used { continue; }
            sum += (v.value-m) * (v.value-m);
            cnt += 1;
        }
        let std_dev = f64::sqrt(sum / cnt as f64);
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

pub fn median_f32(values: &mut [f32]) -> Option<f32> {
    if values.is_empty() { return None; }
    let median_index = values.len() / 2;
    values.select_nth_unstable_by(median_index, cmp_f32);
    Some(values[median_index])
}

pub fn median_i64(values: &mut [i64]) -> Option<i64> {
    if values.is_empty() { return None; }
    let median_index = values.len() / 2;
    values.select_nth_unstable(median_index);
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

/// Weighted mean with sigma clipping. Returns `None` only for empty input or zero total
/// weight; when clipping would remove all samples, it stops at the last non-empty subset.
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

            // Count-then-apply two-pass: avoids per-pixel allocation for mark rollback.
            let mut used_cnt   = 0_usize;
            let mut clipped_cnt = 0_usize;
            for v in values.iter() {
                if !v.used { continue; }
                used_cnt += 1;
                if (low && v.value < min) || (high && v.value > max) {
                    clipped_cnt += 1;
                }
            }
            // Stop if nothing was clipped or if all remaining samples would be clipped:
            // keep the last non-empty subset (defensive against invalid kappa).
            if clipped_cnt == 0 || clipped_cnt == used_cnt { break; }

            for v in values.iter_mut() {
                if !v.used { continue; }
                if (low && v.value < min) || (high && v.value > max) {
                    v.used = false;
                }
            }
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
            mean_weighted_result(values),
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
            0.1147705
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

/// Two-pass (forward + reverse) recursive filter approximating a zero-phase
/// Gaussian smoothing. `src` and `dst` must have the same length.
pub fn iir_filter(filter: &mut IirFilter, src: &[f32], dst: &mut [f32]) {
    debug_assert_eq!(src.len(), dst.len());
    filter.init();
    for (s, d) in src.iter().zip(dst.iter_mut()) {
        *d = filter.filter(*s);
    }

    filter.init();
    // Reverse pass filters the forward pass output in place, giving a symmetric response.
    for d in dst.iter_mut().rev() {
        *d = filter.filter(*d);
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

pub struct SquareCoeffs {
    pub a2: f64,
    pub a1: f64,
    pub a0: f64,
}

impl SquareCoeffs {
    pub fn calc(&self, x: f64) -> f64 {
        self.a2*x*x + self.a1*x + self.a0
    }
}

pub fn square_ls(x_values: &[f64], y_values: &[f64]) -> Option<SquareCoeffs> {
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
        SquareCoeffs {a0: coeffs.0, a1: coeffs.1, a2: coeffs.2,}
    })
}

#[inline(always)]
pub fn linear_interpol(x: f64, x1: f64, x2: f64, y1: f64, y2: f64) -> f64 {
    (x - x1) * (y2 - y1) / (x2 - x1) + y1
}

pub fn fast_pow(a: f64, b: f64) -> f64 {
    if a < 0.0 { return 0.0; }
    let tmp = a.to_bits() as i64;
    let tmp2 = (b * (tmp - 4606921280493453312_i64) as f64) as i64 + 4606921280493453312_i64;
    f64::from_bits(tmp2 as u64)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Negative quiet NaN: sign bit and mantissa bits set.
    const NEG_NAN_F64: f64 = f64::from_bits(0xFFF8_0000_0000_0000);
    const NEG_NAN_F32: f32 = f32::from_bits(0xFFC0_0000);

    #[test]
    fn cmp_f64_is_a_total_order_with_special_values() {
        // Ordered ascending per total_cmp bitwise order.
        let values: Vec<f64> = vec![
            NEG_NAN_F64, f64::NEG_INFINITY, -1.0, -0.0, 0.0, 1.0, f64::INFINITY, f64::NAN,
        ];
        for (i, a) in values.iter().enumerate() {
            for (j, b) in values.iter().enumerate() {
                assert_eq!(
                    cmp_f64(a, b),
                    usize::cmp(&i, &j),
                    "bad order for bits {:x} vs {:x}",
                    f64::to_bits(*a),
                    f64::to_bits(*b)
                );
                assert_eq!(cmp_f64(b, a), cmp_f64(a, b).reverse(), "antisymmetry broken");
            }
        }
    }

    #[test]
    fn median_f64_with_nan_is_deterministic() {
        // The nth slot must hold the sorted nth element (verified against std total_cmp).
        let base: Vec<f64> = vec![4.0, 8.0, 2.0, 9.0, 1.0, 7.0, 3.0, 6.0, 5.0];
        let mut v = base.clone();
        v[2] = f64::NAN;
        assert_eq!(median_f64(&mut v), Some(6.0));
        let mut w = base;
        w[2] = NEG_NAN_F64;
        assert_eq!(median_f64(&mut w), Some(5.0));

        let mut x = vec![5.0, 1.0, NEG_NAN_F64, 3.0, 4.0];
        assert_eq!(median_f64(&mut x), Some(3.0));
        let mut y = vec![5.0, 1.0, f64::NAN, 3.0, 4.0];
        assert_eq!(median_f64(&mut y), Some(4.0));

        let mut z = vec![1.0, 2.0, f64::NAN, f64::NAN, f64::NAN];
        assert!(median_f64(&mut z).unwrap().is_nan());
    }

    #[test]
    fn median_f32_with_nan_is_deterministic() {
        let mut v: Vec<f32> = vec![4.0, 8.0, NEG_NAN_F32, 9.0, 1.0, 7.0, 3.0, 6.0, 5.0];
        assert_eq!(median_f32(&mut v), Some(5.0));
        let mut w: Vec<f32> = vec![4.0, 8.0, f32::NAN, 9.0, 1.0, 7.0, 3.0, 6.0, 5.0];
        assert_eq!(median_f32(&mut w), Some(6.0));
    }

    #[test]
    fn iir_filter_gauss_impulse_response_is_centered_and_symmetric() {
        const N: usize = 400;
        const CENTER: usize = N / 2;
        const SIGMA: f32 = 42.0;

        let mut src = vec![0.0f32; N];
        src[CENTER] = 1.0;
        let mut dst = vec![0.0f32; N];
        let mut filter = IirFilter::new_gauss(SIGMA);
        iir_filter(&mut filter, &src, &mut dst);

        let peak = dst.iter().enumerate()
            .max_by(|a, b| f32::total_cmp(a.1, b.1)).unwrap();
        assert_eq!(peak.0, CENTER);

        let expected_peak = 1.0 / (SIGMA * f32::sqrt(std::f32::consts::TAU));
        assert!(f32::abs(dst[CENTER] / expected_peak - 1.0) < 0.05);

        // Check symmetry only within ~2.5 sigma of the peak: near the array edges
        // the first_time state init transient dominates the tiny Gaussian tail.
        for k in 1..=100 {
            let l = dst[CENTER - k];
            let r = dst[CENTER + k];
            let m = f32::max(l, r);
            assert!(f32::abs(l - r) / m < 0.05, "asymmetric at offset {}", k);
        }
    }

    #[test]
    fn iir_filter_preserves_constant_signal() {
        let src = vec![7.5f32; 100];
        let mut dst = vec![0.0f32; src.len()];
        let mut filter = IirFilter::new_gauss(42.0);
        iir_filter(&mut filter, &src, &mut dst);
        for &v in &dst {
            // Loose bound: f32 cancellation in the IIR recursion is ~1e-4 of the signal.
            assert!(f32::abs(v - 7.5) < 7.5e-3);
        }
    }

    #[test]
    fn median_result_with_nan_selects_deterministic_slot() {
        let make = |vals: &[f64]| -> Vec<CalcValue> {
            vals.iter().map(|&v| CalcValue::new(v)).collect()
        };
        let mut values = make(&[4.0, 8.0, f64::NAN, 9.0, 1.0, 7.0, 3.0, 6.0, 5.0]);
        assert_eq!(median_result(&mut values).unwrap().result, 6.0);
        let mut values2 = make(&[1.0, 2.0, f64::NAN, f64::NAN, f64::NAN]);
        assert!(median_result(&mut values2).unwrap().result.is_nan());
    }

    #[test]
    fn cappa_sigma_keeps_last_nonempty_subset_on_invalid_kappa() {
        let make = |vals: &[f64]| -> Vec<CalcValue> {
            vals.iter().map(|&v| CalcValue::new(v)).collect()
        };

        // Negative kappa makes min > max: every sample would be clipped in one
        // iteration; must fall back to the full set instead of returning None.
        let mut values = make(&[1.0, 2.0, 3.0, 4.0, 5.0]);
        let res = cappa_sigma_weighted_result(&mut values, -3.0, 5, true, true).unwrap();
        assert_eq!(res.result, 3.0);
        assert_eq!(res.discarded, 0);

        // 0 <= kappa < 1 also empties typical sample sets over the repeats.
        let mut values = make(&[1.0, 2.0, 3.0, 4.0, 5.0]);
        let res = cappa_sigma_weighted_result(&mut values, 0.0, 5, true, true).unwrap();
        assert_eq!(res.result, 3.0);
        assert_eq!(res.discarded, 4);

        // NaN/inf kappa: comparisons are false, nothing gets clipped -> plain mean.
        for kappa in [f32::NAN, f32::INFINITY] {
            let mut values = make(&[1.0, 2.0, 3.0, 4.0, 5.0]);
            let res = cappa_sigma_weighted_result(&mut values, kappa, 5, true, true).unwrap();
            assert_eq!(res.result, 3.0);
            assert_eq!(res.discarded, 0);
        }

        // Normal clipping is unaffected.
        let mut values = make(&[1.0, 2.0, 3.0, 4.0, 5.0, 100.0]);
        let res = cappa_sigma_weighted_result(&mut values, 2.0, 5, true, true).unwrap();
        assert_eq!(res.result, 3.0);
        assert_eq!(res.discarded, 1);
        assert!(!values[5].used);
    }
}

