use crate::image_raw::*;

// Color coefficients for camera sensors
const SONY_IMX294_WB: [f32; 4] = [1.255, 1.000, 1.607, 0.000];
const SONY_IMX294_CCM: &[f32; 9] = &[
     1.13647, -0.54132,  0.08907,
    -0.10207,  1.79176, -0.56323,
    -0.03440, -0.25044,  1.47416,
];

const SONY_IMX455_WB: [f32; 4] = [1.225, 1.000, 1.526, 0.000];
const SONY_IMX455_CCM: &[f32; 9] = &[
     1.09863, -0.32455,  0.05725,
    -0.09183,  1.58883, -0.44144,
    -0.00681, -0.26429,  1.38419,
];

const SONY_IMX178_WB: [f32; 4] = [1.332, 1.000, 1.572, 0.000];
const SONY_IMX178_CCM: &[f32; 9] = &[
     1.19315, -0.50011,  0.05137,
    -0.16439,  1.79350, -0.52794,
    -0.02877, -0.29339,  1.47656,
];

const SONY_IMX183_WB: [f32; 4] = [1.293, 1.000, 1.574, 0.000];
const SONY_IMX183_CCM: &[f32; 9] = &[
     1.19935, -0.48885,  0.06163,
    -0.16574,  1.77902, -0.53861,
    -0.03362, -0.29018,  1.47698,
];

const SONY_IMX571_WB: [f32; 4] = [1.251, 1.000, 1.548, 0.000];
const SONY_IMX571_CCM: &[f32; 9] = &[
     1.08988, -0.33464,  0.07279,
    -0.08379,  1.60281, -0.50702,
    -0.00609, -0.26817,  1.43424,
];

// table for cameras if no params in raw FITS file
const CAMERAS_TABLE: &[(&str, [f32; 4], Option<CfaType>, Option<&[f32; 9]>)] = &[
    // camera    |color balance  |bayer|color matrix
    ("asi294mc",         SONY_IMX294_WB, None, Some(SONY_IMX294_CCM)),
    ("sv405cc",          SONY_IMX294_WB, None, Some(SONY_IMX294_CCM)),
    ("asi6200mc",        SONY_IMX455_WB, None, Some(SONY_IMX455_CCM)),
    ("asi178mc",         SONY_IMX178_WB, None, Some(SONY_IMX178_CCM)),
    ("asi183mc",         SONY_IMX183_WB, None, Some(SONY_IMX183_CCM)),
    ("atr3cmos26000kpa", SONY_IMX571_WB, None, Some(SONY_IMX571_CCM)),
];

pub fn find_camera_params(
    camera_name: Option<&str>
) -> Option<([f32; 4], Option<CfaType>, Option<&[f32; 9]>)> {
    if let Some(camera_name) = camera_name {
        let name_lc = camera_name.to_string().to_lowercase();
        CAMERAS_TABLE.iter()
            .find(|(c, _, _, _)| name_lc.contains(c))
            .map(|(_, wb, cfa, ccm)| (*wb, *cfa, *ccm))
    } else {
        None
    }
}
