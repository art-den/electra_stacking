use crate::image_raw::*;

// Color coefficients for camera sensors
const SONY_IMX294_WB: [f32; 4] = [1.216, 1.000, 1.424, 0.000];
const SONY_IMX294_CCM: &[f32; 9] = &[
     1.89859, -0.78759, -0.26672,
     0.12546,  0.70569,  0.06947,
     0.01381,  0.05857,  0.50326,
];

const SONY_IMX455_WB: [f32; 4] = [1.186, 1.000, 1.347, 0.000];
const SONY_IMX455_CCM: &[f32; 9] = &[
     1.82345, -0.73083, -0.27351,
    -0.17026,  0.87025,  0.14144,
    -0.02383,  0.05566,  0.53220,
];

const SONY_IMX178_WB: [f32; 4] = [1.292, 1.000, 1.387, 0.000];
const SONY_IMX178_CCM: &[f32; 9] = &[
     1.71221, -0.66253, -0.23873,
     0.06362,  0.75193,  0.11160,
     0.03049,  0.04920,  0.53220,
];

const SONY_IMX571_WB: [f32; 4] = [1.11, 1.00, 1.25, 0.00];  // ???
const SONY_IMX071_WB: [f32; 4] = [1.11, 1.00, 1.18, 0.00];  // ???
const SONY_IMX183_WB: [f32; 4] = [1.04, 1.00, 1.25, 0.00];  // ???
const SONY_IMX533_WB: [f32; 4] = [1.11, 1.00, 1.23, 0.00];  // ???
const SONY_IMX410_WB: [f32; 4] = [1.14, 1.00, 1.25, 0.00];  // ???

// table for cameras if no params in raw FITS file
const CAMERAS_TABLE: &[(&str, [f32; 4], Option<CfaType>, Option<&[f32; 9]>)] = &[
    // camera     color balance   bayer type           color matrix
    ("asi294mc",  SONY_IMX294_WB, Some(CfaType::RGGB), Some(SONY_IMX294_CCM)),
    ("asi2600mc", SONY_IMX571_WB, Some(CfaType::RGGB), None),
    ("asi071mc",  SONY_IMX071_WB, None,                None),
    ("asi183mc",  SONY_IMX183_WB, None,                None),
    ("asi533mc",  SONY_IMX533_WB, None,                None),
    ("asi6200mc", SONY_IMX455_WB, None,                Some(SONY_IMX455_CCM)),
    ("asi2400mc", SONY_IMX410_WB, None,                None),
    ("asi178mc",  SONY_IMX178_WB, None,                Some(SONY_IMX178_CCM)),
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
