use std::path::*;
use electra_stacking::{image::*, image_raw::*, image_formats::*};



fn main() -> anyhow::Result<()> {
    let args: Vec<_> = std::env::args().collect();
    let file_name = Path::new(&args[1]);

    if let RawOrImage::Image(image) = load_image_from_file(file_name, false)?.image {
        let mut lin_result = make_bayer_image(&image).demosaic(DemosaicAlgo::Linear, false)?;
        calc_and_show_error("Linear", &image, &lin_result);
        correct_image(&mut lin_result);
        save_image_to_file(&lin_result, &ImageInfo::default(), &file_name.with_extension("linear.tif"))?;

        let mut green_result = make_bayer_image(&image).demosaic(DemosaicAlgo::ColorRatio, false)?;
        calc_and_show_error("Color ratio", &image, &green_result);
        correct_image(&mut green_result);
        save_image_to_file(&green_result, &ImageInfo::default(), &file_name.with_extension("color-ratio.tif"))?;

        println!("Done!");
    }

    Ok(())
}

fn make_bayer_image(image: &Image) -> RawImage {
    let mut info = RawImageInfo::default();

    info.width = image.width();
    info.height = image.height();
    info.max_values = [1.0; 4];
    info.black_values = [0.0; 4];
    info.wb = [1.0; 4];
    info.cfa = Cfa::from_str("RGGB");

    let mut raw = RawImage::new_from_info(info);

    for (d, (x, y, r, g, b)) in raw.data.iter_mut().zip(image.iter_rgb_crd()) {
        *d = match raw.info.cfa.get_pixel_color(x, y) {
            CfaColor::R => r,
            CfaColor::G => g,
            CfaColor::B => b,
            _ => panic!("Internal error"),
        }
    }

    raw
}

fn correct_image(image: &mut Image) {
    for v in image.r.iter_mut() {
        if *v < 0.0 { *v = 0.0; }
        if *v > 1.0 { *v = 1.0; }
    }

    for v in image.g.iter_mut() {
        if *v < 0.0 { *v = 0.0; }
        if *v > 1.0 { *v = 1.0; }
    }

    for v in image.b.iter_mut() {
        if *v < 0.0 { *v = 0.0; }
        if *v > 1.0 { *v = 1.0; }
    }
}

fn calc_and_show_error(caption: &str, orig_image: &Image, image: &Image) {
    let calc_error = |org_layer: &ImageLayerF32, layer: &ImageLayerF32| -> f64 {
        let sum = org_layer.iter().zip(layer.iter())
            .map(|(v1, v2)| (v1-v2) * (v1-v2))
            .fold(0_f64, |acc, v| acc + v as f64) as f64;

        (sum / org_layer.as_slice().len() as f64).sqrt()
    };

    let r_error = calc_error(&orig_image.r, &image.r);
    let g_error = calc_error(&orig_image.g, &image.g);
    let b_error = calc_error(&orig_image.b, &image.b);

    println!(
        "{} errors: R = {:.3}%, G = {:.3}%, B = {:.3}%",
        caption,
        100.0 * r_error,
        100.0 * g_error,
        100.0 * b_error
    );

}