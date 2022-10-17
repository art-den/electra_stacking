use std::{path::*, io::*, fs::*};
use std::sync::*;
use anyhow::bail;
use bitstream_io::{BigEndian, BitReader};
use crate::{
    compression::*,
    image::*,
    calc::*,
    progress::*,
    image_raw::*,
    image_formats::*,
    fs_utils::*,
    stars::*,
    image_norm::*,
    light_file::*,
    log_utils::*,
};

use std::f64::consts::PI;

///////////////////////////////////////////////////////////////////////////////

/* Stacking bias, darks and flat files */

pub fn create_master_dark_or_bias_file(
    files_list:        &[PathBuf],
    calc_opts:         &CalcOpts,
    result_file:       &Path,
    progress:          &ProgressTs,
    thread_pool:       &rayon::ThreadPool,
    cancel_flag:       &IsCancelledFun,
) -> anyhow::Result<bool> {
    create_master_calibr_file(
        files_list,
        calc_opts,
        result_file,
        |_| true,
        progress,
        thread_pool,
        cancel_flag,
        false
    )
}

fn postprocess_single_flat_image_color(
    raw_image:   &mut RawImage,
    cc:          CfaColor,
    white_level: f32) -> bool
{
    let center_x = raw_image.info.width / 2;
    let center_y = raw_image.info.height / 2;
    let center_size = (raw_image.info.width + raw_image.info.height) / 40;

    let mut data = Vec::new();

    for (x, y, v) in raw_image.data.iter_rect_crd(
        center_x-center_size,
        center_y-center_size+1,
        center_x+center_size,
        center_y+center_size+1)
    {
        if raw_image.info.cfa.get_pixel_color(x, y) != cc { continue; }
        data.push(v);
    }

    if data.is_empty() { return true; }

    let high_index = 95 * data.len() / 100;
    let center_max = *data.select_nth_unstable_by(high_index, cmp_f32).1;
    let center_level_percent = 100.0 * (center_max / white_level);

    log::info!("{:?} -> {:.2}% at center", cc, center_level_percent);

    if center_level_percent > 90.0 {
        log::info!("Dropped due to overexposure");
        return false;
    }

    data.clear();
    for (x, y, v) in raw_image.data.iter_crd() {
        if raw_image.info.cfa.get_pixel_color(x, y) != cc { continue; }
        data.push(v);
    }

    if data.is_empty() { return true; }

    let high_index = 99 * data.len() / 100;
    let norm_value = *data.select_nth_unstable_by(high_index, cmp_f32).1;

    for (x, y, v) in raw_image.data.iter_crd_mut() {
        if raw_image.info.cfa.get_pixel_color(x, y) != cc { continue; }
        *v /= norm_value;
    }

    true
}

fn postprocess_single_flat_image(raw_image: &mut RawImage, bias_image: Option<&RawImage>) -> bool {
    if let Some(bias_image) = bias_image {
        raw_image.data -= &bias_image.data;
    }

    let mono_ok = postprocess_single_flat_image_color(
        raw_image,
        CfaColor::Mono,
        raw_image.info.max_values[0],
    );

    let r_ok = postprocess_single_flat_image_color(
        raw_image,
        CfaColor::R,
        raw_image.info.max_values[0]
    );

    let g_ok = postprocess_single_flat_image_color(
        raw_image,
        CfaColor::G,
        raw_image.info.max_values[1]
    );

    let b_ok = postprocess_single_flat_image_color(
        raw_image,
        CfaColor::B,
        raw_image.info.max_values[2]
    );

    raw_image.info.max_values.fill(1.0);
    raw_image.info.black_values.fill(0.0);

    mono_ok && r_ok && g_ok && b_ok
}

pub fn create_master_flat_file(
    files_list:          &[PathBuf],
    calc_opts:           &CalcOpts,
    master_bias_file:    &Option<PathBuf>,
    result_file:         &Path,
    progress:            &ProgressTs,
    thread_pool:         &rayon::ThreadPool,
    cancel_flag:         &IsCancelledFun,
    force_even_if_exist: bool,
) -> anyhow::Result<bool> {
    let bias_image = match master_bias_file {
        Some(master_bias_file) =>
            Some(RawImage::new_from_master_format_file(master_bias_file)?),
        None =>
            None,
    };

    create_master_calibr_file(
        files_list,
        calc_opts,
        result_file,
        move |img| postprocess_single_flat_image(img, bias_image.as_ref()),
        progress,
        thread_pool,
        cancel_flag,
        force_even_if_exist
    )
}

fn create_master_calibr_file<PF>(
    files_list:          &[PathBuf],
    calc_opts:           &CalcOpts,
    result_file:         &Path,
    postprocess_fun:     PF,
    progress:            &ProgressTs,
    thread_pool:         &rayon::ThreadPool,
    cancel_flag:         &IsCancelledFun,
    force_even_if_exist: bool) -> anyhow::Result<bool>
where
    PF: Fn (&mut RawImage) -> bool + Send + Sync + 'static
{
    let disk_access_mutex = Mutex::new(());

    let this_info = MasterFileInfo {
        files: files_list.to_vec(),
        calc_opts: calc_opts.clone(),
    };

    if !force_even_if_exist && result_file.exists() {
        let from_disk_info = MasterFileInfo::read_fro(result_file);
        if let Ok(from_disk_info) = from_disk_info {
            if from_disk_info == this_info { return Ok(false); }
        }
    }

    let cur_result = Mutex::new(anyhow::Result::<bool>::Ok(false));

    progress.lock().unwrap().set_total(files_list.len());
    let files_to_process = Mutex::new(Vec::new());

    thread_pool.scope(|s| {
        for file_path in files_list.iter() {
            s.spawn(|_| {
                if cancel_flag()
                || cur_result.lock().unwrap().is_err() {
                    return;
                }
                let (mut raw, _) = match load_raw_file(file_path) {
                    Ok(raw) => raw,
                    Err(err) => {
                        *cur_result.lock().unwrap() = Err(anyhow::anyhow!(
                            r#"Error "{}" during processing of file "{}""#,
                            err.to_string(),
                            file_path.to_str().unwrap_or("")
                        ));
                        return;
                    }
                };
                raw.extract_black();
                let is_ok = postprocess_fun(&mut raw);
                if !is_ok { return; }
                let temp_fn = file_path.with_extension("temp_raw");
                let locker = disk_access_mutex.lock();
                let save_res = raw.save_to_calibr_format_file(&temp_fn);
                drop(locker);
                if let Err(save_res) = save_res {
                    *cur_result.lock().unwrap() = Err(save_res);
                    return;
                }
                progress.lock().unwrap().progress(true, extract_file_name(file_path));
                files_to_process.lock().unwrap().push(temp_fn);
            });
        }
    });

    if cancel_flag() {
        return Ok(false);
    }

    if cur_result.lock().unwrap().is_err() {
        return cur_result.into_inner()?;
    }

    struct FileData {
        reader: BitReader<BufReader<File>, BigEndian>,
        decompress: ValuesDecompressor,
    }

    let mut opened_files = Vec::new();
    let mut temp_file_list = FilesToDeleteLater::new();
    let mut first_image_info: Option<RawImageInfo> = None;
    for file_path in files_to_process.lock().unwrap().iter() {
        let mut file = BufReader::new(File::open(&file_path)?);
        let image_info = RawImageInfo::read_from(&mut file)?;
        if let Some(fii) = &first_image_info {
            if !fii.check_is_compatible(&image_info) { bail!(
                "Parameters of file {:?} is not same compared first one",
                file_path
            ); }
        } else {
            first_image_info = Some(image_info.clone());
        }

        let reader = BitReader::endian(file, BigEndian);
        opened_files.push(FileData {
            reader,
            decompress: ValuesDecompressor::new(),
        });
        temp_file_list.add(file_path);
    }

    if opened_files.is_empty() {
        bail!("Nothing to merge")
    }

    let image_info = first_image_info.unwrap();
    let mut image = RawImage::new_from_info(image_info);
    let mut data_to_calc = Vec::<CalcValue>::new();

    let mut prev_y = -1;
    let img_height = image.data.height() as usize;
    for (_, y, v) in image.data.iter_crd_mut() {
        if y != prev_y {
            if cancel_flag() {
                return Ok(false);
            }
            progress.lock().unwrap().percent(y as usize + 1, img_height, "Stacking values...");
            prev_y = y;
        }

        data_to_calc.clear();
        for f in opened_files.iter_mut() {
            let value = f.decompress.read_f32(&mut f.reader)?;
            data_to_calc.push(CalcValue::new(value as f64));
        }
        *v = calc(&mut data_to_calc, calc_opts)
            .map(|v| v.result)
            .unwrap_or(NO_VALUE_F32 as f64) as f32;
    }

    progress.lock().unwrap().percent(100, 100, "Done");

    image.save_to_master_format_file(
        result_file,
        &this_info
    )?;

    Ok(true)
}

///////////////////////////////////////////////////////////////////////////////

/* Stacking light files */

pub struct TempFileData {
    orig_file:    PathBuf,
    file_name:    PathBuf,
    range_factor: f32,
    noise:        f32,
    info:         ImageInfo,
    img_offset:   ImageOffset,
    group_idx:    usize,
}

#[derive(Copy, Clone)]
pub enum SaveAlignedImageMode {
    No,
    Tif,
    Fits
}

struct SaveTempFileData {
    file_name:    PathBuf,
    image:        Image,
    info:         ImageInfo,
    save_aligned: SaveAlignedImageMode,
    orig_fn:      PathBuf,
}

fn save_temp_file(mut args: SaveTempFileData) -> anyhow::Result<()> {
    let save_log = TimeLogger::start();
    save_image_into_internal_format(&args.image, &args.file_name)?;

    if let SaveAlignedImageMode::Fits|SaveAlignedImageMode::Tif = args.save_aligned {
        let ext = match args.save_aligned {
            SaveAlignedImageMode::Fits => FIT_EXTS[0],
            SaveAlignedImageMode::Tif => TIF_EXTS[0],
            _ => "",
        };
        let file_name = args.file_name.with_extension(format!("aligned.{}", ext));
        log::info!("Saving aligned image to {:?} file", file_name);

        args.image.set_novalue_as_zero();
        args.image.fill_inf_areas();
        save_image_to_file(
            &args.image,
            &args.info,
            &file_name
        )?;
    }

    save_log.log("saving temp file");

    Ok(())
}

pub fn create_temp_light_files(
    progress:           &ProgressTs,
    files_list:         Vec<PathBuf>,
    master_flat:        Option<&Path>,
    master_dark:        Option<&Path>,
    master_bias:        Option<&Path>,
    ref_data:           &RefBgData,
    bin:                usize,
    raw_params:         &RawOpenParams,
    result_list:        &Mutex<Vec<TempFileData>>,
    files_to_del_later: &Mutex<FilesToDeleteLater>,
    thread_pool:        &rayon::ThreadPool,
    cancel_flag:        &IsCancelledFun,
    group_idx:          usize,
    save_aligned:       SaveAlignedImageMode,
    align_rgb_each:     bool,
) -> anyhow::Result<()> {
    progress.lock().unwrap().percent(0, 100, "Loading calibration images...");
    let cal_data = CalibrationData::load(
        master_flat,
        master_dark,
        master_bias
    )?;

    let (save_tx, save_rx) = mpsc::sync_channel::<SaveTempFileData>(5);
    let cur_result = Arc::new(Mutex::new(anyhow::Result::<()>::Ok(())));

    progress.lock().unwrap().set_total(files_list.len());

    let save_thread = {
        let cur_result = Arc::clone(&cur_result);
        let progress = Arc::clone(&progress);
        std::thread::spawn(move || {
            let save_rx = save_rx;
            while let Ok(item) = save_rx.recv() {
                let file_name = item.file_name.clone();
                let org_fn = item.orig_fn.clone();
                let res = save_temp_file(item);
                if let Err(err) = res {
                    *cur_result.lock().unwrap() = Err(anyhow::anyhow!(
                        r#"Error "{}" during saving of file "{}""#,
                        err.to_string(),
                        file_name.to_str().unwrap_or("")
                    ));
                    break;
                }
                progress.lock().unwrap().progress(true, extract_file_name(&org_fn));
            }
            log::info!("Exiting from save thread...");
        })
    };

    let cancel_flag = Arc::clone(cancel_flag);
    thread_pool.scope(|s| {
        for file in files_list.iter() {
            let save_tx = save_tx.clone();
            s.spawn(|_| {
                if cancel_flag()
                || cur_result.lock().unwrap().is_err() {
                    return;
                }
                let res = create_temp_file_from_light_file(
                    file,
                    group_idx,
                    &cal_data,
                    ref_data,
                    bin,
                    raw_params,
                    files_to_del_later,
                    result_list,
                    save_tx,
                    save_aligned,
                    align_rgb_each
                );
                if let Err(err) = res {
                    *cur_result.lock().unwrap() = Err(anyhow::anyhow!(
                        r#"Error "{}" during processing of file "{}""#,
                        err.to_string(),
                        file.to_str().unwrap_or("")
                    ));
                    return;
                }
            });
        }

        drop(save_tx);
    });

    save_thread.join().expect("save_thread.join()");

    result_list.lock().unwrap().sort_by_key(|f| (f.group_idx, f.file_name.clone()));

    Arc::try_unwrap(cur_result).unwrap().into_inner()?

}

fn create_temp_file_from_light_file(
    file:               &Path,
    group_idx:          usize,
    cal_data:           &CalibrationData,
    ref_data:           &RefBgData,
    bin:                usize,
    raw_params:         &RawOpenParams,
    files_to_del_later: &Mutex<FilesToDeleteLater>,
    result_list:        &Mutex<Vec<TempFileData>>,
    save_tx:            mpsc::SyncSender<SaveTempFileData>,
    save_aligned:       SaveAlignedImageMode,
    align_rgb:          bool
) -> anyhow::Result<()> {
    let file_total_log = TimeLogger::start();

    log::info!("loading light file {}...", file.to_str().unwrap_or(""));
    let load_log = TimeLogger::start();
    let mut light_file = LightFile::load_and_calc_params(
        file,
        cal_data,
        LoadLightFlags::STARS | LoadLightFlags::NOISE,
        OpenMode::Processing,
        bin,
        raw_params
    )?;
    log::info!("loaded light file {}!", file.to_str().unwrap_or(""));
    load_log.log("loading light file TOTAL");

    log::info!("noise = {:.8}", light_file.noise);
    log::info!("info = {:?}", light_file.info);

    if align_rgb && light_file.image.is_rgb() {
        align_rgb_layers(&mut light_file.image)?;
    }

    let diff_log = TimeLogger::start();
    let mut img_offset: Option<ImageOffset> = None;

    for (max_stars, find_triangle_max_err, triangulation) in [
        (50,  5.0, false),
        (100, 3.0, false),
        (200, 3.0, false),
        (50,  3.0, true),
        (100, 3.0, true),
    ] {
        img_offset = calc_image_offset_by_stars(
            &ref_data.image.stars,
            &light_file.stars,
            light_file.image.width() as f64,
            light_file.image.height() as f64,
            max_stars,
            find_triangle_max_err,
            triangulation,
        );
        if img_offset.is_some() {
            break;
        }
    }
    diff_log.log("calculating light and ref difference");

    if let Some(img_offset) = img_offset {
        log::info!(
            "offset = x:{:.3}, y:{:.3}; rotation = {:.3}°",
            img_offset.offset_x,
            img_offset.offset_y,
            180.0 * img_offset.angle / PI
        );

        let rot_log = TimeLogger::start();
        light_file.image = light_file.image.rotated_and_translated(
            -img_offset.angle,
            -img_offset.offset_x,
            -img_offset.offset_y,
            NO_VALUE_F32,
            light_file.image.width(),
            light_file.image.height()
        );
        rot_log.log("rotating image");

        let norm_log = TimeLogger::start();
        let norm_res = normalize_range_and_bg(ref_data, &mut light_file)?;
        norm_log.log("bg normalization TOTAL");

        let nan_log = TimeLogger::start();
        light_file.image.check_contains_inf_or_nan(false, true)?;
        nan_log.log("check_contains_nan");

        let temp_file_name = file.with_extension("temp_light_data");
        log::info!("Sending image into saving queue...");
        save_tx.send(SaveTempFileData{
            file_name:    temp_file_name.clone(),
            orig_fn:      file.to_path_buf(),
            image:        light_file.image,
            info:         light_file.info.clone(),
            save_aligned,
        })?;
        log::info!("Sending image into saving queue... OK!");

        files_to_del_later.lock().unwrap().add(&temp_file_name);
        result_list.lock().unwrap().push(TempFileData{
            orig_file:    file.to_path_buf(),
            file_name:    temp_file_name,
            range_factor: norm_res.range_factor,
            noise:        light_file.noise * norm_res.range_factor,
            info:         light_file.info,
            img_offset,
            group_idx,
        });
    } else {
        anyhow::bail!("Can't calculate offset and angle between reference image and light file");
    }
    file_total_log.log("processing file TOTAL");
    Ok(())
}

pub fn seconds_to_total_time_str(seconds: f64, short: bool) -> String {
    let secs_total = seconds as u64;
    let minutes_total = secs_total / 60;
    let hours = minutes_total / 60;

    if hours != 0 {
        if !short {
            format!("{} h. {} min.", hours, minutes_total % 60)
        } else {
            format!("{}h{}m", hours, minutes_total % 60)
        }
    } else {
        if !short {
            format!("{} min.", minutes_total % 60)
        } else {
            format!("{}m", minutes_total % 60)
        }
    }
}

pub fn merge_temp_light_files(
    progress:        &ProgressTs,
    temp_file_names: &[TempFileData],
    calc_opts:       &CalcOpts,
    is_rgb_image:    bool,
    ref_width:       Crd,
    ref_height:      Crd,
    align_rgb:       bool,
    result_file:     &Path,
    cancel_flag:     &IsCancelledFun,
) -> anyhow::Result<()> {
    let min_noise = temp_file_names.iter().map(|v| v.noise).min_by(cmp_f32).unwrap();

    progress.lock().unwrap().percent(0, 100, "Opening temp files...");
    let mut stack_items = Vec::new();

    struct StackItem {
        reader: InternalFormatReader,
        weight: f64,
    }

    log::info!(
        "| {:7} | {:7} | {:7} | {:6} | {:6} | {:9} | {:6} | {:8} | {:7} | {:7} | {}",
        "X offs.", "Y offs.", "Angle", "Weight", "Range", "Noise", "ISO", "Exp.time", "Foc.len", "F.Numb.", "File name"
    );
    let mut total_time = 0_f64;
    let mut weighted_time = 0_f64;
    for temp_file in temp_file_names.iter() {
        let weight = min_noise.powf(2.0) / temp_file.noise.powf(2.0);
        total_time += temp_file.info.exp.unwrap_or(0.0) as f64;
        weighted_time += weight as f64 * temp_file.info.exp.unwrap_or(0.0);

        log::info!(
            "| {:7.1} | {:7.1} | {:7.2} | {:6.3} | {:6.3} | {:9.7} | {:6} | {:8.1} | {:7.1} | {:7} | {}",
            temp_file.img_offset.offset_x,
            temp_file.img_offset.offset_y,
            180.0 * temp_file.img_offset.angle / PI,
            weight,
            temp_file.range_factor,
            temp_file.noise,
            temp_file.info.iso.unwrap_or(0),
            temp_file.info.exp.unwrap_or(0.0),
            temp_file.info.focal_len.unwrap_or(0.0),
            format!("f/{:.1}", temp_file.info.fnumber.unwrap_or(0.0)),
            extract_file_name(&temp_file.orig_file)
        );

        stack_items.push(StackItem {
            reader: InternalFormatReader::new(&temp_file.file_name)?,
            weight: weight as f64,
        });
    }

    log::info!("Total time    = {}", seconds_to_total_time_str(total_time, false));
    log::info!("Weighted time = {}", seconds_to_total_time_str(weighted_time, false));

    let time_log = TimeLogger::start();

    let calc_for_values = |values: &mut Vec<CalcValue>| -> f32 {
        if values.is_empty() { return 0.0; }
        let contains_inf = values.iter().any(|v| v.value.is_infinite());
        let contains_values = values.iter().any(|v| !v.value.is_infinite());
        if contains_inf && !contains_values {
            return f32::INFINITY;
        } else if contains_inf && contains_values {
            values.retain(|v| !v.value.is_infinite());
        }
        calc(values, calc_opts)
            .map(|v| v.result as f32)
            .unwrap_or(NO_VALUE_F32)
    };

    let mut result_image = Image::new();

    if is_rgb_image {
        result_image.make_color(ref_width, ref_height);
        let mut r_values = Vec::new();
        let mut g_values = Vec::new();
        let mut b_values = Vec::new();
        let mut prev_y = -1;
        for (x, y, r, g, b) in result_image.iter_rgb_crd_mut() {
            if y != prev_y {
                if cancel_flag() {
                    return Ok(());
                }
                progress.lock().unwrap().percent(
                    y as usize + 1,
                    ref_height as usize,
                    "Merging values..."
                );
                prev_y = y;
            }

            r_values.clear();
            g_values.clear();
            b_values.clear();
            for stack_item in stack_items.iter_mut() {
                let (fr, fg, fb) = stack_item.reader.get_rgb()?;
                if fr != NO_VALUE_F32 {
                    r_values.push(CalcValue::new_weighted(fr as f64, stack_item.weight));
                }
                if fg != NO_VALUE_F32 {
                    g_values.push(CalcValue::new_weighted(fg as f64, stack_item.weight));
                }
                if fb != NO_VALUE_F32 {
                    b_values.push(CalcValue::new_weighted(fb as f64, stack_item.weight));
                }
            }

            *r = calc_for_values(&mut r_values);
            *g = calc_for_values(&mut g_values);
            *b = calc_for_values(&mut b_values);

            if r.is_nan() || g.is_nan() || b.is_nan() {
                log::error!("NAN result in merge_temp_light_files!");
                log::error!("r_values = {:#?}", r_values);
                log::error!("g_values = {:#?}", g_values);
                log::error!("b_values = {:#?}", b_values);
                anyhow::bail!("r = {}, g = {}, b = {} at ({}, {})", *r, *g, *b, x, y);
            }
        }
    } else {
        result_image.make_grey(ref_width, ref_height);
        let mut l_values = Vec::new();
        let mut prev_y = -1;
        for (_, y, l) in result_image.l.iter_crd_mut() {
            if y != prev_y {
                if cancel_flag() {
                    return Ok(());
                }
                progress.lock().unwrap().percent(
                    y as usize + 1,
                    ref_height as usize,
                    "Merging values..."
                );
                prev_y = y;
            }

            l_values.clear();
            for stack_item in stack_items.iter_mut() {
                let fl = stack_item.reader.get_l()?;
                if fl != NO_VALUE_F32 {
                    l_values.push(CalcValue::new_weighted(fl as f64, stack_item.weight));
                }
            }

            *l = calc_for_values(&mut l_values);
        }
    }

    time_log.log("merging files");

    progress.lock().unwrap().percent(100, 100, "Saving result...");

    result_image.check_contains_inf_or_nan(false, true)?;
    result_image.normalize_to_1(false);
    result_image.fill_inf_areas_with_one();
    result_image.check_contains_inf_or_nan(true, true)?;

    if align_rgb && result_image.is_rgb() {
        align_rgb_layers(&mut result_image)?;
    }

    log::info!("Saving image into file {}", result_file.to_str().unwrap_or(""));
    let mut dst_info = ImageInfo::default();
    dst_info.exp = Some(weighted_time);
    save_image_to_file(&result_image, &dst_info, result_file)?;

    progress.lock().unwrap().percent(100, 100, "Done!");

    Ok(())
}

fn align_rgb_layers(image: &mut Image) -> anyhow::Result<()> {
    let get_stars = |img| {
        let noise = calc_noise(img) as f32;
        find_stars_on_image(img, Some(noise), false)
    };
    let g_stars = get_stars(&image.g)?;
    let img_width = image.width();
    let img_height = image.height();
    let align_layer = |img| -> anyhow::Result<ImageLayerF32> {
        let stars = get_stars(img)?;
        let offset = calc_image_offset_by_stars(
            &g_stars,
            &stars,
            img_width as f64,
            img_height as f64,
            50, 5.0, false
        ).ok_or_else(|| anyhow::anyhow!("Can't calculate offset"))?;
        log::info!(
            "offset for rgb align = x:{:.3}, y:{:.3}; rotation = {:.3}°, ratio = {:.6}",
            offset.offset_x,
            offset.offset_y,
            180.0 * offset.angle / PI,
            offset.ratio
        );
        let mut aligned_layer = img.rotated_and_translated(
            -offset.angle,
            -offset.offset_x,
            -offset.offset_y,
            NO_VALUE_F32,
            img_width,
            img_height
        );
        for (orig, aligned) in img.iter().zip(aligned_layer.iter_mut()) {
            if *aligned == NO_VALUE_F32 {
                *aligned = *orig;
            }
        }
        Ok(aligned_layer)
    };

    log::info!("Aligning R layer");
    let aligned_r_layer = align_layer(&image.r)?;
    log::info!("Aligning B layer");
    let aligned_b_layer = align_layer(&image.b)?;
    image.r = aligned_r_layer;
    image.b = aligned_b_layer;

    Ok(())
}