use std::{path::*, io::*, fs::*, sync::{*, atomic::{AtomicBool, Ordering}}};
use anyhow::bail;
use byteorder::*;
use crate::{
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

///////////////////////////////////////////////////////////////////////////////

/* Stacking bias, darks and flat files */

pub fn create_master_dark_or_bias_file(
    files_list:        &[PathBuf],
    calc_opts:         &CalcOpts,
    result_file:       &Path,
    progress:          &ProgressTs,
    thread_pool:       &rayon::ThreadPool,
    cancel_flag:       &Arc<AtomicBool>,
) -> anyhow::Result<bool> {
    create_master_calibr_file(
        files_list,
        calc_opts,
        result_file,
        RawLoadFlags::EXTRACT_BLACK,
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
    let center_size = (raw_image.info.width + raw_image.info.height) / 40; // have to be odd

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
        *v = *v / norm_value;
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
    cancel_flag:         &Arc<AtomicBool>,
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
        RawLoadFlags::EXTRACT_BLACK,
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
    load_raw_flags:      RawLoadFlags,
    postprocess_fun:     PF,
    progress:            &ProgressTs,
    thread_pool:         &rayon::ThreadPool,
    cancel_flag:         &Arc<AtomicBool>,
    force_even_if_exist: bool) -> anyhow::Result<bool>
where
    PF: Fn (&mut RawImage) -> bool + Send + Sync + 'static
{
    let disk_access_mutex = Mutex::new(());

    let this_info = MasterFileInfo {
        files: files_list.iter().cloned().collect(),
        calc_opts: calc_opts.clone(),
    };

    if !force_even_if_exist {
        if result_file.exists() {
            let from_disk_info = MasterFileInfo::read_fro(&result_file);
            if let Ok(from_disk_info) = from_disk_info {
                if from_disk_info == this_info { return Ok(false); }
            }
        }
    }

    let cur_result = Mutex::new(anyhow::Result::<bool>::Ok(false));

    progress.lock().unwrap().set_total(files_list.len());
    let files_to_process = Mutex::new(Vec::new());

    thread_pool.scope(|s| {
        for file_path in files_list.iter() {
            s.spawn(|_| {
                let file_path = file_path.clone();
                if cancel_flag.load(Ordering::Relaxed)
                || cur_result.lock().unwrap().is_err() {
                    return;
                }
                let raw_res = RawImage::load_camera_raw_file(
                    &file_path,
                    load_raw_flags,
                    Some(&disk_access_mutex)
                );
                let mut raw = match raw_res {
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
                progress.lock().unwrap().progress(true, extract_file_name(&file_path));
                files_to_process.lock().unwrap().push(temp_fn);
            });
        }
    });

    if cancel_flag.load(Ordering::Relaxed) { return Ok(false); }

    if cur_result.lock().unwrap().is_err() {
        return cur_result.into_inner()?;
    }

    struct FileData {
        file:       BufReader<File>,
        image_info: RawImageInfo,
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
        opened_files.push(FileData{ file, image_info });
        temp_file_list.add(&file_path);
    }

    if opened_files.is_empty() { bail!("Nothing to merge") }

    let image_info = first_image_info.unwrap();
    let mut image = RawImage::new_from_info(image_info);
    let mut data_to_calc = Vec::<CalcValue>::new();

    let mut prev_y = -1;
    let img_height = image.data.height() as usize;
    for (_, y, v) in image.data.iter_crd_mut() {
        if y != prev_y {
            if cancel_flag.load(Ordering::Relaxed) { return Ok(false); }
            progress.lock().unwrap().percent(y as usize + 1, img_height, "Stacking values...");
            prev_y = y;
        }

        data_to_calc.clear();
        for f in opened_files.iter_mut() {
            let value = f.file.read_f32::<BigEndian>()?;
            data_to_calc.push(CalcValue::new(value as f64));
        }
        *v = calc(&mut data_to_calc, calc_opts)
            .and_then(|v| Some(v.result))
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
    exif:         Exif,
    img_offset:   ImageOffset,
}

pub fn create_temp_light_files(
    progress:           &ProgressTs,
    files_list:         Vec<PathBuf>,
    master_flat:        &Option<PathBuf>,
    master_dark:        &Option<PathBuf>,
    master_bias:        &Option<PathBuf>,
    ref_data:           &RefBgData,
    bin:                usize,
    result_list:        &Mutex<Vec<TempFileData>>,
    files_to_del_later: &Mutex<FilesToDeleteLater>,
    thread_pool:        &rayon::ThreadPool,
    cancel_flag:        &Arc<AtomicBool>,
) -> anyhow::Result<()> {
    progress.lock().unwrap().percent(0, 100, "Loading calibration images...");
    let cal_data = CalibrationData::load(
        master_flat,
        master_dark,
        master_bias
    )?;

    progress.lock().unwrap().set_total(files_list.len());
    let disk_access_mutex = Mutex::new(());
    let cur_result = Mutex::new(anyhow::Result::<()>::Ok(()));
    thread_pool.scope(|s| {
        for file in files_list.iter() {
            s.spawn(|_| {
                if cancel_flag.load(Ordering::Relaxed)
                || cur_result.lock().unwrap().is_err() {
                    return;
                }
                let file = file.clone();
                let res = create_temp_file_from_light_file(
                    &file,
                    &cal_data,
                    ref_data,
                    bin,
                    files_to_del_later,
                    &disk_access_mutex,
                    result_list
                );
                if let Err(err) = res {
                    *cur_result.lock().unwrap() = Err(anyhow::anyhow!(
                        r#"Error "{}" during processing of file "{}""#,
                        err.to_string(),
                        file.to_str().unwrap_or("")
                    ));
                    return;
                }
                progress.lock().unwrap().progress(true, extract_file_name(&file));
            });
        }
    });

    cur_result.into_inner()?
}

fn create_temp_file_from_light_file(
    file:               &PathBuf,
    cal_data:           &CalibrationData,
    ref_data:           &RefBgData,
    bin:                usize,
    files_to_del_later: &Mutex<FilesToDeleteLater>,
    disk_access_mutex:  &Mutex<()>,
    result_list:        &Mutex<Vec<TempFileData>>
) -> anyhow::Result<()> {
    let file_total_log = TimeLogger::start();

    let load_log = TimeLogger::start();
    let mut light_file = LightFile::load(
        file,
        &cal_data,
        Some(&disk_access_mutex),
          LoadLightFlags::STARS
        | LoadLightFlags::NOISE,
        bin
    )?;
    load_log.log("loading light file TOTAL");

    log::info!("noise = {:.8}", light_file.noise);
    log::info!("exif = {:?}", light_file.exif);

    let diff_log = TimeLogger::start();
    let img_offset = calc_image_offset_by_stars(
        &ref_data.image.stars,
        &light_file.stars,
        light_file.image.width() as f64,
        light_file.image.height() as f64,
    );
    diff_log.log("calculating light and ref difference");

    if let Some(img_offset) = img_offset {
        log::info!(
            "offset = x:{:.3}, y:{:.3}; rotation = {:.3}°",
            img_offset.offset_x,
            img_offset.offset_y,
            180.0 * img_offset.angle / std::f64::consts::PI
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
        light_file.grey = light_file.image.create_greyscale_layer();

        let norm_log = TimeLogger::start();
        let norm_res = normalize(&ref_data, &mut light_file)?;
        norm_log.log("bg normalization TOTAL");

        let temp_file_name = file.with_extension("temp_light_data");
        let save_lock = disk_access_mutex.lock();
        let save_log = TimeLogger::start();
        save_image_into_internal_format(&light_file.image, &temp_file_name)?;
        save_log.log("saving temp file");
        drop(save_lock);

        files_to_del_later.lock().unwrap().add(&temp_file_name);
        result_list.lock().unwrap().push(TempFileData{
            orig_file:    file.clone(),
            file_name:    temp_file_name,
            range_factor: norm_res.range_factor,
            noise:        light_file.noise * norm_res.range_factor,
            exif:         light_file.exif,
            img_offset,
        });
    } else {
        anyhow::bail!("Can't calculate offset and angle between reference image and light file");
    }
    file_total_log.log("processing file TOTAL");
    Ok(())
}

pub fn seconds_to_total_time_str(seconds: f64) -> String {
    let secs_total = seconds as u64;
    let minutes_total = secs_total / 60;
    let hours = minutes_total / 60;

    if hours != 0 {
        format!("{} h. {} min.", hours, minutes_total % 60)
    } else {
        format!("{} min.", minutes_total % 60)
    }
}

pub fn merge_temp_light_files(
    progress:        &ProgressTs,
    temp_file_names: &Vec<TempFileData>,
    calc_opts:       &CalcOpts,
    is_rgb_image:    bool,
    ref_width:       Crd,
    ref_height:      Crd,
    result_file:     &PathBuf,
    cancel_flag:     &Arc<AtomicBool>,
) -> anyhow::Result<()> {
    let min_noise = temp_file_names.iter().map(|v| v.noise).min_by(cmp_f32).unwrap();

    progress.lock().unwrap().percent(0, 100, "Opening temp files...");
    let mut stack_items = Vec::new();

    struct StackItem {
        reader: InternalFormatReader,
        weight: f64,
    }

    log::info!(
        "| {:7} | {:7} | {:6} | {:6} | {:6} | {:9} | {:6} | {:8} | {:7} | {:7} | {}",
        "X offs.", "Y offs.", "Angle", "Weight", "Range", "Noise", "ISO", "Exp.time", "Foc.len", "F.Numb.", "File name"
    );
    let mut total_time = 0_f64;
    let mut weighted_time = 0_f64;
    for temp_file in temp_file_names.iter() {
        let weight = min_noise.powf(2.0) / temp_file.noise.powf(2.0);
        total_time += temp_file.exif.exp_time.unwrap_or(0.0) as f64;
        weighted_time += (weight * temp_file.exif.exp_time.unwrap_or(0.0)) as f64;

        log::info!(
            "| {:7.1} | {:7.1} | {:6.2} | {:6.3} | {:6.3} | {:9.7} | {:6} | {:8.1} | {:7.1} | {:7} | {}",
            temp_file.img_offset.offset_x,
            temp_file.img_offset.offset_y,
            180.0 * temp_file.img_offset.angle / std::f64::consts::PI,
            weight,
            temp_file.range_factor,
            temp_file.noise,
            temp_file.exif.iso.unwrap_or(0),
            temp_file.exif.exp_time.unwrap_or(0.0),
            temp_file.exif.focal_len.unwrap_or(0.0),
            format!("f/{:.1}", temp_file.exif.fnumber.unwrap_or(0.0)),
            extract_file_name(&temp_file.orig_file)
        );

        stack_items.push(StackItem {
            reader: InternalFormatReader::new(&temp_file.file_name)?,
            weight: weight as f64,
        });
    }

    log::info!("Total time    = {}", seconds_to_total_time_str(total_time));
    log::info!("Weighted time = {}", seconds_to_total_time_str(weighted_time));

    let time_log = TimeLogger::start();

    let calc_for_values = |values: &mut Vec<CalcValue>| -> f32 {
        if values.is_empty() { return 0.0; }
        let contains_inf = values.iter().position(|v| v.value.is_infinite()).is_some();
        let contains_values = values.iter().position(|v| !v.value.is_infinite()).is_some();
        if contains_inf && !contains_values {
            return f32::INFINITY;
        } else if contains_inf && contains_values {
            values.retain(|v| !v.value.is_infinite());
        }
        calc(values, calc_opts)
            .and_then(|v| Some(v.result as f32))
            .unwrap_or(NO_VALUE_F32)
    };

    let mut result_image = Image::new();

    if is_rgb_image {
        result_image.make_color(ref_width, ref_height);
        let mut r_values = Vec::new();
        let mut g_values = Vec::new();
        let mut b_values = Vec::new();
        let mut prev_y = -1;
        for (_, y, r, g, b) in result_image.iter_rgb_crd_mut() {
            if y != prev_y {
                if cancel_flag.load(Ordering::Relaxed) { return Ok(()); }
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
        }
    } else {
        result_image.make_grey(ref_width, ref_height);
        let mut l_values = Vec::new();
        let mut prev_y = -1;
        for (_, y, l) in result_image.l.iter_crd_mut() {
            if y != prev_y {
                if cancel_flag.load(Ordering::Relaxed) { return Ok(()); }
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

    result_image.fill_inf_areas();
    result_image.check_contains_inf_or_nan()?;
    result_image.normalize_if_greater_1();

    let mut dst_exif = Exif::new_empty();
    dst_exif.exp_time = Some(weighted_time as f32);
    save_image_to_file(&result_image, &dst_exif, result_file)?;

    progress.lock().unwrap().percent(100, 100, "Done!");

    Ok(())
}
