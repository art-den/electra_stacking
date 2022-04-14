use std::{path::*, io::*, fs::*, sync::*};
use anyhow::bail;
use byteorder::*;
use crossbeam::sync::WaitGroup;
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

/* Stacking bias, darks and flat files */

pub fn create_master_file(
    files_list:        Vec<PathBuf>,
    calc_opts:         &CalcOpts,
    result_file:       &PathBuf,
    load_raw_flags:    RawLoadFlags,
    get_file_name_fun: fn (path: &PathBuf) -> PathBuf,
    after_load_fun:    fn (image: &mut RawImage) -> bool,
    before_save_fun:   fn (image: &mut ImageLayerF32),
    progress:          ProgressTs,
    num_tasks:         usize,
) -> anyhow::Result<()> {
    let disk_access_mutex = Arc::new(Mutex::new(()));

    struct FileData {
        file:       BufReader<File>,
        image_info: RawImageInfo,
    }

    let thread_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_tasks)
        .build()?;

    let all_tasks_finished_waiter = WaitGroup::new();

    progress.lock().unwrap().set_total(files_list.len());
    let files_to_process = Arc::new(Mutex::new(Vec::new()));
    for file_path in files_list.iter() {
        let progress = Arc::clone(&progress);
        let disk_access_mutex = Arc::clone(&disk_access_mutex);
        let files_to_process = Arc::clone(&files_to_process);
        let file_path = file_path.clone();
        let waiter = all_tasks_finished_waiter.clone();

        thread_pool.spawn(move || {
            progress.lock().unwrap().progress(true, extract_file_name(&file_path));
            let mut raw = RawImage::load_camera_raw_file(
                &file_path,
                load_raw_flags,
                Some(&disk_access_mutex)
            ).unwrap();
            let is_ok = after_load_fun(&mut raw);
            if !is_ok { return; }
            let temp_fn = get_file_name_fun(&file_path);

            let locker = disk_access_mutex.lock();
            raw.save_to_internal_format_file(&temp_fn).unwrap();
            drop(locker);

            files_to_process.lock().unwrap().push(temp_fn);
            drop(waiter);
        });
    }

    all_tasks_finished_waiter.wait();
    drop(thread_pool);

    let mut opened_files = Vec::new();
    let mut temp_file_list = FilesToDeleteLater::new();
    let mut first_image_info: Option<RawImageInfo> = None;
    for file_path in files_to_process.lock().unwrap().iter() {
        let mut file = BufReader::new(File::open(&file_path)?);
        let image_info = RawImageInfo::read_from(&mut file)?;
        if let Some(fii) = &first_image_info {
            if !fii.check_is_compatible(&image_info) { bail!(
                "Dimesions of file {:?} is not same compared first one",
                file_path
            ); }
        } else {
            first_image_info = Some(image_info.clone());
        }
        opened_files.push(FileData{ file, image_info });
        temp_file_list.add(&file_path);
    }

    progress.lock().unwrap().next_step();

    if opened_files.is_empty() { bail!("Nothing to merge") }

    let image_info = first_image_info.unwrap();
    let mut image = RawImage::new_from_info(image_info);
    let mut data_to_calc = Vec::<CalcValue>::new();

    let mut prev_y = -1;
    let img_height = image.data.height() as usize;
    for (_, y, v) in image.data.iter_crd_mut() {
        if y != prev_y {
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
    progress.lock().unwrap().next_step();

    before_save_fun(&mut image.data);
    image.save_to_internal_format_file(result_file)?;

    Ok(())
}

///////////////////////////////////////////////////////////////////////////////

/* Stacking light files */

pub struct TempFileData {
    orig_file:    PathBuf,
    file_name:    PathBuf,
    range_factor: f64,
    noise:        f64,
    exif:         Exif,
    img_offset:   ImageOffset,
}


pub fn create_temp_light_files(
    progress:           &ProgressTs,
    files_list:         Vec<PathBuf>,
    master_flat:        &Option<PathBuf>,
    master_dark:        &Option<PathBuf>,
    ref_data:           &Arc<RefBgData>,
    bin:                usize,
    result_list:        &Arc<Mutex<Vec<TempFileData>>>,
    files_to_del_later: &Arc<Mutex<FilesToDeleteLater>>,
    thread_pool:        &rayon::ThreadPool
) -> anyhow::Result<()> {
    progress.lock().unwrap().percent(0, 100, "Loading calibration images...");
    let cal_data = Arc::new(CalibrationData::load(master_flat, master_dark)?);

    progress.lock().unwrap().set_total(files_list.len());

    let disk_access_mutex = Arc::new(Mutex::new(()));

    let all_tasks_finished_waiter = WaitGroup::new();
    for file in files_list.iter() {
        let cal_data = Arc::clone(&cal_data);
        let ref_data = Arc::clone(&ref_data);
        let files_to_del_later = Arc::clone(&files_to_del_later);
        let result_list = Arc::clone(&result_list);
        let progress = Arc::clone(&progress);
        let file = file.clone();
        let wait = all_tasks_finished_waiter.clone();
        let disk_access_mutex = Arc::clone(&disk_access_mutex);

        thread_pool.spawn(move || {
            progress.lock().unwrap().progress(true, extract_file_name(&file));
            create_temp_file_from_light_file(
                &file,
                cal_data,
                ref_data,
                bin,
                files_to_del_later,
                disk_access_mutex,
                result_list
            ).unwrap();
            drop(wait)
        });
    }

    all_tasks_finished_waiter.wait();

    Ok(())
}

fn create_temp_file_from_light_file(
    file:               &PathBuf,
    cal_data:           Arc<CalibrationData>,
    ref_data:           Arc<RefBgData>,
    bin:                usize,
    files_to_del_later: Arc<Mutex<FilesToDeleteLater>>,
    disk_access_mutex:  Arc<Mutex<()>>,
    result_list:        Arc<Mutex<Vec<TempFileData>>>
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

        let temp_file_name = get_temp_light_file_name(&file);
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
        //todo!() // TODO: log "can't calculate"
    }
    file_total_log.log("processing file TOTAL");
    Ok(())
}

fn seconds_to_total_time_str(seconds: f64) -> String {
    let secs_total = seconds as u64;
    let minutes_total = secs_total / 60;
    format!(
        "{} hours {:02} minutes",
        minutes_total / 60,
        minutes_total % 60
    )
}

pub fn merge_temp_light_files(
    progress:        &ProgressTs,
    temp_file_names: &Vec<TempFileData>,
    calc_opts:       &CalcOpts,
    is_rgb_image:    bool,
    ref_width:       Crd,
    ref_height:      Crd,
    result_file:     &PathBuf,
) -> anyhow::Result<()> {
    let min_noise = temp_file_names.iter().map(|v| v.noise).min_by(cmp_f64).unwrap();

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
        weighted_time += weight * temp_file.exif.exp_time.unwrap_or(0.0) as f64;

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
            weight,
        });
    }

    log::info!("Total time    = {}", seconds_to_total_time_str(total_time));
    log::info!("Weighted time = {}", seconds_to_total_time_str(weighted_time));

    let time_log = TimeLogger::start();

    if is_rgb_image {
        let mut result_image = Image::new_color(ref_width, ref_height);
        let mut r_values = Vec::new();
        let mut g_values = Vec::new();
        let mut b_values = Vec::new();
        let mut prev_y = -1;
        for (_, y, r, g, b) in result_image.iter_rgb_crd_mut() {
            if y != prev_y {
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

            *r = calc_for_values(&mut r_values);
            *g = calc_for_values(&mut g_values);
            *b = calc_for_values(&mut b_values);
        }

        progress.lock().unwrap().percent(100, 100, "Saving result...");

        result_image.l.fill_inf_areas();
        result_image.r.fill_inf_areas();
        result_image.g.fill_inf_areas();
        result_image.b.fill_inf_areas();

        result_image.check_contains_inf_or_nan()?;
        result_image.normalize_if_greater_1();

        let mut dst_exif = Exif::new_empty();
        dst_exif.exp_time = Some(weighted_time as f32);
        save_image_to_file(&result_image, &dst_exif, result_file)?;
    }

    time_log.log("merging files");
    progress.lock().unwrap().percent(100, 100, "Done!");

    Ok(())
}
