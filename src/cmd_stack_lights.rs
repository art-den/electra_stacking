use std::{path::*, sync::*};
use anyhow::bail;
use itertools::Itertools;
use structopt::*;
use crossbeam::sync::WaitGroup;
use crate::{
    consts::*,
    calc::*,
    image::*,
    image_formats::*,
    fs_utils::*,
    stars::*,
    progress::*,
    light_file::*,
    log_utils::*,
    image_norm::*,
};

fn exts_to_str(exts: &[&str]) -> String {
    exts
        .iter()
        .map(|e| format!("*.{}", e)).join(";")
}

#[derive(StructOpt, Debug)]
pub struct CmdOptions {
    /// path to directory containing RAW flat files
    #[structopt(long, parse(from_os_str))]
    path: Vec<PathBuf>,

    /// Master-flat file
    #[structopt(long, parse(from_os_str))]
    master_flat: Vec<PathBuf>,

    /// Master-dark file
    #[structopt(long, parse(from_os_str))]
    master_dark: Vec<PathBuf>,

    /// Reference light file
    #[structopt(long, parse(from_os_str))]
    ref_file: PathBuf,

    /// Extensions of RAW lights files
    #[structopt(long, default_value = DEF_RAW_EXTS)]
    exts: String,

    #[structopt(flatten)]
    calc_opts: CalcOpts,

    /// File name of result file (with .tif extension)
    #[structopt(long, parse(from_os_str))]
    result_file: PathBuf,

    /// Number of parallel tasks
    #[structopt(long, default_value = "1")]
    num_tasks: usize,

    /// reduce image size by `bin` times.
    /// Only value 2 is supported
    #[structopt(long)]
    bin: Option<usize>,
}

struct DirData {
    path: PathBuf,
    master_dark: Option<PathBuf>,
    master_flat: Option<PathBuf>,
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    let progress = ProgressConsole::new_ts();
    let temp_file_names = Arc::new(Mutex::new(Vec::<TempFileData>::new()));
    let files_to_del_later = Arc::new(Mutex::new(FilesToDeleteLater::new()));
    let mut dir_data = Vec::new();

    if options.path.is_empty() {
        bail!("You must define at least one --path");
    }

    if options.path.len() == 1 {
        if options.master_dark.len() > 1 {
            bail!("Wrong amount of master dark files");
        }
        if options.master_flat.len() > 1 {
            bail!("Wrong amount of master flat files");
        }

        dir_data.push(DirData {
            path: options.path[0].clone(),
            master_dark:
                if !options.master_dark.is_empty() { Some(options.master_dark[0].clone()) }
                else { None },
            master_flat:
                if !options.master_flat.is_empty() { Some(options.master_flat[0].clone()) }
                else { None },
        });
    } else {
        if options.path.len() != options.master_dark.len() {
            bail!("Wrong amount of master dark files")
        }
        if options.path.len() != options.master_flat.len() {
            bail!("Wrong amount of master flat files")
        }

        for i in 0..options.path.len() {
            dir_data.push(DirData {
                path:        options.path[i].clone(),
                master_dark: Some(options.master_dark[i].clone()),
                master_flat: Some(options.master_flat[i].clone()),
            });
        }
    }

    progress.lock().unwrap().percent(0, 100, "Loading reference file...");

    let ref_cal = CalibrationData::load(
        &dir_data[0].master_flat,
        &dir_data[0].master_dark
    )?;

    let ref_data = Arc::new(RefData::new(
        &options.ref_file,
        &ref_cal,
        options.bin.unwrap_or(1)
    )?);

    let thread_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(options.num_tasks)
        .build()
        .unwrap();

    for item in dir_data.iter() {
        let ref_data = Arc::clone(&ref_data);

        create_temp_files(
            Arc::clone(&progress),
            &item.path,
            &item.master_flat,
            &item.master_dark,
            ref_data,
            options.bin.unwrap_or(1),
            &options.exts,
            Arc::clone(&temp_file_names),
            Arc::clone(&files_to_del_later),
            &thread_pool
        )?;
    }

    if temp_file_names.lock().unwrap().is_empty() {
        anyhow::bail!("Nothing to merge");
    }

    merge_temp_files(
        Arc::clone(&progress),
        &temp_file_names.lock().unwrap(),
        &options.calc_opts,
        ref_data.image.image.is_rgb(),
        ref_data.image.image.width(),
        ref_data.image.image.height(),
        &options.result_file
    )?;

    drop(files_to_del_later);

    Ok(())
}

struct TempFileData {
    orig_file:    PathBuf,
    file_name:    PathBuf,
    range_factor: f64,
    noise:        f64,
    exif:         Exif,
    img_offset:   ImageOffset,
}


fn create_temp_files(
    progress:           ProgressTs,
    path:               &PathBuf,
    master_flat:        &Option<PathBuf>,
    master_dark:        &Option<PathBuf>,
    ref_data:           Arc<RefData>,
    bin:                usize,
    exts:               &String,
    result_list:        Arc<Mutex<Vec<TempFileData>>>,
    files_to_del_later: FilesToDeleteLaterTs,
    thread_pool:        &rayon::ThreadPool
) -> anyhow::Result<()> {
    progress.lock().unwrap().percent(0, 100, "Searching files...");

    log::info!("Searching files in {} ...", path_to_str(path));
    let file_names_list = get_files_list(path, exts, true)?;

    progress.lock().unwrap().percent(0, 100, "Loading calibration images...");
    let cal_data = Arc::new(CalibrationData::load(master_flat, master_dark)?);

    progress.lock().unwrap().set_total(file_names_list.len());

    let disk_access_mutex = Arc::new(Mutex::new(()));

    let all_tasks_finished_waiter = WaitGroup::new();
    for file in file_names_list.iter() {
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
                file,
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

    progress.lock().unwrap().percent(100, 100, format!("{} done", path_to_str(path)).as_str());
    progress.lock().unwrap().next_step();

    Ok(())
}

fn create_temp_file_from_light_file(
    file:               PathBuf,
    cal_data:           Arc<CalibrationData>,
    ref_data:           Arc<RefData>,
    bin:                usize,
    files_to_del_later: FilesToDeleteLaterTs,
    disk_access_mutex:  Arc<Mutex<()>>,
    result_list:        Arc<Mutex<Vec<TempFileData>>>
) -> Result<(), anyhow::Error> {
    let file_total_log = TimeLogger::start();

    let load_log = TimeLogger::start();
    let mut light_file = LightFile::load(
        &file,
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

/*
        aligned_image.l.fill_inf_areas();
        aligned_image.r.fill_inf_areas();
        aligned_image.g.fill_inf_areas();
        aligned_image.b.fill_inf_areas();
        aligned_image.save_to_tiff_file(&get_temp_light_tif_file_name(&file))?;
*/

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

fn merge_temp_files(
    progress:        ProgressTs,
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

