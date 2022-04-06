use std::{path::*, io::*, fs::*, sync::*};
use anyhow::bail;
use byteorder::*;
use crossbeam::sync::WaitGroup;
use crate::{image::*, calc::*, progress::*, image_raw::*, fs_utils::*};

pub fn create_master_file(
    path:              &PathBuf,
    exts:              &String,
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
    let file_names_list = get_files_list(path, &exts, true)?;

    struct FileData {
        file:       BufReader<File>,
        image_info: RawImageInfo,
    }

    let thread_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_tasks)
        .build()?;

    let all_tasks_finished_waiter = WaitGroup::new();

    progress.lock().unwrap().set_total(file_names_list.len());
    let files_to_process = Arc::new(Mutex::new(Vec::new()));
    for file_path in file_names_list.iter() {
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
            if !fii.check_is_compatible(&image_info) { anyhow::bail!(
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
