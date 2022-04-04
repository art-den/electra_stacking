use std::{path::*, io::*, fs::*, sync::*};
use anyhow::bail;
use itertools::Itertools;
use byteorder::*;
use crate::{image::*, calc::*, progress::*, image_raw::*};

pub fn file_mask_to_regex_str(text: &str) -> String {
    let mut result = String::new();
    for sym in text.chars() {
        match sym {
            '.' | '\\' | '[' | ']' | '(' | ')' |
            '{' | '}' | '^' | '$' | '|' | '+' => {
                result.push('\\');
                result.push(sym)
            },

            '?' =>  result.push('.'),

            '*' => result.push_str(".+"),

            _ => result.push(sym)
        }
    }
    result
}

pub fn create_regex_for_masks(masks: &str) -> anyhow::Result<regex::Regex> {
    let masks = masks.trim();
    if masks.is_empty() {
        return Ok(regex::Regex::new(&".*")?);
    }
    let regex_str = masks
        .split(";")
        .map(|s| file_mask_to_regex_str(s.trim()))
        .map(|s| format!("(?:{}$)", s))
        .join("|");

    let regex_str = format!("(?i){}", regex_str); // (?i) - case insensitive flag
    let res = regex::Regex::new(&regex_str)?;
    Ok(res)
}

pub fn get_files_list(
    path:                  &PathBuf,
    exts:                  &str,
    err_if_no_files_found: bool
) -> anyhow::Result<Vec<PathBuf>> {
    let r = create_regex_for_masks(exts)?;
    let result: Vec<_> = std::fs::read_dir(path)?
        .filter(|entry| entry.is_ok())
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.is_file() && r.is_match(path.to_str().unwrap_or("")))
        .collect();

    if err_if_no_files_found && result.is_empty() {
        anyhow::bail!(
            "No '{}' files found in '{}' directory",
            exts,
            path_to_str(path)
        );
    }

    Ok(result)
}

pub fn get_temp_dark_file_name(dark_file_name: &PathBuf) -> PathBuf {
    let mut result = dark_file_name.clone();
    result.set_extension("temp_dark");
    return result;
}

pub fn get_temp_flat_file_name(dark_file_name: &PathBuf) -> PathBuf {
    let mut result = dark_file_name.clone();
    result.set_extension("temp_flat");
    return result;
}

pub fn get_temp_light_file_name(dark_file_name: &PathBuf) -> PathBuf {
    let mut result = dark_file_name.clone();
    result.set_extension("temp_light");
    return result;
}

pub fn get_light_info_file_name(dark_file_name: &PathBuf) -> PathBuf {
    let mut result = dark_file_name.clone();
    result.set_extension("light_info");
    return result;
}

pub fn get_temp_light_tif_file_name(dark_file_name: &PathBuf) -> PathBuf {
    let mut result = dark_file_name.clone();
    result.set_extension("tif");
    return result;
}

pub fn extract_file_name(path: &PathBuf) -> &str {
    path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("")
}

pub fn extract_extension(path: &PathBuf) -> &str {
    path.extension()
        .and_then(|s| s.to_str())
        .unwrap_or("")
}

pub fn path_to_str(path: &PathBuf) -> &str {
    path
        .to_str()
        .unwrap_or("")
}

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
) -> anyhow::Result<()> {
    let file_names_list = get_files_list(path, &exts, true)?;

    struct FileData {
        file:       BufReader<File>,
        image_info: RawImageInfo,
    }

    progress.lock().unwrap().set_total(file_names_list.len());
    for file_path in file_names_list.iter() {
        progress.lock().unwrap().progress(true, extract_file_name(file_path));
        let mut raw = RawImage::load_camera_raw_file(&file_path, load_raw_flags, None)?;
        let is_ok = after_load_fun(&mut raw);
        if !is_ok { continue; }
        let temp_fn = get_file_name_fun(&file_path);
        raw.save_to_internal_format_file(&temp_fn)?;
    }

    let mut opened_files = Vec::new();
    let mut temp_file_list = FilesToDeleteLater::new();
    let mut first_image_info: Option<RawImageInfo> = None;
    for file_path in file_names_list.iter() {
        let temp_fn = get_file_name_fun(file_path);
        if !Path::new(&temp_fn).is_file() { continue; }

        let mut file = BufReader::new(File::open(&temp_fn)?);
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
        temp_file_list.add(&temp_fn);
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

pub struct FilesToDeleteLater {
    files: Vec<PathBuf>,
}

pub type FilesToDeleteLaterTs = Arc<Mutex<FilesToDeleteLater>>;

impl FilesToDeleteLater {
    pub fn new() -> FilesToDeleteLater {
        FilesToDeleteLater { files: Vec::new() }
    }

    pub fn add(&mut self, file: &PathBuf) {
        self.files.push(file.clone());
    }
}

impl Drop for FilesToDeleteLater {
    fn drop(&mut self) {
        for file in self.files.iter() {
            let _ = std::fs::remove_file(file);
        }
    }
}

