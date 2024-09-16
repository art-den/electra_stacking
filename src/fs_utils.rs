use std::path::*;
use itertools::Itertools;
use chrono::prelude::*;

pub fn file_mask_to_regex_str(text: &str) -> String {
    let mut result = String::new();
    for sym in text.chars() {
        match sym {
            '.' | '\\' | '[' | ']' | '(' | ')' |
            '{' | '}' | '^' | '$' | '|' | '+' => {
                    result.push('\\');
                    result.push(sym)
                },

            '?' =>
                result.push('.'),

            '*' =>
                result.push_str(".+"),

            _ =>
                result.push(sym)
        }
    }
    result
}

pub fn create_regex_for_masks(masks: &str) -> anyhow::Result<regex::Regex> {
    let masks = masks.trim();
    if masks.is_empty() {
        return Ok(regex::Regex::new(".*")?);
    }
    let regex_str = masks
        .split(';')
        .map(|s| file_mask_to_regex_str(s.trim()))
        .map(|s| format!("(?:{}$)", s))
        .join("|");

    let regex_str = format!("(?i){}", regex_str); // (?i) - case insensitive flag
    let res = regex::Regex::new(&regex_str)?;
    Ok(res)
}

pub fn get_files_list(
    path:                  &Path,
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

pub fn get_light_info_file_name(file_name: &Path) -> PathBuf {
    file_name.with_extension("light_info")
}

pub fn get_temp_light_tif_file_name(file_name: &Path) -> PathBuf {
    file_name.with_extension("tif")
}

pub fn extract_file_name(path: &Path) -> &str {
    path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("")
}

pub fn extract_extension(path: &Path) -> &str {
    path.extension()
        .and_then(|s| s.to_str())
        .unwrap_or("")
}

pub fn path_to_str(path: &Path) -> &str {
    path
        .to_str()
        .unwrap_or("")
}

pub struct FilesToDeleteLater {
    files: Vec<PathBuf>,
}

impl FilesToDeleteLater {
    pub fn new() -> FilesToDeleteLater {
        FilesToDeleteLater { files: Vec::new() }
    }

    pub fn add(&mut self, file: &Path) {
        self.files.push(file.to_path_buf());
    }
}

impl Drop for FilesToDeleteLater {
    fn drop(&mut self) {
        for file in self.files.iter() {
            let _ = std::fs::remove_file(file);
        }
    }
}

pub fn get_file_time(file_name: &Path) -> anyhow::Result<DateTime<Local>> {
    let metadata = std::fs::metadata(file_name)?;
    Ok(metadata.created()?.into())
}
