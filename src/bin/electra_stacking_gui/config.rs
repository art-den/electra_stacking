use std::{path::*, collections::HashMap};
use serde::*;

#[derive(Serialize, Deserialize)]
pub enum Theme { Dark, Light, Other(String) }

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq)]
pub enum ImgScale { Original, FitWindow }

#[derive(Serialize, Deserialize)]
#[serde(default)]
pub struct PrjTreeCol {
    pub width: i32,
    pub visible: bool,
    pub pos: i32,
}

impl Default for PrjTreeCol {
    fn default() -> Self {
        Self {
            width: -1,
            visible: true,
            pos: -1,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Copy)]
pub enum CpuLoad {
    OneThread,
    HalfCPUs,
    AllCPUs,
    CustomCPUs(usize),
}

impl CpuLoad {
    pub fn to_threads_count(self) -> usize {
        match self {
            CpuLoad::OneThread     => 1,
            CpuLoad::HalfCPUs      => (num_cpus::get()/2).max(1),
            CpuLoad::AllCPUs       => num_cpus::get(),
            CpuLoad::CustomCPUs(v) => v,
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(default)]
pub struct Config {
    pub theme: Theme,
    pub prj_tree_width: i32,
    pub main_win_width: i32,
    pub main_win_height: i32,
    pub main_win_maximized: bool,
    pub preview_scale: ImgScale,
    pub preview_auto_min: bool,
    pub preview_auto_wb: bool,
    pub preview_gamma: f32,
    pub prj_cols: HashMap<String, PrjTreeCol>,
    pub cpu_load: CpuLoad,
    pub last_path: PathBuf,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            theme: Theme::Dark,
            prj_tree_width: -1,
            prj_cols: HashMap::new(),
            main_win_width: -1,
            main_win_height: -1,
            main_win_maximized: false,
            preview_scale: ImgScale::Original,
            preview_auto_min: true,
            preview_auto_wb: true,
            preview_gamma: 4.0,
            cpu_load: CpuLoad::HalfCPUs,
            last_path: PathBuf::new(),
        }
    }
}

impl Config {
    pub fn load(&mut self) -> anyhow::Result<()> {
        let file_name = Self::get_file_name(false)?;
        if !file_name.is_file() { return Ok(()); }
        let conf_str = std::fs::read_to_string(file_name)?;
        let config = serde_json::from_str(&conf_str)?;
        *self = config;
        Ok(())
    }

    pub fn save(&self) -> anyhow::Result<()> {
        let file_name = Self::get_file_name(true)?;
        let json_str = serde_json::to_string_pretty(self)?;
        std::fs::write(&file_name, &json_str)?;
        Ok(())
    }

    pub fn get_file_name(create_dir: bool) -> anyhow::Result<PathBuf> {
        let mut conf_dir = get_app_conf_dir(create_dir)?;
        conf_dir.push("config.json");
        Ok(conf_dir)
    }
}

pub fn get_app_conf_dir(create_dir: bool) -> anyhow::Result<PathBuf> {
    let mut conf_dir = dirs::data_local_dir().unwrap();
    conf_dir.push(format!(".{}", env!("CARGO_PKG_NAME")));
    if create_dir && !conf_dir.exists() {
        std::fs::create_dir(&conf_dir)?;
    }
    Ok(conf_dir)
}