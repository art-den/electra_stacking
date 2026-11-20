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
        self.load_from(&file_name)
    }

    fn load_from(&mut self, file_name: &Path) -> anyhow::Result<()> {
        if !file_name.is_file() { return Ok(()); }
        let conf_str = match std::fs::read_to_string(file_name) {
            Ok(conf_str) => conf_str,
            Err(error) => {
                Self::backup_corrupt_file(file_name);
                return Err(error.into());
            }
        };
        let config = match serde_json::from_str(&conf_str) {
            Ok(config) => config,
            Err(error) => {
                Self::backup_corrupt_file(file_name);
                return Err(error.into());
            }
        };
        *self = config;
        Ok(())
    }

    // Keep the corrupt file for manual recovery; otherwise closing the app overwrites it with defaults
    fn backup_corrupt_file(file_name: &Path) {
        let mut bak = file_name.to_path_buf();
        bak.as_mut_os_string().push(".bak");
        if let Err(error) = std::fs::rename(file_name, &bak) {
            log::warn!("Failed to backup corrupt config file {:?}: {}", file_name, error);
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn temp_config_dir(tag: &str) -> PathBuf {
        let mut dir = std::env::temp_dir();
        dir.push(format!("electra_config_test_{}_{}_{}", tag, std::process::id(), uuid::Uuid::new_v4()));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn load_from_corrupt_json_creates_backup_and_keeps_defaults() {
        let dir = temp_config_dir("corrupt");
        let conf = dir.join("config.json");
        std::fs::write(&conf, b"{ \"preview_gamma\": 2.5, \"theme\": ").unwrap();
        let mut config = Config::default();
        assert!(config.load_from(&conf).is_err());
        assert!(!conf.exists());
        assert_eq!(std::fs::read_to_string(dir.join("config.json.bak")).unwrap(), "{ \"preview_gamma\": 2.5, \"theme\": ");
        assert!(matches!(config.theme, Theme::Dark));
        assert_eq!(config.preview_gamma, 4.0);
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn load_from_invalid_utf8_creates_backup() {
        let dir = temp_config_dir("utf8");
        let conf = dir.join("config.json");
        std::fs::write(&conf, [0xFFu8, 0xFE, 0x00, 0x80]).unwrap();
        let mut config = Config::default();
        assert!(config.load_from(&conf).is_err());
        assert!(!conf.exists());
        assert!(dir.join("config.json.bak").is_file());
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn load_from_valid_json_replaces_config_without_backup() {
        let dir = temp_config_dir("valid");
        let conf = dir.join("config.json");
        std::fs::write(&conf, b"{ \"theme\": \"Light\", \"preview_gamma\": 2.5 }").unwrap();
        let mut config = Config::default();
        assert!(config.load_from(&conf).is_ok());
        assert!(matches!(config.theme, Theme::Light));
        assert_eq!(config.preview_gamma, 2.5);
        assert!(conf.is_file());
        assert!(!dir.join("config.json.bak").exists());
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn load_from_missing_file_is_ok_with_defaults() {
        let dir = temp_config_dir("missing");
        let mut config = Config::default();
        assert!(config.load_from(&dir.join("config.json")).is_ok());
        assert_eq!(config.preview_gamma, 4.0);
        std::fs::remove_dir_all(&dir).unwrap();
    }
}