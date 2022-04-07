use std::path::*;
use structopt::*;
use clap::arg_enum;
use crate::{light_file::*, fs_utils::*, calc::*};

arg_enum! {
    #[derive(Debug)]
    pub enum CleanupMode {
        Noise,
        Background,
        StarsR,
        StarsRDev,
    }
}

#[derive(StructOpt, Debug)]
pub struct CmdOptions {
    /// path to directory containing RAW flat files
    #[structopt(long, parse(from_os_str))]
    path: PathBuf,

    /// path to directory to place bad RAW files
    #[structopt(long, parse(from_os_str))]
    bad_file_path: Option<PathBuf>,

    /// Cleanup mode
    #[structopt(
        long,
        default_value = "StarsRDev",
        possible_values = &CleanupMode::variants(),
        case_insensitive = true
    )]
    mode: CleanupMode,

    /// Kappa for cappa-sigma mode
    #[structopt(long, default_value = "2")]
    kappa: f64,

    /// repeats count for cappa-sigma mode
    #[structopt(long, default_value = "10")]
    repeats: u32
}

struct FileData {
    value: f64,
    file_name: String,
    to_delete: bool,
}

pub fn execute(options: CmdOptions) -> anyhow::Result<()> {
    let files = get_files_list(&options.path, "*.light_info", true)?;

    let bad_file_path = match options.bad_file_path {
        Some(path) => path,
        None => {
            let mut path = options.path.clone();
            path.push("bad");
            path
        }
    };

    std::fs::create_dir_all(&bad_file_path)?;

    let mut data = Vec::new();
    for file in files {
        let file_data = std::fs::read_to_string(file)?;
        let reg_data: LightFileRegInfo = serde_json::from_str(&file_data)?;

        let value = match options.mode {
            CleanupMode::Noise      => reg_data.noise,
            CleanupMode::Background => reg_data.background as f64,
            CleanupMode::StarsR     => reg_data.stars_r,
            CleanupMode::StarsRDev  => reg_data.stars_r_dev,
        };

        data.push(FileData {
            value,
            file_name: reg_data.file_name,
            to_delete: false,
        });
    }

    let mut values = Vec::new();
    for _ in 0..options.repeats {
        values.clear();
        for item in data.iter() {
            if item.to_delete { continue; }
            values.push(CalcValue::new(item.value));
        }

        let (mean, dev) =
            mean_and_std_dev(&values)
                .ok_or_else(|| anyhow::anyhow!("mean_and_std_dev"))?;

        let max = mean + dev * options.kappa;

        for item in data.iter_mut() {
            if item.to_delete { continue; }
            if item.value > max { item.to_delete = true; }
        }
    }

    let mut moved_cnt = 0_usize;
    for item in data.iter() {
        if !item.to_delete { continue; }

        let mut existing_name = options.path.clone();
        existing_name.push(&item.file_name);
        let mut new_file_name = bad_file_path.clone();
        new_file_name.push(&item.file_name);

        log::info!(
            "Moving from {} to {}",
            path_to_str(&existing_name),
            path_to_str(&new_file_name)
        );

        std::fs::rename(&existing_name, &new_file_name)?;

        existing_name.set_extension("light_info");
        new_file_name.set_extension("light_info");
        std::fs::rename(&existing_name, &new_file_name)?;

        moved_cnt += 1;
    }

    if moved_cnt != 0 {
        println!("Moved {} files", moved_cnt)
    } else {
        println!("No files moved")
    }

    Ok(())
}