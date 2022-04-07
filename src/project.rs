use std::{path::*, io::*, fs::*};
use serde::*;
use crate::calc::*;

#[derive(Serialize, Deserialize)]
pub struct Project {
    pub name: String,
    pub groups: Vec<ProjectGroup>,
}

impl Project {
    pub fn new() -> Project {
        Project {
            name: String::new(),
            groups: Vec::new(),
        }
    }

    pub fn load(file_name: &Path) -> anyhow::Result<Project> {
        let reader = BufReader::new(File::open(file_name)?);
        let project = serde_json::from_reader(reader)?;
        Ok(project)
    }

    pub fn save(&self, file_name: &Path) -> anyhow::Result<()> {
        let writer = BufWriter::new(File::create(file_name)?);
        serde_json::to_writer_pretty(writer, self)?;
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub struct CalibrFiles {
    calc_opts: CalcOpts,
    files_list: Vec<ProjectFile>,
}

impl CalibrFiles {
    fn new() -> CalibrFiles {
        CalibrFiles {
            calc_opts: CalcOpts::default(),
            files_list: Vec::new(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ProjectGroup {
    pub name: String,
    pub dark_files: CalibrFiles,
    pub bias_files: CalibrFiles,
    pub flat_files: CalibrFiles,
    pub light_files: CalibrFiles,
}

impl ProjectGroup {
    pub fn new() -> ProjectGroup {
        ProjectGroup {
            name: String::new(),
            dark_files: CalibrFiles::new(),
            bias_files: CalibrFiles::new(),
            flat_files: CalibrFiles::new(),
            light_files: CalibrFiles::new(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ProjectFile {
    pub file_name: PathBuf,
}

impl ProjectFile {
    pub fn new() -> ProjectFile {
        ProjectFile {
            file_name: PathBuf::new(),
        }
    }
}