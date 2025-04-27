#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#![allow(clippy::too_many_arguments)]
#![allow(clippy::needless_range_loop)]
#![allow(clippy::new_without_default)]
#![allow(dead_code)]


mod image;
mod image_norm;
mod image_raw;
mod cameras_database;
mod image_io;
mod light_file;
mod fs_utils;
mod log_utils;
mod calc;
mod stars;
mod tests;
mod progress;
mod compression;
mod stacking_utils;
mod gtk_utils;
mod config;
mod project;
mod str_utils;
mod ui_main;
mod ui_prj_columns_dialog;
mod ui_about_dialog;
mod ui_move_file_to_group_dialog;
mod ui_project_options_dialog;
mod ui_assign_ref_frame_dialog;
mod ui_cleanup_dialog;
mod ui_dnd_files_type_dialog;
mod ui_select_group_dialog;
mod ui_group_options_dialog;

use gtk::prelude::*;
use gettextrs::*;
use crate::{config::*, log_utils::*};

fn main() -> anyhow::Result<()> {
    // localization
    let locale_path = std::env::current_exe()?
        .parent().ok_or_else(|| anyhow::anyhow!("Can't get path parent"))?
        .to_path_buf()
        .join("locale");

    setlocale(LocaleCategory::LcAll, "");
    bindtextdomain("electra_stacking", locale_path.to_str().unwrap_or(""))?;
    textdomain("electra_stacking")?;

    // logger
    let mut log_dir = get_app_conf_dir(true)?;
    log_dir.push("logs");
    if !log_dir.exists() {
        std::fs::create_dir(&log_dir)?;
    }
    start_logger(&log_dir)?;
    log::info!(
        "Application {} {} started",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION")
    );

    // Panic handler
    std::panic::set_hook(Box::new(panic_handler));

    // build gui
    let application = gtk::Application::new(
        Some("com.github.art-den.electra-stacking"),
        Default::default(),
    );
    application.connect_activate(crate::ui_main::build_ui);

    // run
    application.run();

    Ok(())
}

fn panic_handler(panic_info: &std::panic::PanicHookInfo) {
    let payload_str =
        if let Some(msg) = panic_info.payload().downcast_ref::<&'static str>() {
            Some(*msg)
        } else if let Some(msg) = panic_info.payload().downcast_ref::<String>() {
            Some(msg.as_str())
        } else {
            None
        };

    log::error!("(╯°□°）╯︵ ┻━┻ PANIC OCCURRED");

    if let Some(payload) = payload_str {
        log::error!("Panic paiload: {}", payload);
    }

    if let Some(loc) = panic_info.location() {
        log::error!("Panic location: {}", loc);
    }

    eprintln!("{}", payload_str.unwrap_or_default());
    eprintln!("{}", panic_info.location().map(|loc| loc.to_string()).unwrap_or_default());
}
