#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

// TODO: Version for calibration files
// TODO: Insert info of calibration file info file data

mod config;
mod project;

use std::{rc::Rc, path::*, thread, collections::*, cell::RefCell};
use std::sync::{*, atomic::{AtomicBool, Ordering}};
use gtk::{
    prelude::*,
    builders::*,
    gio,
    gdk_pixbuf,
    glib::{MainContext, PRIORITY_DEFAULT, clone},
    glib,
};
use rayon::prelude::*;
use itertools::*;
use astro_utils::{
    image_formats::*,
    image_raw::*,
    stacking_utils::*,
    light_file::*,
    calc::*,
    image,
    progress::*,
    log_utils::*,
};

use crate::{config::*, project::*};

fn main() -> anyhow::Result<()> {
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
        Some("com.github.art-den-astro-utils"),
        Default::default(),
    );
    application.connect_activate(build_ui);

    // run
    application.run();

    Ok(())
}

fn panic_handler(panic_info: &std::panic::PanicInfo) {
    if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
        log::error!("panic occurred: {}", s);
    } else {
        log::error!("panic occurred");
    }

    if let Some(loc) = panic_info.location() {
        log::error!("at location: {}", loc.to_string());
    }
}

///////////////////////////////////////////////////////////////////////////////

/* Main window */

fn build_ui(application: &gtk::Application) {
    let mut project = Project::new();
    let config = Config::new();

    project.make_default();

    let icons = gtk::IconTheme::default().unwrap();

    let icon_folder = icons.load_icon("folder", 16, gtk::IconLookupFlags::empty()).ok().flatten();
    let icon_image = icons.load_icon("image-x-generic", 16, gtk::IconLookupFlags::empty()).ok().flatten();
    let icon_photo = icons.load_icon("camera-photo-symbolic.symbolic", 16, gtk::IconLookupFlags::empty()).ok().flatten();
    let icon_ref_image = gdk_pixbuf::Pixbuf::from_read(include_bytes!(r"ui/key.png").as_slice()).ok();

    let builder = gtk::Builder::from_string(include_str!(r"ui/main_window.ui"));

    let window              = builder.object::<gtk::ApplicationWindow>("main_window").unwrap();
    let menu_bar            = builder.object::<gtk::MenuBar>("menu_bar").unwrap();
    let prj_tree_menu       = builder.object::<gtk::Menu>("prj_tree_menu").unwrap();
    let project_tree        = builder.object::<gtk::TreeView>("project_tree").unwrap();
    let progress_bar        = builder.object::<gtk::ProgressBar>("progress_bar").unwrap();
    let progress_box        = builder.object::<gtk::Grid>("progress_box").unwrap();
    let progress_text       = builder.object::<gtk::Label>("progress_text").unwrap();
    let cancel_btn          = builder.object::<gtk::Button>("cancel_btn").unwrap();
    let preview_img_scr     = builder.object::<gtk::ScrolledWindow>("preview_image_scrolled").unwrap();
    let prj_img_paned       = builder.object::<gtk::Paned>("prj_img_paned").unwrap();
    let preview_img_scale   = builder.object::<gtk::ComboBoxText>("preview_img_scale").unwrap();
    let preview_auto_min    = builder.object::<gtk::CheckButton>("preview_auto_min").unwrap();
    let preview_img_gamma   = builder.object::<gtk::Scale>("preview_img_gamma").unwrap();
    let preview_file_name   = builder.object::<gtk::Label>("preview_file_name").unwrap();
    let preview_ctrls_box   = builder.object::<gtk::Widget>("preview_ctrls_box").unwrap();
    let recent_menu         = builder.object::<gtk::RecentChooserMenu>("recent_menu").unwrap();
    let mi_change_file_type = builder.object::<gtk::MenuItem>("mi_change_file_type").unwrap();
    let mi_dark_theme       = builder.object::<gtk::RadioMenuItem>("dark_theme_mi").unwrap();
    let mi_light_theme      = builder.object::<gtk::RadioMenuItem>("light_theme_mi").unwrap();

    let prj_tree_col_names = get_prj_tree_col_items();

    for (col1, col2) in prj_tree_col_names.iter().tuple_windows() {
        assert!(col1.1+1 == col2.1);
    }

    let cell_check = gtk::CellRendererToggle::builder()
        .activatable(true)
        .mode(gtk::CellRendererMode::Activatable)
        .sensitive(true)
        .build();

    for (col_name, idx, _) in prj_tree_col_names {
        if col_name.is_empty() { continue; }
        let cell_text = gtk::CellRendererText::new();
        let col = gtk::TreeViewColumn::builder()
            .title(col_name)
            .resizable(true)
            .clickable(true)
            .build();

        if idx != COLUMN_FILE_NAME {
            col.pack_start(&cell_text, true);
            col.add_attribute(&cell_text, "text", idx as i32);
        } else {
            let cell_img = gtk::CellRendererPixbuf::new();

            col.pack_start(&cell_check, false);
            col.pack_start(&cell_img, false);
            col.pack_start(&cell_text, true);

            col.add_attribute(&cell_text, "text", idx as i32);
            col.add_attribute(&cell_img, "pixbuf", COLUMN_ICON as i32);
            col.add_attribute(&cell_check, "active", COLUMN_CHECKBOX as i32);
            col.add_attribute(&cell_check, "visible", COLUMN_CHECKBOX_VIS as i32);
        }

        col.set_sort_column_id(idx as i32);

        project_tree.append_column(&col);
    }

    let preview_event_box = gtk::EventBox::builder()
        .expand(true)
        .parent(&preview_img_scr)
        .build();

    let preview_image = gtk::Image::builder()
        .expand(true)
        .parent(&preview_event_box)
        .build();

    let preview_tp = rayon::ThreadPoolBuilder::new()
        .num_threads(2)
        .build()
        .unwrap();

    let objects = Rc::new(MainWindowObjects {
        project: RefCell::new(project),
        config: RefCell::new(config),
        window: window.clone(),
        menu_bar,
        prj_tree: project_tree.clone(),
        prj_tree_changed_flag: RefCell::new(false),
        prj_tree_menu: prj_tree_menu.clone(),
        prj_img_paned,
        mi_dark_theme: mi_dark_theme.clone(),
        mi_light_theme: mi_light_theme.clone(),
        mi_change_file_type,
        progress_bar,
        progress_cont: progress_box.upcast(),
        progress_text,
        icon_photo,
        icon_folder,
        icon_image,
        icon_ref_image,
        preview_image: preview_image.clone(),
        last_preview_file: RefCell::new(PathBuf::new()),
        cancel_flag: Arc::new(AtomicBool::new(false)),
        preview_img_scale: preview_img_scale.clone(),
        preview_auto_min: preview_auto_min.clone(),
        preview_tp,
        prev_preview_cancel_flags: RefCell::new(None),
        prev_preview_img: RefCell::new(image::Image::new()),
        preview_img_gamma: preview_img_gamma.clone(),
        preview_file_name,
        preview_ctrls_box,
        preview_scroll_pos: RefCell::new(None),
        last_selected_path: RefCell::new(PathBuf::new()),
        move_to_group_last_uuid: RefCell::new(String::new()),
    });

    // Load and apply config

    let res = objects.config.borrow_mut().load();
    if let Err(error) = res {
        show_error_message(&error.to_string(), &objects);
    }
    apply_config(&objects);

    // Events + Actions

    cancel_btn.connect_clicked(clone!(@strong objects => move |_| {
        objects.cancel_flag.store(true, Ordering::Relaxed);
    }));

    project_tree.connect_destroy(clone!{ @weak prj_tree_menu => move |_| {
        prj_tree_menu.unparent();
    }});

    project_tree.connect_button_press_event(move |project_tree, evt| {
        if (evt.button() == gtk::gdk::ffi::GDK_BUTTON_SECONDARY as u32)
        && prj_tree_menu.is_sensitive()
        && project_tree.model().is_some() {
            prj_tree_menu.set_attach_widget(Some(project_tree));
            prj_tree_menu.popup_easy(evt.button(), evt.time());
            if project_tree.selection().count_selected_rows() > 1 {
                return glib::signal::Inhibit(true);
            }
        }
        glib::signal::Inhibit(false)
    });

    objects.prj_tree.selection().connect_changed(clone!{ @weak objects => move |_| {
        handler_project_tree_selection_changed(&objects);
    }});

    cell_check.connect_toggled(clone!(@weak objects => move |_, path| {
        handler_project_tree_checked_changed(&objects, path);
    }));

    mi_dark_theme.connect_activate(clone!(@strong objects => move |mi| {
        if mi.is_active() {
            action_dark_theme(&objects);
        }
    }));

    mi_light_theme.connect_activate(clone!(@strong objects => move |mi| {
        if mi.is_active() {
            action_light_theme(&objects);
        }
    }));

    preview_img_scale.connect_changed(clone!(@strong objects => move |cb| {
        objects.config.borrow_mut().preview_scale = match cb.active() {
            Some(0) => ImgScale::Original,
            Some(1) => ImgScale::FitWindow,
            _       => return,
        };
        preview_image_after_change_view_opts(&objects);
    }));

    preview_auto_min.connect_clicked(clone!(@strong objects => move |chb| {
        objects.config.borrow_mut().preview_auto_min = chb.is_active();
        preview_image_after_change_view_opts(&objects);
    }));

    preview_img_gamma.connect_change_value(clone!(@strong objects => move |_, _, value| {
        objects.config.borrow_mut().preview_gamma = value as f32;
        preview_image_after_change_view_opts(&objects);
        Inhibit(false)
    }));

    conn_action(&objects, "new_project",            action_new_project);
    conn_action(&objects, "open_project",           action_open_project);
    conn_action(&objects, "save_project_as",        action_save_project_as);
    conn_action(&objects, "save_project",           action_save_project);
    conn_action(&objects, "exit",                   action_exit);
    conn_action(&objects, "add_light_files",        action_add_light_files);
    conn_action(&objects, "add_dark_files",         action_add_dark_files);
    conn_action(&objects, "add_flat_files",         action_add_flat_files);
    conn_action(&objects, "add_bias_files",         action_add_bias_files);
    conn_action(&objects, "new_group",              action_new_group);
    conn_action(&objects, "delete_item",            action_delete_item);
    conn_action(&objects, "use_as_ref_image",       action_use_as_reference_image);
    conn_action(&objects, "item_properties",        action_item_properties);
    conn_action(&objects, "register_light_files",   action_register);
    conn_action(&objects, "stack_light_files",      action_stack);
    conn_action(&objects, "project_options",        action_project_options);
    conn_action(&objects, "light_theme",            action_light_theme);
    conn_action(&objects, "dark_theme",             action_dark_theme);
    conn_action(&objects, "cleanup_light_files",    action_cleanup_light_files);
    conn_action(&objects, "change_file_to_light",   action_change_file_to_light);
    conn_action(&objects, "change_file_to_dark",    action_change_file_to_dark);
    conn_action(&objects, "change_file_to_flat",    action_change_file_to_flat);
    conn_action(&objects, "change_file_to_bias",    action_change_file_to_bias);
    conn_action(&objects, "move_file_to_group",     action_move_file_to_group);
    conn_action(&objects, "check_all_files",        action_check_all_files);
    conn_action(&objects, "uncheck_all_files",      action_uncheck_all_files);
    conn_action(&objects, "check_selected_files",   action_check_selected_files);
    conn_action(&objects, "uncheck_selected_files", action_uncheck_selected_files);

    //

    fill_project_tree(&objects);
    update_project_name_and_time_in_gui(&objects, true, true);

    if cfg!(target_os = "windows") {
        let settings = gtk::Settings::default().unwrap();
        settings.set_property("gtk-font-name", "Tahoma 9");
    }

    window.set_application(Some(application));
    window.show_all();

    objects.progress_cont.set_visible(false);

    objects.window.connect_delete_event(clone!(@strong objects => move |_, _| {
        let can_close = ask_user_to_save_project(&objects);
        gtk::Inhibit(!can_close)
    }));

    objects.window.connect_hide(clone!(@strong objects => move |_| {
        assign_config(&objects);
        let _ = objects.config.borrow().save();
    }));

    recent_menu.connect_item_activated(clone!(@strong objects => move |item| {
        let cur_item = item.current_item().unwrap();
        let mut uri = cur_item.uri().unwrap().to_string();
        const PREFIX: &str = "file:///";
        if uri.starts_with(PREFIX) {
            uri = uri[PREFIX.len()..].to_string();
        }
        uri = uri.replace("%20", " ");
        let path = PathBuf::from(uri);
        let can_open = ask_user_to_save_project(&objects);
        if can_open {
            open_project(&objects, &path);
        }
    }));

    preview_event_box.connect_button_press_event(clone!(@strong objects, @strong preview_img_scr => move |_, evt| {
        if evt.button() == gtk::gdk::ffi::GDK_BUTTON_PRIMARY as u32 {
            let hadjustment = preview_img_scr.hadjustment();
            let vadjustment = preview_img_scr.vadjustment();
            *objects.preview_scroll_pos.borrow_mut() = Some((
                evt.root(),
                (hadjustment.value(), vadjustment.value())
            ));
        }
        Inhibit(false)
    }));

    preview_event_box.connect_button_release_event(clone!(@strong objects => move |_, evt| {
        if evt.button() == gtk::gdk::ffi::GDK_BUTTON_PRIMARY as u32 {
            *objects.preview_scroll_pos.borrow_mut() = None;
        }
        Inhibit(false)
    }));

    preview_event_box.connect_motion_notify_event(clone!(@strong objects => move |_, evt| {
        const SCROLL_SPEED: f64 = 2.0;
        if let Some((start_mouse_pos, start_scroll_pos)) = &*objects.preview_scroll_pos.borrow() {
            let new_pos = evt.root();
            let move_x = new_pos.0 - start_mouse_pos.0;
            let move_y = new_pos.1 - start_mouse_pos.1;
            let hadjustment = preview_img_scr.hadjustment();
            hadjustment.set_value(start_scroll_pos.0 - SCROLL_SPEED*move_x);
            let vadjustment = preview_img_scr.vadjustment();
            vadjustment.set_value(start_scroll_pos.1 - SCROLL_SPEED*move_y);
        }
        Inhibit(false)
    }));
}

fn conn_action<F: Fn(&MainWindowObjectsPtr) + 'static>(
    objects:  &MainWindowObjectsPtr,
    act_name: &str,
    fun:      F
) {
    let action = gio::SimpleAction::new(act_name, None);
    action.connect_activate(clone!(@strong objects => move |_, _| {
        fun(&objects);
    }));
    objects.window.add_action(&action);
}

fn enable_action(window: &gtk::ApplicationWindow, action_name: &str, enabled: bool) {
    if let Some(action) = window.lookup_action(action_name) {
        let sa = action
            .downcast::<gio::SimpleAction>()
            .expect("Is not gio::SimpleAction");
        sa.set_enabled(enabled);
    } else {
        panic!("Action {} not found", action_name);
    }
}

fn ask_user_to_save_project(objects: &MainWindowObjectsPtr) -> bool {
    if !objects.project.borrow().changed { return true; }

    let dialog = gtk::MessageDialog::builder()
        .transient_for(&objects.window)
        .title("Close application")
        .text("Project changes. Save?")
        .modal(true)
        .message_type(gtk::MessageType::Question)
        .build();

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            ("Yes", gtk::ResponseType::Yes),
            ("No", gtk::ResponseType::No),
            ("Cancel", gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            ("Cancel", gtk::ResponseType::Cancel),
            ("No", gtk::ResponseType::No),
            ("Yes", gtk::ResponseType::Yes),
        ]);
    }

    let resp = dialog.run();
    dialog.close();

    match resp {
        gtk::ResponseType::Yes => {
            action_save_project(objects);
            objects.project.borrow_mut().changed = false;
            true
        },
        gtk::ResponseType::No => {
            objects.project.borrow_mut().changed = false;
            true
        },
        _ => {
            false
        },
    }
}

struct MainWindowObjects {
    project: RefCell<Project>,
    config: RefCell<Config>,

    window: gtk::ApplicationWindow,
    menu_bar: gtk::MenuBar,

    prj_tree: gtk::TreeView,
    prj_tree_changed_flag: RefCell<bool>,
    prj_tree_menu: gtk::Menu,
    prj_img_paned: gtk::Paned,

    progress_bar: gtk::ProgressBar,
    progress_cont: gtk::Widget,
    progress_text: gtk::Label,

    preview_image: gtk::Image,
    preview_img_scale: gtk::ComboBoxText,
    preview_auto_min: gtk::CheckButton,
    preview_img_gamma: gtk::Scale,
    last_preview_file: RefCell<PathBuf>,
    preview_tp: rayon::ThreadPool,
    prev_preview_cancel_flags: RefCell<Option<Arc<AtomicBool>>>,
    prev_preview_img: RefCell<image::Image>,
    preview_file_name: gtk::Label,
    preview_ctrls_box: gtk::Widget,
    preview_scroll_pos: RefCell<Option<((f64, f64), (f64, f64))>>,

    mi_dark_theme: gtk::RadioMenuItem,
    mi_light_theme: gtk::RadioMenuItem,
    mi_change_file_type: gtk::MenuItem,

    icon_folder: Option<gdk_pixbuf::Pixbuf>,
    icon_image: Option<gdk_pixbuf::Pixbuf>,
    icon_photo: Option<gdk_pixbuf::Pixbuf>,
    icon_ref_image: Option<gdk_pixbuf::Pixbuf>,

    cancel_flag: Arc<AtomicBool>,
    last_selected_path: RefCell<PathBuf>,
    move_to_group_last_uuid: RefCell<String>,
}

type MainWindowObjectsPtr = Rc::<MainWindowObjects>;

fn assign_config(objects: &MainWindowObjectsPtr) {
    let mut config = objects.config.borrow_mut();
    config.prj_tree_width = objects.prj_img_paned.position();

    if objects.mi_dark_theme.is_active() {
        config.theme = Theme::Dark;
    } else if objects.mi_light_theme.is_active() {
        config.theme = Theme::Light;
    }

    config.prj_tree_cols.clear();
    for col in objects.prj_tree.columns() {
        config.prj_tree_cols.push(PrjTreeCol {
            width: col.width(),
            visible: col.is_visible(),
            pos: -1,
        })
    }

    let (width, height) = objects.window.size();
    config.main_win_width = width;
    config.main_win_height = height;
    config.main_win_maximized = objects.window.is_maximized();
}

fn apply_config(objects: &MainWindowObjectsPtr) {
    let config = objects.config.borrow();

    match config.theme {
        Theme::Dark     => {
            action_dark_theme(objects);
            objects.mi_dark_theme.set_active(true);
        },
        Theme::Light    => {
            action_light_theme(objects);
            objects.mi_light_theme.set_active(true);
        },
        Theme::Other(_) => (),
    }

    if config.prj_tree_width != -1 {
        objects.prj_img_paned.set_position(config.prj_tree_width);
    }

    if config.prj_tree_cols.len() == objects.prj_tree.n_columns() as usize {
        for (i, col) in config.prj_tree_cols.iter().enumerate() {
            let tree_col = objects.prj_tree.column(i as i32).unwrap();
            tree_col.set_fixed_width(col.width);
        }
    }

    if config.main_win_width != -1 && config.main_win_height != -1 {
        objects.window.resize(config.main_win_width, config.main_win_height);
    }

    if config.main_win_maximized {
        objects.window.maximize();
    }

    match config.preview_scale {
        ImgScale::Original =>
            objects.preview_img_scale.set_active(Some(0)),
        ImgScale::FitWindow =>
            objects.preview_img_scale.set_active(Some(1)),
    }

    objects.preview_auto_min.set_active(config.preview_auto_min);

    objects.preview_img_gamma.set_value(config.preview_gamma as f64);
}

#[derive(PartialEq, Clone)]
enum SelItemType {
    None,
    Project,
    Group,
    FileType,
    File
}

#[derive(Clone)]
struct SelectedItem {
    item_type: SelItemType,
    group_idx: Option<usize>,
    file_type: Option<ProjectFileType>,
    files: Vec<usize>,
}

impl SelectedItem {
    fn new_empty() -> Self {
        Self {
            item_type: SelItemType::None,
            group_idx: None,
            file_type: None,
            files: Vec::new(),
        }
    }
}

fn get_selection_for_path(path: &gtk::TreePath) -> SelectedItem {
    let mut result = SelectedItem::new_empty();
    let values = path.indices();

    if values.len() >= 2 {
        result.group_idx = Some(values[1] as usize);
    }

    if values.len() >= 3 {
        result.file_type = Some(match values[2] {
            0 => ProjectFileType::Light,
            1 => ProjectFileType::Dark,
            2 => ProjectFileType::Flat,
            3 => ProjectFileType::Bias,
            _ => panic!("Wrong folder index ({})", values[2]),
        });
    }

    if values.len() >= 4 {
        result.files.push(values[3] as usize);
    }

    match values.len() {
        1 => result.item_type = SelItemType::Project,
        2 => result.item_type = SelItemType::Group,
        3 => result.item_type = SelItemType::FileType,
        4 => result.item_type = SelItemType::File,
        _ => (),
    }

    result
}

fn get_current_selection(objects: &MainWindowObjectsPtr) -> SelectedItem {
    let model = match objects.prj_tree.model() {
        Some(model) =>
            model.downcast::<gtk::TreeModelSort>().unwrap(),
        None =>
            return SelectedItem::new_empty(),
    };

    let items = objects.prj_tree
        .selection()
        .selected_rows().0
        .iter()
        .filter_map(|path| model.convert_path_to_child_path(path))
        .map(|path| get_selection_for_path(&path))
        .collect::<Vec<_>>();

    if items.len() == 1 {
        return items[0].clone();
    } else if items.len() > 1 {
        let not_file_selection = items
            .iter()
            .find(|sel| sel.item_type != SelItemType::File || sel.files.len() != 1);

        if not_file_selection.is_some() {
            return SelectedItem::new_empty();
        }

        let group = items[0].group_idx;
        let file_type = items[0].file_type;

        let not_same_group_or_filetype = items
            .iter()
            .find(|sel| sel.group_idx != group || sel.file_type != file_type);

        if not_same_group_or_filetype.is_some() {
            return SelectedItem::new_empty();
        }

        let mut result = items[0].clone();
        for item in items.iter().skip(1) {
            result.files.push(item.files[0]);
        }

        return result;
    }

    SelectedItem::new_empty()
}

fn action_new_project(objects: &MainWindowObjectsPtr) {
    let can_create_new_project = ask_user_to_save_project(objects);
    if !can_create_new_project { return; }
    let mut new_project = Project::new();
    new_project.make_default();
    *objects.project.borrow_mut() = new_project;
    fill_project_tree(objects);
    log::info!("New project created");
}

fn action_open_project(objects: &MainWindowObjectsPtr) {
    let can_opened = ask_user_to_save_project(objects);
    if !can_opened { return; }

    let ff = create_file_filter_for_project();
    let fc = gtk::FileChooserDialog::builder()
        .action(gtk::FileChooserAction::Open)
        .title("Select project file to open")
        .filter(&ff)
        .modal(true)
        .transient_for(&objects.window)
        .build();

    fc.set_current_folder(objects.last_selected_path.borrow().clone());

    if cfg!(target_os = "windows") {
        fc.add_buttons(&[
            ("_Open", gtk::ResponseType::Accept),
            ("_Cancel", gtk::ResponseType::Cancel),
        ]);
    } else {
        fc.add_buttons(&[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Open", gtk::ResponseType::Accept),
        ]);
    }

    fc.connect_response(clone!(@strong objects => move |file_chooser, response| {
        if response == gtk::ResponseType::Accept {
            let file_name = file_chooser.file().expect("Can't get file_name");
            let path = file_name.path().unwrap();

            if let Some(cur_folder) = file_chooser.current_folder() {
                *objects.last_selected_path.borrow_mut() = cur_folder;
            }

            open_project(&objects, &path);
        }
        file_chooser.close();
    }));

    fc.show();
}

fn open_project(objects: &MainWindowObjectsPtr, path: &PathBuf) {
    let res = objects.project.borrow_mut().load(&path);
    if let Err(err) = res {
        show_error_message(&err.to_string(), &objects);
        log::error!("'{}' during opening project", err.to_string());
    } else {
        objects.project.borrow_mut().file_name = Some(path.clone());
        fill_project_tree(&objects);
        update_project_name_and_time_in_gui(&objects, true, true);
        log::info!("Project {} opened", path.to_str().unwrap_or(""));
    }
}

fn action_save_project(objects: &MainWindowObjectsPtr) {
    let file_name = objects.project.borrow().file_name.clone();
    if let Some(file_name) = file_name {
        save_project(objects, &file_name);
    } else {
        action_save_project_as(objects);
    }
}

fn action_save_project_as(objects: &MainWindowObjectsPtr) {
    let ff = create_file_filter_for_project();
    let fc = gtk::FileChooserDialog::builder()
        .action(gtk::FileChooserAction::Save)
        .title("Select project file to save")
        .filter(&ff)
        .modal(true)
        .transient_for(&objects.window)
        .build();

    if cfg!(target_os = "windows") {
        fc.add_buttons(&[
            ("_Save", gtk::ResponseType::Accept),
            ("_Cancel", gtk::ResponseType::Cancel),
        ]);
    } else {
        fc.add_buttons(&[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Save", gtk::ResponseType::Accept),
        ]);
    }

    if let Some(file_name) = objects.project.borrow().file_name.clone() {
        let _ = fc.set_file(&gio::File::for_path(file_name));
    }

    let resp = fc.run();
    fc.close();

    if resp == gtk::ResponseType::Accept {
        let file_name = fc.file().expect("Can't get file_name");
        let path = file_name
            .path()
            .unwrap()
            .with_extension("au_proj");
        let project_name = path.with_extension("")
            .file_name()
            .and_then(|v| v.to_str())
            .unwrap_or("")
            .to_string();

        if let Some(cur_folder) = fc.current_folder() {
            *objects.last_selected_path.borrow_mut() = cur_folder;
        }

        objects.project.borrow_mut().config.name = Some(project_name);

        let ok = save_project(&objects, &path);
        if !ok { return; }

        update_project_name_and_time_in_gui(&objects, true, true);
    }
}

fn save_project(objects: &MainWindowObjectsPtr, file_name: &PathBuf) -> bool {
    let res = objects.project.borrow_mut().save(&file_name);
    if let Err(err) = res {
        show_error_message(&err.to_string(), &objects);
        log::error!("'{}' during saving project", err.to_string());
        false
    } else {
        objects.project.borrow_mut().file_name = Some(file_name.clone());
        log::info!("Project {} saved", file_name.to_str().unwrap_or(""));
        true
    }
}

fn action_exit(objects: &MainWindowObjectsPtr) {
    log::info!("Exit called");
    objects.window.close();
}

fn action_add_light_files(objects: &MainWindowObjectsPtr) {
    select_and_add_files_into_project(
        objects,
        ProjectFileType::Light,
        "Select LIGHT files",
        "Add light files",
    );
}

fn action_add_dark_files(objects: &MainWindowObjectsPtr) {
    select_and_add_files_into_project(
        objects,
        ProjectFileType::Dark,
        "Select DARK files",
        "Add dark files",
    );
}

fn action_add_flat_files(objects: &MainWindowObjectsPtr) {
    select_and_add_files_into_project(
        objects,
        ProjectFileType::Flat,
        "Select FLAT files",
        "Add flat files",
    );
}

fn action_add_bias_files(objects: &MainWindowObjectsPtr) {
    select_and_add_files_into_project(
        objects,
        ProjectFileType::Bias,
        "Select BIAS files",
        "Add bias files",
    );
}

fn select_and_add_files_into_project(
    objects:          &MainWindowObjectsPtr,
    file_type:        ProjectFileType,
    files_dialog_cap: &'static str,
    select_group_cap: &'static str,
) {
    let fc = create_src_file_select_dialog(files_dialog_cap, objects, true);
    fc.connect_response(clone!(@strong objects => move |file_chooser, response| {
        if response == gtk::ResponseType::Accept {
            let group_index = get_active_group_index(&objects, select_group_cap).
                or_else(|| {
                    if objects.project.borrow().groups.is_empty() {
                        Some(0)
                    } else {
                        None
                    }
                });

            if let Some(group_index) = group_index {
                let user_selected_files = get_filenames_from_file_vec(file_chooser.files());
                log::info!("Dialog '{}' confirmed", select_group_cap);
                add_files_into_project(
                    user_selected_files,
                    &objects,
                    group_index,
                    file_type
                );
            }
            if let Some(cur_folder) = file_chooser.current_folder() {
                *objects.last_selected_path.borrow_mut() = cur_folder;
            }
        }
        file_chooser.close();
    }));
    fc.show();

    fn add_files_into_project(
        mut file_names:   Vec<PathBuf>,
        objects:          &MainWindowObjectsPtr,
        group_iter_index: usize,
        file_type:        ProjectFileType
    ) {
        {
            let project = objects.project.borrow();
            let group = project.groups.get(group_iter_index);
            if let Some(group) = group {
                let files = &group.get_file_list_by_type(file_type);
                files.retain_files_if_they_are_not_here(&mut file_names);
            }
        }

        exec_and_show_progress(
            objects,
            move |progress, cancel_flag| {
                load_src_file_info_for_files(&file_names, &cancel_flag, &progress)
            },
            move |objects, result| {
                let helper = TreeViewFillHelper::new(&objects.project.borrow());
                {
                    let mut project = objects.project.borrow_mut();
                    project.add_default_group_if_empty();
                    let group = &mut project.groups[group_iter_index];
                    let files = &mut group.get_file_list_by_type_mut(file_type);
                    log::info!("Added {} files", result.len());
                    files.add_files(result);
                    project.changed = true;
                }
                helper.apply_changes(objects, true);
            }
        );
    }
}

fn create_file_filter_for_project() -> gtk::FileFilter {
    let ff = gtk::FileFilter::new();
    ff.set_name(Some("Astro-utils project"));
    ff.add_pattern("*.au_proj");
    ff
}

fn create_src_file_select_dialog(
    title:       &str,
    objects:     &MainWindowObjectsPtr,
    light_files: bool
) -> gtk::FileChooserDialog {
    let ff = gtk::FileFilter::new();
    ff.set_name(Some("Source files"));
    let add_exts = |exts| {
        for ext in exts {
            let mut pattern = format!("*.{}", ext).to_ascii_lowercase();
            ff.add_pattern(&pattern);
            pattern = pattern.to_ascii_uppercase();
            ff.add_pattern(&pattern);
        }
    };
    add_exts(RAW_EXTS);
    if light_files {
        add_exts(TIF_EXTS);
        add_exts(FIT_EXTS);
    }
    let fc = FileChooserDialogBuilder::new()
        .action(gtk::FileChooserAction::Open)
        .title(title)
        .filter(&ff)
        .modal(true)
        .transient_for(&objects.window)
        .select_multiple(true)
        .build();

    fc.set_current_folder(objects.last_selected_path.borrow().clone());

    if cfg!(target_os = "windows") {
        fc.add_buttons(&[
            ("_Open", gtk::ResponseType::Accept),
            ("_Cancel", gtk::ResponseType::Cancel),
        ]);
    } else {
        fc.add_buttons(&[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Open", gtk::ResponseType::Accept),
        ]);
    }

    fc
}

fn get_filenames_from_file_vec(files: Vec<gio::File>) -> Vec<PathBuf> {
    files
        .iter()
        .filter_map(|f| f.path())
        .collect()
}

fn action_register(objects: &MainWindowObjectsPtr) {
    log::info!("Registering light files started");
    let project = objects.project.borrow().clone();
    let cpu_load = objects.config.borrow().cpu_load;
    exec_and_show_progress(
        objects,
        move |progress, cancel_flag| {
            project.register_light_files(progress, cancel_flag, cpu_load)
        },
        move |objects, result| {
            let helper = TreeViewFillHelper::new(&objects.project.borrow());
            objects.project.borrow_mut().update_light_files_reg_info(result);
            helper.apply_changes(&objects, true);
        }
    );
}

fn action_light_theme(_: &MainWindowObjectsPtr) {
    let settings = gtk::Settings::default().unwrap();
    settings.set_property("gtk-theme-name", "Adwaita");
    log::info!("Light theme selected");
}

fn action_dark_theme(_: &MainWindowObjectsPtr) {
    let settings = gtk::Settings::default().unwrap();
    settings.set_property("gtk-theme-name", "Skeuos-Blue-Dark");
    log::info!("Dark theme selected");
}

fn show_message(
    objects:  &MainWindowObjects,
    title:    &str,
    text:     &str,
    msg_type: gtk::MessageType,
) {
    let dialog = gtk::MessageDialog::builder()
        .transient_for(&objects.window)
        .title(title)
        .text(text)
        .modal(true)
        .message_type(msg_type)
        .buttons(gtk::ButtonsType::Close)
        .build();

    dialog.show();

    dialog.connect_response(move |dlg, _| {
        dlg.close();
    });

}

fn show_error_message(text: &str, objects: &MainWindowObjects) {
    log::error!("Show error message: {}", text);
    show_message(
        objects,
        "Error",
        text,
        gtk::MessageType::Error,
    );
}

fn fill_project_tree(objects: &MainWindowObjectsPtr) {
    objects.prj_tree.set_model(None::<&gtk::TreeStore>);
    let helper = TreeViewFillHelper::new_emplty();
    helper.apply_changes(objects, true);
}

fn get_project_title(project: &Project) -> String {
    let mut result = project.config.name
        .clone()
        .unwrap_or("Unnamed project".to_string());

    if project.config.image_size == ImageSize::Bin2x2 {
        result.push_str(" - bin 2x2");
    }

    let time = project.calc_time();
    if time != 0.0 {
        result.push_str(&format!(
            " - {} total",
            seconds_to_total_time_str(time)
        ));
    }

    result
}

fn update_project_name_and_time_in_gui(objects: &MainWindowObjectsPtr, title: bool, tree: bool) {
    if title {
        objects.window.set_title(&format!(
            "[{}] - {} v{} - {}",
            get_project_title(&objects.project.borrow()),
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION"),
            env!("CARGO_PKG_DESCRIPTION"),
        ));
    }

    if tree {
        let tree_helper = TreeViewFillHelper::new(&objects.project.borrow());
        tree_helper.apply_changes(objects, false);
    }
}

fn enable_progress_bar(objects: &MainWindowObjectsPtr, enable: bool) {
    objects.progress_cont.set_visible(enable);
    objects.menu_bar.set_sensitive(!enable);
    objects.prj_tree_menu.set_sensitive(!enable);

    if enable {
        objects.progress_bar.set_fraction(0.0);
        objects.progress_bar.set_text(None);
    }
}

const COLUMN_FILE_NAME:    u32 = 0;
const COLUMN_FILE_PATH:    u32 = 1;
const COLUMN_FILE_TIME:    u32 = 2;
const COLUMN_ISO:          u32 = 3;
const COLUMN_EXP:          u32 = 4;
const COLUMN_DIM:          u32 = 5;
const COLUMN_NOISE:        u32 = 6;
const COLUMN_BACKGROUND:   u32 = 7;
const COLUMN_STARS_FWHM:   u32 = 8;
const COLUMN_STARS_R_DEV:  u32 = 9;
const COLUMN_SHARPNESS:    u32 = 10;

const COLUMN_ICON:         u32 = 11;
const COLUMN_CHECKBOX:     u32 = 12;
const COLUMN_CHECKBOX_VIS: u32 = 13;

fn get_prj_tree_col_items() -> [(&'static str, u32, glib::Type); 14] {
    [
        ("Project/File name", COLUMN_FILE_NAME,    String::static_type()),
        ("File path",         COLUMN_FILE_PATH,    String::static_type()),
        ("File time",         COLUMN_FILE_TIME,    String::static_type()),
        ("ISO",               COLUMN_ISO,          String::static_type()),
        ("Exposure",          COLUMN_EXP,          String::static_type()),
        ("Dimensions",        COLUMN_DIM,          String::static_type()),
        ("Noise",             COLUMN_NOISE,        String::static_type()),
        ("Background",        COLUMN_BACKGROUND,   String::static_type()),
        ("Stars FWHM",        COLUMN_STARS_FWHM,   String::static_type()),
        ("Stars R dev",       COLUMN_STARS_R_DEV,  String::static_type()),
        ("Sharpness",         COLUMN_SHARPNESS,    String::static_type()),
        ("",                  COLUMN_ICON,         gdk_pixbuf::Pixbuf::static_type()),
        ("",                  COLUMN_CHECKBOX,     bool::static_type()),
        ("",                  COLUMN_CHECKBOX_VIS, bool::static_type()),
    ]
}

struct PrjTreeFillHelperGroup {
    idx: usize,
    light_names: HashMap<PathBuf, usize>,
    dark_names: HashMap<PathBuf, usize>,
    flat_names: HashMap<PathBuf, usize>,
    bias_names: HashMap<PathBuf, usize>,
}

impl PrjTreeFillHelperGroup {
    fn new() -> PrjTreeFillHelperGroup {
        PrjTreeFillHelperGroup {
            idx: 0,
            light_names: HashMap::new(),
            dark_names: HashMap::new(),
            flat_names: HashMap::new(),
            bias_names: HashMap::new(),
        }
    }
}

struct TreeViewFillHelper {
    groups: HashMap<String, PrjTreeFillHelperGroup>,
}

impl TreeViewFillHelper {
    fn new_emplty() -> TreeViewFillHelper {
        TreeViewFillHelper {
            groups: HashMap::new(),
        }
    }

    fn new(project: &Project) -> TreeViewFillHelper {
        let mut groups = HashMap::new();
        for (idx, project_group) in project.groups.iter().enumerate() {
            let group = PrjTreeFillHelperGroup {
                idx,
                light_names: project_group.light_files.list
                    .iter()
                    .enumerate()
                    .map(|(idx, f)| (f.file_name.clone(), idx))
                    .collect(),
                dark_names: project_group.dark_files.list
                    .iter()
                    .enumerate()
                    .map(|(idx, f)| (f.file_name.clone(), idx))
                    .collect(),
                flat_names: project_group.flat_files.list
                    .iter()
                    .enumerate()
                    .map(|(idx, f)| (f.file_name.clone(), idx))
                    .collect(),
                bias_names: project_group.bias_files.list
                    .iter()
                    .enumerate()
                    .map(|(idx, f)| (f.file_name.clone(), idx))
                    .collect(),
            };
            groups.insert(project_group.uuid.clone(), group);
        }
        TreeViewFillHelper { groups }
    }

    fn apply_changes(&self, objects: &MainWindowObjectsPtr, update_files: bool) {
        *objects.prj_tree_changed_flag.borrow_mut() = true;

        let mut ids_to_delete = Vec::new();
        let dummy_group = PrjTreeFillHelperGroup::new();
        let project = objects.project.borrow();

        let tree_store = match objects.prj_tree.model() {
            Some(model) => {
                let sorted_model = model.downcast::<gtk::TreeModelSort>().unwrap();
                let tree_store = sorted_model.model().downcast::<gtk::TreeStore>().unwrap();
                tree_store
            }
            None => {
                let col_types = get_prj_tree_col_items()
                    .iter()
                    .map(|(_, _, tp)| *tp)
                    .collect::<Vec<_>>();

                let result = gtk::TreeStore::new(&col_types);
                result.insert_with_values(None, None, &[
                    (COLUMN_ICON, &objects.icon_photo),
                ]);

                let sorted_model = gtk::TreeModelSort::new(&result);
                objects.prj_tree.set_model(Some(&sorted_model));
                result
            },
        };

        let project_iter = tree_store.iter_first().unwrap();
        let project_name = get_project_title(&project);

        let project_path = project.file_name
            .as_ref()
            .and_then(|v|v.parent().and_then(|p| p.to_str()))
            .unwrap_or("");

        tree_store.set(&project_iter, &[
            (COLUMN_FILE_NAME, &project_name),
            (COLUMN_FILE_PATH, &project_path),
        ]);

        // add/update groups
        for (idx, project_group) in project.groups.iter().enumerate() {
            let prev_group = self.groups.get(&project_group.uuid);
            let (group_iter, group_added) = if prev_group.is_none() {
                let iter = tree_store.insert_with_values(
                    Some(&project_iter),
                    None,
                    &[(COLUMN_ICON, &objects.icon_folder)]
                );
                (iter, true)
            } else {
                (tree_store.iter_nth_child(Some(&project_iter), idx as i32).unwrap(), false)
            };

            let group_name = project_group.name(idx);
            tree_store.set(&group_iter, &[
                (COLUMN_FILE_NAME,    &group_name),
                (COLUMN_CHECKBOX,     &project_group.used),
                (COLUMN_CHECKBOX_VIS, &true),
            ]);

            let prev_group = if let Some(prev_group) = prev_group {
                &prev_group
            } else {
                &dummy_group
            };

            let data_to_update = &[
                ("Light files", &project_group.light_files, &prev_group.light_names, true),
                ("Dark files",  &project_group.dark_files,  &prev_group.dark_names,  true),
                ("Flat files",  &project_group.flat_files,  &prev_group.flat_names,  false),
                ("Bias files",  &project_group.bias_files,  &prev_group.bias_names,  false),
            ];

            for (idx, (folder_name, project_files, prev_files, show_total_time)) in data_to_update.iter().cloned().enumerate() {
                let files_iter = if let Some(iter) = tree_store.iter_nth_child(Some(&group_iter), idx as i32) {
                    iter
                } else {
                    tree_store.insert_with_values(
                        Some(&group_iter),
                        None,
                        &[(COLUMN_ICON, &objects.icon_folder)]
                    )
                };

                if update_files {
                    // add/update files
                    for project_file in project_files.list.iter() {
                        let prev_file_idx = prev_files.get(&project_file.file_name);
                        let file_iter = if let Some(prev_file_idx) = prev_file_idx {
                            tree_store.iter_nth_child(Some(&files_iter), *prev_file_idx as i32).unwrap()
                        } else {
                            tree_store.insert_with_values(Some(&files_iter), None, &[
                                (COLUMN_ICON, &objects.icon_image),
                            ])
                        };

                        let file_name = project_file
                            .file_name
                            .file_name()
                            .and_then(|v| v.to_str())
                            .unwrap_or("");

                        let path = project_file
                            .file_name
                            .parent()
                            .and_then(|v|v.to_str())
                            .unwrap_or("");

                        let file_time_str = if let Some(file_time) = project_file.file_time {
                            file_time.format("%Y-%m-%d %H:%M:%S").to_string()
                        } else {
                            String::new()
                        };

                        let iso_str = if let Some(iso) = project_file.iso {
                            format!("{}", iso)
                        } else {
                            String::new()
                        };

                        let exp_str = if let Some(exp) = project_file.exp {
                            format!("{:.1} s", exp)
                        } else {
                            String::new()
                        };

                        let dim_str = if let (Some(width), Some(height)) = (project_file.width, project_file.height) {
                            format!("{} x {}", width, height)
                        } else {
                            String::new()
                        };

                        let noise_str = if let Some(reg_info) = &project_file.reg_info {
                            format!("{:.7}", reg_info.noise)
                        } else {
                            String::new()
                        };

                        let background_str = if let Some(reg_info) = &project_file.reg_info {
                            format!("{:.4}", reg_info.background)
                        } else {
                            String::new()
                        };

                        let star_r_str = if let Some(reg_info) = &project_file.reg_info {
                            format!("{:.3}", reg_info.stars_r)
                        } else {
                            String::new()
                        };

                        let star_r_dev_str = if let Some(reg_info) = &project_file.reg_info {
                            format!("{:.3}", reg_info.stars_r_dev)
                        } else {
                            String::new()
                        };

                        let sharpness = if let Some(reg_info) = &project_file.reg_info {
                            format!("{:.3}", reg_info.sharpness)
                        } else {
                            String::new()
                        };

                        let is_ref_file = Some(&project_file.file_name) == project.ref_image.as_ref();
                        let icon = if is_ref_file { &objects.icon_ref_image } else { &objects.icon_image };

                        tree_store.set(&file_iter, &[
                            (COLUMN_ICON,         icon),
                            (COLUMN_CHECKBOX,     &project_file.used),
                            (COLUMN_CHECKBOX_VIS, &true),
                            (COLUMN_FILE_NAME,    &file_name),
                            (COLUMN_FILE_PATH,    &path),
                            (COLUMN_FILE_TIME,    &file_time_str),
                            (COLUMN_ISO,          &iso_str),
                            (COLUMN_EXP,          &exp_str),
                            (COLUMN_DIM,          &dim_str),
                            (COLUMN_NOISE,        &noise_str),
                            (COLUMN_BACKGROUND,   &background_str),
                            (COLUMN_STARS_FWHM,   &star_r_str),
                            (COLUMN_STARS_R_DEV,  &star_r_dev_str),
                            (COLUMN_SHARPNESS,    &sharpness),
                        ]);
                    }

                    // Delete files
                    let mut poject_files_set = HashSet::new();
                    for project_file in project_files.list.iter() {
                        poject_files_set.insert(project_file.file_name.clone());
                    }
                    ids_to_delete.clear();
                    for (prev_file, idx) in prev_files.iter() {
                        if !poject_files_set.contains(prev_file) {
                            ids_to_delete.push(*idx);
                        }
                    }
                    ids_to_delete.sort_by(|idx1, idx2| idx1.partial_cmp(idx2).unwrap().reverse());
                    for idx in ids_to_delete.iter() {
                        let iter = tree_store.iter_nth_child(Some(&files_iter), *idx as i32).unwrap();
                        tree_store.remove(&iter);
                    }
                }

                // Files folder name
                let folder_text = if project_files.list.is_empty() {
                    folder_name.to_string()
                } else if show_total_time {
                    format!(
                        "{} [{}] ({})",
                        folder_name,
                        seconds_to_total_time_str(project_files.calc_total_exp_time()),
                        project_files.list.len()
                    )
                } else {
                    format!("{} ({})", folder_name, project_files.list.len())
                };

                tree_store.set(
                    &files_iter,
                    &[(COLUMN_FILE_NAME, &folder_text)]
                );
            }

            if group_added {
                let group_path = tree_store.path(&group_iter).unwrap();
                objects.prj_tree.expand_to_path(&group_path);
            }
        }

        // delete groups
        ids_to_delete.clear();
        for (uuid, group) in self.groups.iter() {
            if !project.group_exists(uuid) {
                ids_to_delete.push(group.idx);
            }
        }
        ids_to_delete.sort_by(|idx1, idx2| idx2.partial_cmp(idx1).unwrap());
        for idx in ids_to_delete.iter() {
            let group_iter = tree_store.iter_nth_child(Some(&project_iter), *idx as i32).unwrap();
            tree_store.remove(&group_iter);
        }

        let project_path = tree_store.path(&project_iter).unwrap();
        objects.prj_tree.expand_to_path(&project_path);

        *objects.prj_tree_changed_flag.borrow_mut() = false;
    }
}

fn handler_project_tree_selection_changed(objects: &MainWindowObjectsPtr) {
    if *objects.prj_tree_changed_flag.borrow() {
        return;
    }
    preview_selected_file(objects);
    let selection = get_current_selection(objects);
    let is_file = selection.item_type == SelItemType::File;
    let is_group = selection.item_type == SelItemType::Group;
    let is_file_type = selection.item_type == SelItemType::FileType;
    let support_item_properties =
        selection.item_type == SelItemType::Project || is_group;
    let support_delete = is_group || is_file;
    let support_use_as_ref_image =
        is_file &&
        selection.file_type == Some(ProjectFileType::Light) &&
        selection.files.len() == 1;
    enable_action(&objects.window, "item_properties", support_item_properties);
    enable_action(&objects.window, "delete_item", support_delete);
    enable_action(&objects.window, "use_as_ref_image", support_use_as_ref_image);
    enable_action(&objects.window, "move_file_to_group", is_file);
    objects.mi_change_file_type.set_sensitive(is_file);
    enable_action(
        &objects.window,
        "change_file_to_light",
        selection.file_type != Some(ProjectFileType::Light)
    );
    enable_action(
        &objects.window,
        "change_file_to_dark",
        selection.file_type != Some(ProjectFileType::Dark)
    );
    enable_action(
        &objects.window,
        "change_file_to_flat",
        selection.file_type != Some(ProjectFileType::Flat)
    );
    enable_action(
        &objects.window,
        "change_file_to_bias",
        selection.file_type != Some(ProjectFileType::Bias)
    );
    enable_action(&objects.window, "check_all_files", is_file || is_file_type);
    enable_action(&objects.window, "uncheck_all_files", is_file || is_file_type);
    enable_action(&objects.window, "check_selected_files", is_file);
    enable_action(&objects.window, "uncheck_selected_files", is_file);
}

fn preview_selected_file(objects: &MainWindowObjectsPtr) {
    let file_name = {
        match get_current_selection(objects) {
            SelectedItem {
                item_type: SelItemType::File,
                group_idx: Some(group_idx),
                file_type: Some(file_type),
                files,
                ..
            } if !files.is_empty() => {
                let project = objects.project.borrow();
                let file_list = project.groups[group_idx].get_file_list_by_type(file_type);
                let project_file = &file_list.list[files[0]];
                project_file.file_name.clone()
            },

            SelectedItem {
                item_type: SelItemType::Project,
                ..
            } => {
                let project = objects.project.borrow();
                match project.get_result_file_name() {
                    Ok(file_name) => file_name,
                    Err(_) => return,
                }
            },

            _ => return,
        }
    };

    if *objects.last_preview_file.borrow() == file_name {
        return;
    }

    preview_image_file(objects, &file_name);
}

fn preview_image_after_change_view_opts(objects: &MainWindowObjectsPtr) {
    let image = objects.prev_preview_img.borrow();
    if image.is_empty() { return; }

    let auto_min_flag = objects.config.borrow().preview_auto_min;
    let scale = objects.config.borrow().preview_scale;
    let gamma = objects.config.borrow().preview_gamma;

    let bytes = convert_image_to_bytes(&image, gamma, auto_min_flag);
    show_preview_image(
        objects,
        bytes,
        image.width() as i32,
        image.height() as i32,
        scale
    );
}

fn preview_image_file(objects: &MainWindowObjectsPtr, file_name: &PathBuf) {
    enum UiMessage {
        Image{ image: image::Image, file_name: PathBuf },
        Error(String),
    }

    let auto_min_flag = objects.config.borrow().preview_auto_min;
    let scale = objects.config.borrow().preview_scale;
    let gamma = objects.config.borrow().preview_gamma;

    // cancel previous task in thread pool
    if let Some(flag) = &*objects.prev_preview_cancel_flags.borrow() {
        flag.store(true, Ordering::Relaxed);
    }

    let (sender, receiver) = MainContext::channel(PRIORITY_DEFAULT);

    receiver.attach(
        None,
        clone!(@strong objects => move |message: UiMessage| {
            match message {
                UiMessage::Image { image, file_name } => {
                    show_preview_image(
                        &objects,
                        convert_image_to_bytes(&image, gamma, auto_min_flag),
                        image.width() as i32,
                        image.height() as i32,
                        scale
                    );
                    objects.preview_file_name.set_label(file_name.to_str().unwrap_or(""));
                    *objects.last_preview_file.borrow_mut() = file_name.clone();
                    *objects.prev_preview_img.borrow_mut() = image;
                    objects.preview_ctrls_box.set_sensitive(true);
                },

                UiMessage::Error(text) => {
                    objects.preview_ctrls_box.set_sensitive(true);
                    objects.preview_file_name.set_label(&text);
                    objects.preview_image.clear();
                    objects.prev_preview_img.borrow_mut().clear();
                    objects.last_preview_file.borrow_mut().clear();
                },
            }
            Continue(false)
        }),
    );

    let cancel_flag = Arc::new(AtomicBool::new(false));
    *objects.prev_preview_cancel_flags.borrow_mut() = Some(cancel_flag.clone());
    objects.preview_file_name.set_label("???");
    objects.preview_ctrls_box.set_sensitive(false);

    objects.preview_tp.spawn(clone!(@strong file_name, @strong cancel_flag => move  || {
        if cancel_flag.load(Ordering::Relaxed) { return; }
        let calibr_data = CalibrationData::new_empty();

        let mut load_flags = if cfg!(debug_assertions) {
            LoadLightFlags::STARS
        } else {
            LoadLightFlags::empty()
        };
        load_flags |= LoadLightFlags::FAST_DEMOSAIC;

        let light_file = LightFile::load(
            &file_name,
            &calibr_data,
            None,
            load_flags,
            1
        );

        match light_file {
            Ok(mut light_file) => {
                light_file.image.normalize_if_greater_1();

                // fill stars with green for debug purposes
                if cfg!(debug_assertions) {
                    for star in &light_file.stars {
                        for pt in &star.points {
                            if light_file.image.is_rgb() {
                                light_file.image.r.set(pt.x, pt.y, 0.0);
                                light_file.image.g.set(pt.x, pt.y, 1.0);
                                light_file.image.b.set(pt.x, pt.y, 0.0);
                            } else if light_file.image.is_greyscale() {
                                light_file.image.l.set(pt.x, pt.y, 0.5);
                            }
                        }
                    }
                }

                sender.send(UiMessage::Image {
                    image: light_file.image,
                    file_name
                }).unwrap();
            },
            Err(error) => {
                sender.send(UiMessage::Error(error.to_string())).unwrap();
            }
        }
    }));
}

fn convert_image_to_bytes(image: &image::Image, gamma: f32, auto_minimum: bool) -> Vec<u8> {
    let timer = TimeLogger::start();

    let mut tt = InterpolTable::new();
    for i in 0..=10 {
        let x = (i as f32 / 10.0).powf(gamma);
        tt.add(x, x.powf(1.0/gamma));
    }
    tt.prepare();

    let (min, range) = if auto_minimum {
        let mut test_values: Vec<_> =
            image.l.as_slice().par_iter()
            .chain(image.r.as_slice().par_iter().step_by(42))
            .chain(image.g.as_slice().par_iter().step_by(42))
            .chain(image.b.as_slice().par_iter().step_by(42))
            .filter(|v| !v.is_infinite())
            .copied()
            .collect();

        let pos = test_values.len()/100;
        let min = test_values.select_nth_unstable_by(pos, cmp_f32).1.max(0.0);
        let range = 1.0/(1.0-min);
        (min, range)
    } else {
        (0.0, 1.0)
    };

    let bytes: Vec<u8> = if image.is_rgb() {
        image.r.as_slice().par_iter()
            .zip_eq(image.g.as_slice().par_iter())
            .zip_eq(image.b.as_slice().par_iter())
            .map(|((&r, &g), &b)| {
                let mut r = (tt.get((r-min)*range) * 255.0) as i32;
                if r < 0 { r = 0; }
                if r > 255 { r = 255; }
                let mut g = (tt.get((g-min)*range) * 255.0) as i32;
                if g < 0 { g = 0; }
                if g > 255 { g = 255; }
                let mut b = (tt.get((b-min)*range) * 255.0) as i32;
                if b < 0 { b = 0; }
                if b > 255 { b = 255; }
                [r as u8, g as u8, b as u8]
            })
            .flatten_iter()
            .collect()
    } else {
        image.l.as_slice().par_iter()
            .map(|l| {
                let mut l = (tt.get((l-min)*range) * 255.0) as i32;
                if l < 0 { l = 0; }
                if l > 255 { l = 255; }
                let l = l as u8;
                [l, l, l]
            })
            .flatten_iter()
            .collect()
    };

    timer.log("convert_image_to_bytes");

    bytes
}

fn show_preview_image(
    objects: &MainWindowObjectsPtr,
    img_bytes: Vec<u8>,
    img_width: i32,
    img_height: i32,
    scale: ImgScale
) {
    let bytes = glib::Bytes::from_owned(img_bytes);
    let mut pixbuf = gdk_pixbuf::Pixbuf::from_bytes(
        &bytes,
        gdk_pixbuf::Colorspace::Rgb,
        false,
        8,
        img_width,
        img_height,
        img_width * 3,
    );

    if scale == ImgScale::FitWindow {
        let parent = objects.preview_image
            .parent().unwrap() // gtk::EventBox
            .parent().unwrap(); // gtk::ScrolledWindows
        let width = parent.allocation().width();
        let height = parent.allocation().height();
        let img_ratio = img_width as f64 / img_height as f64;
        let gui_ratio = width as f64 / height as f64;
        let (new_width, new_height) = if img_ratio > gui_ratio {
            (width, (width as f64 / img_ratio) as i32)
        } else {
            ((height as f64 * img_ratio) as i32, height)
        };
        if new_width < 42 || new_height < 42 { return; }
        pixbuf = pixbuf.scale_simple(
            new_width, new_height,
            gdk_pixbuf::InterpType::Bilinear,
        ).unwrap();
    }

    objects.preview_image.set_pixbuf(Some(&pixbuf));
}

fn handler_project_tree_checked_changed(
    objects:  &MainWindowObjectsPtr,
    sorted_path: gtk::TreePath,
) {
    let sorted_model = objects.prj_tree
        .model().unwrap()
        .downcast::<gtk::TreeModelSort>().unwrap();
    let tree_store = sorted_model
        .model()
        .downcast::<gtk::TreeStore>().unwrap();

    let path = sorted_model.convert_path_to_child_path(&sorted_path).unwrap();

    let item = get_selection_for_path(&path);

    match item {
        SelectedItem {
            item_type: SelItemType::Group,
            group_idx: Some(group_idx),
            ..
        } => {
            let mut group = &mut objects.project.borrow_mut().groups[group_idx];
            group.used = !group.used;
            let iter = tree_store.iter(&path).unwrap();
            tree_store.set(&iter, &[(COLUMN_CHECKBOX,  &group.used)]);
        },

        SelectedItem {
            item_type: SelItemType::File,
            group_idx: Some(group_idx),
            file_type: Some(file_type),
            files,
            ..
        } => {
            let mut project = objects.project.borrow_mut();
            let file_list = project
                .groups[group_idx]
                .get_file_list_by_type_mut(file_type);
            let project_file = &mut file_list.list[files[0]];
            project_file.used = !project_file.used;
            let iter = tree_store.iter(&path).unwrap();
            tree_store.set(&iter, &[(COLUMN_CHECKBOX, &project_file.used)]);
        },
        _ => {
            return;
        },
    };

    update_project_name_and_time_in_gui(objects, true, true);
}

fn action_new_group(objects: &MainWindowObjectsPtr) {
    let def_group_options = GroupOptions::new();
    let dialog = group_options_dialog(
        objects,
        "Add new group",
        def_group_options,
        clone!(@strong objects => move |group_options| {
            let helper = TreeViewFillHelper::new(&objects.project.borrow());
            objects.project.borrow_mut().add_new_group(group_options);
            helper.apply_changes(&objects, true);
            log::info!("New group created");
        })
    );

    dialog.show();
}

fn action_delete_item(objects: &MainWindowObjectsPtr) {
    let selection = get_current_selection(objects);
    match selection {
        SelectedItem {
            item_type: SelItemType::Group,
            group_idx: Some(group_idx),
            ..
        } => {
            let group = &objects.project.borrow().groups[group_idx];
            let dialog = confirm_dialog(
                objects,
                format!("Remove group '{}' from project?", group.name(group_idx)),
                clone!(@strong objects => move || {
                    let helper = TreeViewFillHelper::new(&objects.project.borrow());
                    let group = objects.project.borrow_mut().remove_group(group_idx);
                    helper.apply_changes(&objects, true);
                    log::info!("Group '{}' removed from project", group.name(group_idx));
                })
            );
            dialog.show()
        },

        SelectedItem {
            item_type: SelItemType::File,
            group_idx: Some(group_idx),
            file_type: Some(file_type),
            mut files,
            ..
        } => {
            files.sort();
            let project = objects.project.borrow();
            let dialog_text = if files.len() == 1 {
                let group = &project.groups[group_idx];
                let folder = &group.get_file_list_by_type(file_type);
                let file = &folder.list[files[0]];
                format!("Remove file '{}' from project?", file.file_name.to_str().unwrap_or(""))
            } else {
                format!("Remove {} files from project?", files.len())
            };
            let dialog = confirm_dialog(
                objects,
                dialog_text,
                clone!(@strong objects => move || {
                    objects.prj_tree.selection().unselect_all();
                    let helper = TreeViewFillHelper::new(&objects.project.borrow());
                    for file_idx in files.iter().rev() {
                        let file = objects.project.borrow_mut()
                            .groups[group_idx]
                            .get_file_list_by_type_mut(file_type)
                            .list
                            .remove(*file_idx);
                        log::info!("File '{}' removed from project", file.file_name.to_str().unwrap_or(""));
                    }

                    objects.project.borrow_mut().changed = true;
                    helper.apply_changes(&objects, true);
                })
            );
            dialog.show()
        },

        _ => (),
    }
}

fn confirm_dialog<F: Fn() + 'static>(
    objects: &MainWindowObjectsPtr,
    text:    String,
    yes_fun: F
) -> gtk::MessageDialog {
    let dialog = gtk::MessageDialog::builder()
        .modal(true)
        .transient_for(&objects.window)
        .message_type(gtk::MessageType::Question)
        .buttons(gtk::ButtonsType::OkCancel)
        .title("Confirmation")
        .text(&text)
        .build();
    dialog.connect_response(clone!(@strong objects => move |dialog, response| {
        dialog.close();
        if response == gtk::ResponseType::Ok {
            yes_fun();
        }
    }));
    dialog
}

fn action_item_properties(objects: &MainWindowObjectsPtr) {
    let selection = get_current_selection(objects);

    match selection {
        SelectedItem {
            item_type: SelItemType::Group,
            group_idx: Some(group_idx),
            ..
        } => {
            let dialog = group_options_dialog(
                objects,
                "Group properties",
                objects.project.borrow().groups[group_idx].options.clone(),
                clone!(@strong objects => move |new_options| {
                    let helper = TreeViewFillHelper::new(&objects.project.borrow());
                    {
                        let mut project = objects.project.borrow_mut();
                        let group = &mut project.groups[group_idx];
                        log::info!("Group '{}' options changed to {:?}", group.name(group_idx), new_options);
                        group.options = new_options;
                        project.changed = true;
                    }
                    helper.apply_changes(&objects, false);
                })
            );
            dialog.show();
        },

        SelectedItem {item_type: SelItemType::Project, ..} => {
            action_project_options(objects);
        },

        _ => (),
    }
}

fn group_options_dialog<F: Fn(GroupOptions) + 'static>(
    objects:       &MainWindowObjectsPtr,
    title:         &str,
    group_options: GroupOptions,
    fun:           F
) -> gtk::Dialog {
    let builder = gtk::Builder::from_string(include_str!("ui/group_options_dialog.ui"));
    let dialog          = builder.object::<gtk::Dialog>("group_options_dialog").unwrap();
    let rb_default_name = builder.object::<gtk::RadioButton>("rb_default_name").unwrap();
    let rb_custom_name  = builder.object::<gtk::RadioButton>("rb_custom_name").unwrap();
    let e_name          = builder.object::<gtk::Entry>("e_name").unwrap();

    dialog.set_title(title);
    dialog.set_transient_for(Some(&objects.window));

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            ("Ok", gtk::ResponseType::Ok),
            ("Cancel", gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            ("Cancel", gtk::ResponseType::Cancel),
            ("Ok", gtk::ResponseType::Ok),
        ]);
    }

    rb_custom_name.connect_clicked(clone!(@strong e_name => move |rb| {
        e_name.set_sensitive(rb.is_active());
    }));

    if let Some(name) = &group_options.name {
        rb_custom_name.set_active(true);
        e_name.set_text(name);
        e_name.set_sensitive(true);
    } else {
        rb_default_name.set_active(true);
        e_name.set_sensitive(false);
    }

    dialog.connect_response(move |dialog, response| {
        dialog.close();
        if response == gtk::ResponseType::Ok {
            let mut group_options = group_options.clone();
            group_options.name = if rb_custom_name.is_active() {
                Some(e_name.text().to_string())
            } else {
                None
            };
            fun(group_options);
        }
    });

    dialog
}

fn get_active_group_index(objects: &MainWindowObjectsPtr, title: &str) -> Option<usize> {
    match objects.project.borrow().groups.len() {
        0 => return None,
        1 => return Some(0),
        _ => {},
    };

    let selection = get_current_selection(objects);
    let cur_group = selection.group_idx;

    let builder = gtk::Builder::from_string(include_str!("ui/select_group_dialog.ui"));
    let dialog = builder.object::<gtk::Dialog>("select_group_dialog").unwrap();
    let groups_list = builder.object::<gtk::ComboBoxText>("groups_list").unwrap();

    dialog.set_title(title);
    dialog.set_transient_for(Some(&objects.window));

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            ("_Ok", gtk::ResponseType::Ok),
            ("_Cancel", gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Ok", gtk::ResponseType::Ok),
        ]);
    }

    for (idx, group) in objects.project.borrow().groups.iter().enumerate() {
        groups_list.append(None, &group.name(idx));
    }

    if let Some(cur_group) = cur_group {
        groups_list.set_active(Some(cur_group as u32));
    }

    let resp = dialog.run();
    dialog.close();

    if resp != gtk::ResponseType::Ok {
        return None;
    }

    groups_list.active().map(|v| v as usize)
}

fn action_project_options(objects: &MainWindowObjectsPtr) {
    configure_project_options(
        objects,
        objects.project.borrow().config.clone(),
        clone!(@strong objects => move |new_config| {
            log::info!("New project options:\n{:#?}", new_config);
            objects.project.borrow_mut().set_new_config(new_config);
            update_project_name_and_time_in_gui(&objects, true, true);
        })
    );
}

fn configure_project_options<F: Fn(ProjectConfig) + 'static>(
    objects:        &MainWindowObjectsPtr,
    project_config: ProjectConfig,
    set_fun:        F,
) {
    let builder = gtk::Builder::from_string(include_str!("ui/project_options_dialog.ui"));
    let dialog = builder.object::<gtk::Dialog>("project_options_dialog").unwrap();
    let project_name = builder.object::<gtk::Entry>("project_name").unwrap();
    let img_size = builder.object::<gtk::ComboBoxText>("img_size").unwrap();
    let res_img_type = builder.object::<gtk::ComboBoxText>("res_img_type").unwrap();
    let lights_stack_mode = builder.object::<gtk::ComboBoxText>("lights_stack_mode").unwrap();
    let lights_stack_kappa = builder.object::<gtk::Entry>("lights_stack_kappa").unwrap();
    let lights_stack_steps = builder.object::<gtk::Entry>("lights_stack_steps").unwrap();
    let darks_stack_mode = builder.object::<gtk::ComboBoxText>("darks_stack_mode").unwrap();
    let darks_stack_kappa = builder.object::<gtk::Entry>("darks_stack_kappa").unwrap();
    let darks_stack_steps = builder.object::<gtk::Entry>("darks_stack_steps").unwrap();
    let flats_stack_mode = builder.object::<gtk::ComboBoxText>("flats_stack_mode").unwrap();
    let flats_stack_kappa = builder.object::<gtk::Entry>("flats_stack_kappa").unwrap();
    let flats_stack_steps = builder.object::<gtk::Entry>("flats_stack_steps").unwrap();
    let bias_stack_mode = builder.object::<gtk::ComboBoxText>("bias_stack_mode").unwrap();
    let bias_stack_kappa = builder.object::<gtk::Entry>("bias_stack_kappa").unwrap();
    let bias_stack_steps = builder.object::<gtk::Entry>("bias_stack_steps").unwrap();

    project_name.set_text(project_config.name.as_ref().unwrap_or(&String::new()).as_str());

    img_size.set_active(Some(match project_config.image_size {
        ImageSize::Original => 0,
        ImageSize::Bin2x2 => 1,
    }));

    res_img_type.set_active(Some(match project_config.res_img_type {
        ResFileType::Fit => 0,
        ResFileType::Tif => 1,
    }));

    let show_calc_opts = |opts: &CalcOpts, mode: &gtk::ComboBoxText, kappa: &gtk::Entry, steps: &gtk::Entry| {
        mode.append_text("Kappa-Sigma clipping");
        mode.append_text("Median");
        mode.append_text("Mean");

        mode.set_active(Some(match opts.mode {
            CalcMode::CappaSigma => 0,
            CalcMode::Median => 1,
            CalcMode::Mean => 2,
        }));
        kappa.set_text(&format!("{:.1}", opts.kappa));
        kappa.set_sensitive(opts.mode == CalcMode::CappaSigma);
        steps.set_text(&format!("{}", opts.repeats));
        steps.set_sensitive(opts.mode == CalcMode::CappaSigma);

        mode.connect_changed(clone!(@strong kappa, @strong steps => move |cb| {
            kappa.set_sensitive(cb.active() == Some(0));
            steps.set_sensitive(cb.active() == Some(0));
        }));
    };

    show_calc_opts(&project_config.light_calc_opts, &lights_stack_mode, &lights_stack_kappa, &lights_stack_steps);
    show_calc_opts(&project_config.dark_calc_opts, &darks_stack_mode, &darks_stack_kappa, &darks_stack_steps);
    show_calc_opts(&project_config.flat_calc_opts, &flats_stack_mode, &flats_stack_kappa, &flats_stack_steps);
    show_calc_opts(&project_config.bias_calc_opts, &bias_stack_mode, &bias_stack_kappa, &bias_stack_steps);

    dialog.set_title("Project options");
    dialog.set_transient_for(Some(&objects.window));

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            ("_Ok", gtk::ResponseType::Ok),
            ("_Cancel", gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Ok", gtk::ResponseType::Ok),
        ]);
    }

    dialog.connect_response(clone!(@strong objects => move |dialog, response| {
        if response == gtk::ResponseType::Ok {
            let mut project_config = project_config.clone();

            let get_calc_opts = |opts: &mut CalcOpts, mode: &gtk::ComboBoxText, kappa: &gtk::Entry, steps: &gtk::Entry| {
                opts.mode = match mode.active() {
                    Some(0) => CalcMode::CappaSigma,
                    Some(1) => CalcMode::Median,
                    Some(2) => CalcMode::Mean,
                    _ => panic!("Wrong mode.active(): {:?}", mode.active()),
                };
                opts.kappa = kappa.text().as_str().parse().unwrap_or(opts.kappa);
                opts.repeats = steps.text().as_str().parse().unwrap_or(opts.repeats);
            };

            get_calc_opts(&mut project_config.light_calc_opts, &lights_stack_mode, &lights_stack_kappa, &lights_stack_steps);
            get_calc_opts(&mut project_config.dark_calc_opts, &darks_stack_mode, &darks_stack_kappa, &darks_stack_steps);
            get_calc_opts(&mut project_config.flat_calc_opts, &flats_stack_mode, &flats_stack_kappa, &flats_stack_steps);
            get_calc_opts(&mut project_config.bias_calc_opts, &bias_stack_mode, &bias_stack_kappa, &bias_stack_steps);

            let name = project_name.text();
            project_config.name = if !name.is_empty() { Some(name.to_string()) } else { None };

            project_config.image_size = match img_size.active() {
                Some(0) => ImageSize::Original,
                Some(1) => ImageSize::Bin2x2,
                _ => panic!("Wrong img_size.active(): {:?}", img_size.active()),
            };

            project_config.res_img_type = match res_img_type.active() {
                Some(0) => ResFileType::Fit,
                Some(1) => ResFileType::Tif,
                _ => panic!("Wrong res_img_type.active(): {:?}", res_img_type.active()),
            };

            set_fun(project_config);
        }
        dialog.close();
    }));

    dialog.show();
}

fn action_cleanup_light_files(objects: &MainWindowObjectsPtr) {
    if !objects.project.borrow().can_exec_cleanup() {
        show_error_message("You have execute register light files first!", objects);
        return;
    }

    let builder = gtk::Builder::from_string(include_str!("ui/cleanup_dialog.ui"));
    let dialog = builder.object::<gtk::Dialog>("cleanup_dialog").unwrap();
    let chbt_check_before = builder.object::<gtk::CheckButton>("check_before").unwrap();
    let chbt_img_sharp = builder.object::<gtk::CheckButton>("chb_img_sharpness").unwrap();
    let e_img_sharp_kappa = builder.object::<gtk::Entry>("e_img_sharpness_kappa").unwrap();
    let e_img_sharp_repeats = builder.object::<gtk::Entry>("e_img_sharpness_repeats").unwrap();
    let chbt_stars_radius = builder.object::<gtk::CheckButton>("chb_stars_radius").unwrap();
    let e_stars_r_kappa = builder.object::<gtk::Entry>("e_stars_radius_kappa").unwrap();
    let e_stars_r_repeats = builder.object::<gtk::Entry>("e_stars_radius_repeats").unwrap();
    let chbt_noise = builder.object::<gtk::CheckButton>("chb_noise").unwrap();
    let e_noise = builder.object::<gtk::Entry>("e_noise").unwrap();
    let chbt_background = builder.object::<gtk::CheckButton>("chb_bg").unwrap();
    let e_background = builder.object::<gtk::Entry>("e_bg").unwrap();

    {
        let conf = &objects.project.borrow().cleanup_conf;
        chbt_check_before.set_active(conf.check_before_execute);
        let show_kappa = |
            item:      &ClenupConfItem,
            chb:       &gtk::CheckButton,
            e_kappa:   &gtk::Entry,
            e_repeats: &gtk::Entry
        |{
            chb.set_active(item.used);
            e_kappa.set_text(&format!("{:.1}", item.kappa));
            e_repeats.set_text(&format!("{}", item.repeats));
        };
        show_kappa(&conf.img_sharpness, &chbt_img_sharp, &e_img_sharp_kappa, &e_img_sharp_repeats);
        show_kappa(&conf.stars_radius, &chbt_stars_radius, &e_stars_r_kappa, &e_stars_r_repeats);

        let show_percent = |
            item:      &ClenupConfItem,
            chb:       &gtk::CheckButton,
            e_percent: &gtk::Entry
        |{
            chb.set_active(item.used);
            e_percent.set_text(&format!("{}", item.percent));
        };

        show_percent(&conf.noise, &chbt_noise, &e_noise);
        show_percent(&conf.background, &chbt_background, &e_background);
    }

    dialog.set_transient_for(Some(&objects.window));
    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            ("_Cleanup", gtk::ResponseType::Ok),
            ("_Close", gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            ("_Close", gtk::ResponseType::Cancel),
            ("_Cleanup", gtk::ResponseType::Ok),
        ]);
    }

    dialog.connect_response(clone!(@strong objects => move |dialog, response| {
        if response == gtk::ResponseType::Ok {
            {
                let conf = &mut objects.project.borrow_mut().cleanup_conf;
                conf.check_before_execute = chbt_check_before.is_active();
                let get_kappa = |
                    item:      &mut ClenupConfItem,
                    chb:       &gtk::CheckButton,
                    e_kappa:   &gtk::Entry,
                    e_repeats: &gtk::Entry
                |{
                    item.used = chb.is_active();
                    item.kappa = e_kappa.text().as_str().parse().unwrap_or(item.kappa);
                    item.repeats = e_repeats.text().as_str().parse().unwrap_or(item.repeats);
                };
                get_kappa(&mut conf.img_sharpness, &chbt_img_sharp, &e_img_sharp_kappa, &e_img_sharp_repeats);
                get_kappa(&mut conf.stars_radius, &chbt_stars_radius, &e_stars_r_kappa, &e_stars_r_repeats);

                let get_percent = |
                    item:      &mut ClenupConfItem,
                    chb:       &gtk::CheckButton,
                    e_percent: &gtk::Entry
                |{
                    item.used = chb.is_active();
                    item.percent = e_percent.text().as_str().parse().unwrap_or(item.percent);
                };

                get_percent(&mut conf.noise, &chbt_noise, &e_noise);
                get_percent(&mut conf.background, &chbt_background, &e_background);

                log::info!("Executing cleanup light files:\n{:#?}", conf);
            }

            let helper = TreeViewFillHelper::new(&objects.project.borrow());
            let result = objects.project.borrow_mut().cleanup_light_files();
            match result {
                Ok(cleaned_up_count) => {
                    show_message(
                        &objects,
                        "Cleanup light files result",
                        &format!("Cleaned up {} files", cleaned_up_count),
                        gtk::MessageType::Info,
                    );
                },
                Err(error) =>
                    show_error_message(&error.to_string(), &objects),
            }
            helper.apply_changes(&objects, true);
            update_project_name_and_time_in_gui(&objects, true, false);
        }
        dialog.close();
    }));

    dialog.show();
}

fn action_stack(objects: &MainWindowObjectsPtr) {
    match objects.project.borrow().can_exec_stack_light_files() {
        CanExecStackLightsRes::Ok => (),
        CanExecStackLightsRes::NoRefFile => {
            show_error_message(
                "Reference image is not defined",
                objects
            );
            return;
        },
    }

    log::info!("Stacking light files started");

    let project = objects.project.borrow().clone();
    let cpu_load = objects.config.borrow().cpu_load;
    exec_and_show_progress(
        objects,
        move|progress, cancel_flag| {
            project.stack_light_files(progress, cancel_flag, cpu_load)
        },
        move |objects, result| {
            preview_image_file(&objects, &result.file_name);
            show_message(
                objects,
                "Finished",
                &format!("Result file saved to {}", result.file_name.to_str().unwrap_or("")),
                gtk::MessageType::Info,
            )
        }
    );
}

fn action_use_as_reference_image(objects: &MainWindowObjectsPtr) {
    if let SelectedItem {
        item_type: SelItemType::File,
        file_type: Some(ProjectFileType::Light),
        group_idx: Some(group_idx),
        files,
    } = get_current_selection(objects) {
        if files.len() == 1 {
            let mut project = objects.project.borrow_mut();
            let file = &project.groups[group_idx].light_files.list[files[0]];
            project.ref_image = Some(file.file_name.clone());
        } else {
            return;
        }

        let helper = TreeViewFillHelper::new(&objects.project.borrow());
        helper.apply_changes(objects, true);
    }
}

fn exec_and_show_progress<R, ExecFun, OkFun> (
    objects:  &MainWindowObjectsPtr,
    exec_fun: ExecFun,
    ok_fun:   OkFun
) where
    R:       Sized + Send + 'static,
    ExecFun: Fn(&ProgressTs, &Arc<AtomicBool>) -> anyhow::Result<R> + Send + 'static,
    OkFun:   Fn(&MainWindowObjectsPtr, R) + 'static
{
    enum UiMessage<R: Sized> {
        ProgressStage{ text: String },
        ProgressPercent{ text: String, percent: usize },
        Finished(R),
        Error(String)
    }

    let (sender, receiver) = MainContext::channel(PRIORITY_DEFAULT);

    /* Worker */
    let cancel_flag = Arc::clone(&objects.cancel_flag);
    cancel_flag.store(false, Ordering::Relaxed);
    thread::spawn(move || {
        let sndr1 = sender.clone();
        let sndr2 = sender.clone();
        let progress = ProgressCallBack::new_ts(
            move |text: &str| {
                sndr1.send(UiMessage::ProgressStage {
                    text: text.to_string(),
                }).unwrap();
            },
            move |percent, text: &str| {
                sndr2.send(UiMessage::ProgressPercent {
                    text: text.to_string(),
                    percent
                }).unwrap();
            }
        );
        let result = exec_fun(&progress, &cancel_flag);
        match result {
            Ok(result) => sender.send(UiMessage::Finished(result)).unwrap(),
            Err(error) => sender.send(UiMessage::Error(error.to_string())).unwrap(),
        }
    });

    /* Ui events */
    enable_progress_bar(&objects, true);
    receiver.attach(
        None,
        clone!(@strong objects => move |message| {
            match message {
                UiMessage::ProgressStage{ text } => {
                    objects.progress_text.set_label(&text);
                    objects.progress_bar.set_fraction(0.0);
                    objects.progress_bar.set_text(None);
                    Continue(true)
                },
                UiMessage::ProgressPercent { text, percent } => {
                    let text = format!("{} {}%", text, percent);
                    objects.progress_bar.set_fraction(percent as f64 / 100.0);
                    objects.progress_bar.set_text(Some(&text));
                    Continue(true)
                },
                UiMessage::Finished(reg_info) => {
                    enable_progress_bar(&objects, false);
                    ok_fun(&objects, reg_info);
                    Continue(false)
                },
                UiMessage::Error(error) => {
                    enable_progress_bar(&objects, false);
                    show_error_message(&error, &objects);
                    Continue(false)
                },
            }
        }),
    );
}

fn action_change_file_to_light(objects: &MainWindowObjectsPtr) {
    change_selected_files_type(objects, ProjectFileType::Light);
}

fn action_change_file_to_dark(objects: &MainWindowObjectsPtr) {
    change_selected_files_type(objects, ProjectFileType::Dark);
}

fn action_change_file_to_flat(objects: &MainWindowObjectsPtr) {
    change_selected_files_type(objects, ProjectFileType::Flat);
}

fn action_change_file_to_bias(objects: &MainWindowObjectsPtr) {
    change_selected_files_type(objects, ProjectFileType::Bias);
}

fn change_selected_files_type(
    objects:  &MainWindowObjectsPtr,
    new_type: ProjectFileType
) {
    let selection = get_current_selection(objects);
    if selection.item_type != SelItemType::File {
        return;
    }

    let message = format!(
        "Change type of {0:} file(s) into {1:?}?",
        selection.files.len(),
        new_type
    );

    let dialog = gtk::MessageDialog::builder()
        .transient_for(&objects.window)
        .title("Change file types")
        .text(&message)
        .modal(true)
        .message_type(gtk::MessageType::Question)
        .build();

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            ("Yes", gtk::ResponseType::Yes),
            ("No", gtk::ResponseType::No),
        ]);
    } else {
        dialog.add_buttons(&[
            ("No", gtk::ResponseType::No),
            ("Yes", gtk::ResponseType::Yes),
        ]);
    }

    dialog.connect_response(clone!(@strong objects => move |dlg, resp| {
        if resp == gtk::ResponseType::Yes {
            let helper = TreeViewFillHelper::new(&objects.project.borrow());
            {
                let mut project = objects.project.borrow_mut();
                let group = &mut project.groups[selection.group_idx.unwrap()];
                group.change_file_types(
                    selection.file_type.unwrap(),
                    new_type,
                    selection.files.clone()
                );
                project.changed = true;
            }
            helper.apply_changes(&objects, true);
            update_project_name_and_time_in_gui(&objects, true, false);
        }
        dlg.close();
    }));

    dialog.show();
}

fn action_move_file_to_group(objects: &MainWindowObjectsPtr) {
    let selection = get_current_selection(objects);
    if selection.item_type != SelItemType::File {
        return;
    }

    let builder = gtk::Builder::from_string(include_str!("ui/move_files_to_group_dialog.ui"));
    let dialog = builder.object::<gtk::Dialog>("dialog").unwrap();
    let rbtn_existing_group = builder.object::<gtk::RadioButton>("rbtn_existing_group").unwrap();
    let rbtn_new_group = builder.object::<gtk::RadioButton>("rbtn_new_group").unwrap();
    let cbx_existing_groups = builder.object::<gtk::ComboBoxText>("cbx_existing_groups").unwrap();
    let e_new_group = builder.object::<gtk::Entry>("e_new_group").unwrap();

    dialog.set_transient_for(Some(&objects.window));
    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            ("_Ok", gtk::ResponseType::Ok),
            ("_Cancel", gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Ok", gtk::ResponseType::Ok),
        ]);
    }

    {
        let project = objects.project.borrow();
        for (idx, group) in project.groups.iter().enumerate() {
            if Some(idx) != selection.group_idx {
                cbx_existing_groups.append(Some(&group.uuid), &group.name(idx));
            }
        }
        let move_to_group_last_uuid = objects.move_to_group_last_uuid.borrow();
        if !move_to_group_last_uuid.is_empty() {
            cbx_existing_groups.set_active_id(Some(&*move_to_group_last_uuid));
        }

        if project.groups.len() <= 1 {
            rbtn_existing_group.set_sensitive(false);
            rbtn_new_group.set_active(true);
        } else {
            rbtn_existing_group.set_active(true);
        }
    }

    cbx_existing_groups.set_sensitive(rbtn_existing_group.is_active());
    e_new_group.set_sensitive(rbtn_new_group.is_active());

    rbtn_existing_group.connect_clicked(clone!(@strong cbx_existing_groups => move |rb| {
        cbx_existing_groups.set_sensitive(rb.is_active());
    }));

    rbtn_new_group.connect_clicked(clone!(@strong e_new_group => move |rb| {
        e_new_group.set_sensitive(rb.is_active());
    }));

    dialog.connect_response(clone!(@strong objects => move |dlg, resp| {
        if resp == gtk::ResponseType::Ok {
            let helper = TreeViewFillHelper::new(&objects.project.borrow());
            {
                let mut project = objects.project.borrow_mut();
                let group_id = if rbtn_new_group.is_active() {
                    let mut group_options = GroupOptions::new();
                    let new_group_name = e_new_group.text().to_string().trim().to_string();
                    if !new_group_name.is_empty() {
                        group_options.name = Some(new_group_name.to_string());
                    }
                    project.add_new_group(group_options);
                    project.groups.last().unwrap().uuid.clone()
                } else {
                    match cbx_existing_groups.active_id() {
                        Some(s) => s.to_string(),
                        _ => return,
                    }
                };

                let from_group = &mut project.groups[selection.group_idx.unwrap()];
                let from_folder = from_group.get_file_list_by_type_mut(selection.file_type.unwrap());
                let files_to_move = from_folder.remove_files_by_idx(selection.files.clone());

                let to_group = project.find_group_by_uuid_mut(&group_id).unwrap();
                let to_folder = to_group.get_file_list_by_type_mut(selection.file_type.unwrap());
                for file in files_to_move {
                    to_folder.list.push(file);
                }

                project.changed = true;
            }
            helper.apply_changes(&objects, true);
            update_project_name_and_time_in_gui(&objects, true, false);
        }
        dlg.close();
    }));

    dialog.show();
}

fn action_check_all_files(objects: &MainWindowObjectsPtr) {
    check_all_files(objects, true);
}

fn action_uncheck_all_files(objects: &MainWindowObjectsPtr) {
    check_all_files(objects, false);
}

fn check_all_files(objects: &MainWindowObjectsPtr, value: bool) {
    let s = get_current_selection(objects);
    let (group_idx, file_type) = match (s.group_idx, s.file_type) {
        (Some(group_idx), Some(file_type)) => (group_idx, file_type),
        _ => return,
    };

    let helper = TreeViewFillHelper::new(&objects.project.borrow());
    objects.project.borrow_mut()
        .groups[group_idx]
        .get_file_list_by_type_mut(file_type)
        .check_all(value);
    helper.apply_changes(&objects, true);
    update_project_name_and_time_in_gui(&objects, true, false);
}

fn action_check_selected_files(objects: &MainWindowObjectsPtr) {
    check_selected_files(objects, true);
}

fn action_uncheck_selected_files(objects: &MainWindowObjectsPtr) {
    check_selected_files(objects, false);
}

fn check_selected_files(objects: &MainWindowObjectsPtr, value: bool) {
    let s = get_current_selection(objects);
    let (group_idx, file_type, files) = match (s.group_idx, s.file_type, s.files) {
        (Some(group_idx), Some(file_type), files) if !files.is_empty() =>
            (group_idx, file_type, files),
        _ =>
            return,
    };

    let helper = TreeViewFillHelper::new(&objects.project.borrow());
    objects.project.borrow_mut()
        .groups[group_idx]
        .get_file_list_by_type_mut(file_type)
        .check_by_indices(&files, value);
    helper.apply_changes(&objects, true);
    update_project_name_and_time_in_gui(&objects, true, false);
}
