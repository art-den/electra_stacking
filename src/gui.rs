
use std::{rc::Rc, path::*, thread, collections::*, cell::*};
use std::sync::{*, atomic::{AtomicBool, Ordering}};
use gtk::{
    prelude::*,
    gio,
    gdk_pixbuf,
    glib::clone,
    glib,
};
use gettextrs::*;
use itertools::*;
use crate::{
    gtk_utils::*,
    image_io::*,
    image_raw::*,
    stacking_utils::*,
    light_file::*,
    calc::*,
    image,
    progress::*,
    config::*,
    project::*,
    str_utils::*,
};

pub fn build_ui(application: &gtk::Application) {
    let mut project = Project::default();
    let config = Config::default();

    project.make_default();

    // Icons

    let icons = gtk::IconTheme::default().unwrap();
    let icon_folder = icons.load_icon("folder", 16, gtk::IconLookupFlags::empty()).ok().flatten();
    let icon_image = icons.load_icon("image-x-generic", 16, gtk::IconLookupFlags::empty()).ok().flatten();
    let icon_photo = icons.load_icon("camera-photo-symbolic.symbolic", 16, gtk::IconLookupFlags::empty()).ok().flatten();
    let icon_ref_image = gdk_pixbuf::Pixbuf::from_read(include_bytes!(r"ui/key.png").as_slice()).ok();

    // Creating main window and getting some widgets

    let builder = gtk::Builder::from_string(include_str!(r"ui/main_window.ui"));
    let window              = builder.object::<gtk::ApplicationWindow>("main_window").unwrap();
    let prj_tree_menu       = builder.object::<gtk::Menu>("prj_tree_menu").unwrap();
    let project_tree        = builder.object::<gtk::TreeView>("project_tree").unwrap();
    let preview_img_scr     = builder.object::<gtk::ScrolledWindow>("preview_image_scrolled").unwrap();
    let preview_img_scale   = builder.object::<gtk::ComboBoxText>("preview_img_scale").unwrap();
    let preview_auto_min    = builder.object::<gtk::CheckButton>("preview_auto_min").unwrap();
    let preview_auto_wb     = builder.object::<gtk::CheckButton>("preview_auto_wb").unwrap();
    let preview_img_gamma   = builder.object::<gtk::Scale>("preview_img_gamma").unwrap();
    let recent_menu         = builder.object::<gtk::RecentChooserMenu>("recent_menu").unwrap();
    let mi_dark_theme       = builder.object::<gtk::RadioMenuItem>("dark_theme_mi").unwrap();
    let mi_light_theme      = builder.object::<gtk::RadioMenuItem>("light_theme_mi").unwrap();
    let mi_cpu_load_min     = builder.object::<gtk::RadioMenuItem>("mi_cpu_load_min").unwrap();
    let mi_cpu_load_half    = builder.object::<gtk::RadioMenuItem>("mi_cpu_load_half").unwrap();
    let mi_cpu_load_max     = builder.object::<gtk::RadioMenuItem>("mi_cpu_load_max").unwrap();
    let preview_image       = builder.object::<gtk::Image>("img_preview").unwrap();
    let preview_event_box   = builder.object::<gtk::EventBox>("eb_preview").unwrap();

    let preview_tp = rayon::ThreadPoolBuilder::new()
        .num_threads(2)
        .build()
        .unwrap();

    // Shared objects for main window

    let objects = Rc::new(MainWindow {
        project: RefCell::new(project),
        config: RefCell::new(config),
        builder,
        window: window.clone(),
        prj_tree: project_tree.clone(),
        prj_tree_is_building: Cell::new(false),
        icon_photo,
        icon_folder,
        icon_image,
        icon_ref_image,
        preview_image,
        last_preview_file: RefCell::new(PathBuf::new()),
        global_cancel_flag: Arc::new(AtomicBool::new(false)),
        preview_tp,
        prev_preview_cancel_flags: RefCell::new(None),
        prev_preview_img: RefCell::new(image::Image::new()),
        prev_preview_params: RefCell::new(image::ToRgbBytesParams::new()),
        preview_scroll_pos: RefCell::new(None),
        move_to_group_last_uuid: RefCell::new(String::new()),
        tasks_count: Cell::new(0),
        trying_to_close: Cell::new(false),
    });

    // Columns for project tree

    let prj_tree_store_columns = get_prj_tree_store_columns();

    for (col1, col2) in prj_tree_store_columns.iter().tuple_windows() {
        assert!(col1.2 as u32+1 == col2.2 as u32);
    }

    let cell_check = gtk::CellRendererToggle::builder()
        .activatable(true)
        .mode(gtk::CellRendererMode::Activatable)
        .sensitive(true)
        .build();

    for (col_name, _, idx, sidx, _) in prj_tree_store_columns {
        if col_name.is_empty() { continue; }
        let cell_text = gtk::CellRendererText::new();
        let col = gtk::TreeViewColumn::builder()
            .title(&gettext(col_name))
            .resizable(true)
            .clickable(true)
            .build();

        col.set_sort_column_id(sidx as i32);

        if idx == ColIdx::FileName {
            let cell_img = gtk::CellRendererPixbuf::new();
            TreeViewColumnExt::pack_start(&col, &cell_check, false);
            TreeViewColumnExt::pack_start(&col, &cell_img, false);
            TreeViewColumnExt::pack_start(&col, &cell_text, true);
            TreeViewColumnExt::add_attribute(&col, &cell_text, "markup", idx as i32);
            TreeViewColumnExt::add_attribute(&col, &cell_img, "pixbuf", ColIdx::Icon as i32);
            TreeViewColumnExt::add_attribute(&col, &cell_check, "active", ColIdx::Checkbox as i32);
            TreeViewColumnExt::add_attribute(&col, &cell_check, "visible", ColIdx::CheckboxVis as i32);
        } else {
            TreeViewColumnExt::pack_start(&col, &cell_text, true);
            TreeViewColumnExt::add_attribute(&col, &cell_text, "markup", idx as i32);
        }

        project_tree.append_column(&col);
    }

    // Drag-n-drop for files

    let targets = vec![
        gtk::TargetEntry::new("text/uri-list", gtk::TargetFlags::OTHER_APP, 0),
    ];

    window.drag_dest_set(gtk::DestDefaults::DROP, &targets, gtk::gdk::DragAction::COPY);
    window.drag_dest_set_track_motion(true);

    window.connect_drag_data_received(clone!(@strong objects => move |_w, _, _, _, sd, _, _| {
        handler_files_dropped(&objects, sd);
    }));

    // Load and apply config

    let res = objects.config.borrow_mut().load();
    if let Err(error) = res {
        show_error_message(&objects.window, &gettext("Error"), &error.to_string());
    }
    apply_config(&objects);

    // Font (only for MS Windows)

    if cfg!(target_os = "windows") {
        let settings = gtk::Settings::default().unwrap();
        settings.set_property("gtk-font-name", "Tahoma 9");
    }

    // Show empty project in tree

    update_project_tree(&objects);
    update_project_name_and_time_in_gui(&objects);

    // Actions + events

    connect_action(&window, &objects, "new_project",            action_new_project);
    connect_action(&window, &objects, "open_project",           action_open_project);
    connect_action(&window, &objects, "save_project_as",        action_save_project_as);
    connect_action(&window, &objects, "save_project",           action_save_project);
    connect_action(&window, &objects, "exit",                   action_exit);
    connect_action(&window, &objects, "add_light_files",        action_add_light_files);
    connect_action(&window, &objects, "add_dark_files",         action_add_dark_files);
    connect_action(&window, &objects, "add_flat_files",         action_add_flat_files);
    connect_action(&window, &objects, "add_bias_files",         action_add_bias_files);
    connect_action(&window, &objects, "new_group",              action_new_group);
    connect_action(&window, &objects, "delete_item",            action_delete_item);
    connect_action(&window, &objects, "use_as_ref_image",       action_use_as_reference_image);
    connect_action(&window, &objects, "item_properties",        action_item_properties);
    connect_action(&window, &objects, "register_light_files",   action_register);
    connect_action(&window, &objects, "stack_light_files",      action_stack);
    connect_action(&window, &objects, "project_options",        action_project_options);
    connect_action(&window, &objects, "light_theme",            action_light_theme);
    connect_action(&window, &objects, "dark_theme",             action_dark_theme);
    connect_action(&window, &objects, "cleanup_light_files",    action_cleanup_light_files);
    connect_action(&window, &objects, "change_file_to_light",   action_change_file_to_light);
    connect_action(&window, &objects, "change_file_to_dark",    action_change_file_to_dark);
    connect_action(&window, &objects, "change_file_to_flat",    action_change_file_to_flat);
    connect_action(&window, &objects, "change_file_to_bias",    action_change_file_to_bias);
    connect_action(&window, &objects, "move_file_to_group",     action_move_file_to_group);
    connect_action(&window, &objects, "check_all_files",        action_check_all_files);
    connect_action(&window, &objects, "uncheck_all_files",      action_uncheck_all_files);
    connect_action(&window, &objects, "check_selected_files",   action_check_selected_files);
    connect_action(&window, &objects, "uncheck_selected_files", action_uncheck_selected_files);
    connect_action(&window, &objects, "about",                  action_about);
    connect_action(&window, &objects, "project_columns",        action_project_columns);
    connect_action(&window, &objects, "assign_ref_light_image", action_assign_ref_light_image);

    enable_actions(&objects);

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
                return glib::Propagation::Stop;
            }
        }
        glib::Propagation::Proceed
    });

    objects.prj_tree.selection().connect_changed(clone!{ @weak objects => move |_| {
        if !objects.prj_tree_is_building.get() {
            enable_actions(&objects);
            preview_selected_file(&objects);
        }
    }});

    cell_check.connect_toggled(clone!(@weak objects => move |_, path| {
        if !objects.prj_tree_is_building.get() {
            handler_project_tree_checked_changed(&objects, path);
        }
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

    mi_cpu_load_min.connect_activate(clone!(@strong objects => move |mi| {
        if mi.is_active() {
            objects.config.borrow_mut().cpu_load = CpuLoad::OneThread;
        }
    }));

    mi_cpu_load_half.connect_activate(clone!(@strong objects => move |mi| {
        if mi.is_active() {
            objects.config.borrow_mut().cpu_load = CpuLoad::HalfCPUs;
        }
    }));

    mi_cpu_load_max.connect_activate(clone!(@strong objects => move |mi| {
        if mi.is_active() {
            objects.config.borrow_mut().cpu_load = CpuLoad::AllCPUs;
        }
    }));

    preview_img_scale.connect_changed(clone!(@strong objects => move |cb| {
        objects.config.borrow_mut().preview_scale = match cb.active() {
            Some(0) => ImgScale::Original,
            Some(1) => ImgScale::FitWindow,
            _       => return,
        };
        preview_image_after_change_view_opts(&objects, false);
    }));

    preview_auto_min.connect_clicked(clone!(@strong objects => move |chb| {
        objects.config.borrow_mut().preview_auto_min = chb.is_active();
        preview_image_after_change_view_opts(&objects, true);
    }));

    preview_auto_wb.connect_clicked(clone!(@strong objects => move |chb| {
        objects.config.borrow_mut().preview_auto_wb = chb.is_active();
        preview_image_after_change_view_opts(&objects, true);
    }));

    preview_img_gamma.connect_change_value(clone!(@strong objects => move |_, _, value| {
        objects.config.borrow_mut().preview_gamma = value as f32;
        preview_image_after_change_view_opts(&objects, false);
        glib::Propagation::Proceed
    }));

    objects.window.connect_delete_event(clone!(@strong objects => move |_, _| {
        if objects.trying_to_close.get() {
            return if objects.tasks_count.get() != 0 {
                glib::Propagation::Stop
            } else {
                glib::Propagation::Proceed
            };
        }

        let can_close = ask_user_to_save_project(&objects);

        if can_close && objects.tasks_count.get() != 0 {
            objects.trying_to_close.set(true);
            objects.global_cancel_flag.store(true, Ordering::Relaxed);
            return glib::Propagation::Stop;
        }

        return if can_close {
            glib::Propagation::Proceed
        } else {
            glib::Propagation::Stop
        };
    }));

    objects.window.connect_hide(clone!(@strong objects => move |_| {
        assign_config(&objects);
        let _ = objects.config.borrow().save();
    }));

    recent_menu.connect_item_activated(clone!(@strong objects => move |item| {
        let file_name = item
            .current_item()
            .and_then(|info| info.uri())
            .and_then(|uri| gio::File::for_uri(&uri).path());
        if let Some(file_name) = file_name {
            let can_open = ask_user_to_save_project(&objects);
            if can_open {
                open_project(&objects, &file_name);
            }
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
        glib::Propagation::Proceed
    }));

    preview_event_box.connect_button_release_event(clone!(@strong objects => move |_, evt| {
        if evt.button() == gtk::gdk::ffi::GDK_BUTTON_PRIMARY as u32 {
            *objects.preview_scroll_pos.borrow_mut() = None;
        }
        glib::Propagation::Proceed
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
        glib::Propagation::Proceed
    }));

    // Show main window

    window.set_application(Some(application));
    window.show_all();
}

struct MainWindow {
    project: RefCell<Project>,
    config: RefCell<Config>,
    builder: gtk::Builder,
    window: gtk::ApplicationWindow,
    prj_tree: gtk::TreeView,
    preview_image: gtk::Image,
    last_preview_file: RefCell<PathBuf>,
    preview_tp: rayon::ThreadPool,
    prev_preview_cancel_flags: RefCell<Option<Arc<AtomicBool>>>,
    prev_preview_img: RefCell<image::Image>,
    prev_preview_params: RefCell<image::ToRgbBytesParams>,
    preview_scroll_pos: RefCell<Option<((f64, f64), (f64, f64))>>,

    icon_folder: Option<gdk_pixbuf::Pixbuf>,
    icon_image: Option<gdk_pixbuf::Pixbuf>,
    icon_photo: Option<gdk_pixbuf::Pixbuf>,
    icon_ref_image: Option<gdk_pixbuf::Pixbuf>,

    global_cancel_flag: Arc<AtomicBool>,
    move_to_group_last_uuid: RefCell<String>,

    prj_tree_is_building: Cell<bool>,
    tasks_count: Cell<u32>,
    trying_to_close: Cell<bool>,
}

fn ask_user_to_save_project(objects: &Rc<MainWindow>) -> bool {
    if !objects.project.borrow().changed() { return true; }

    let dialog = gtk::MessageDialog::builder()
        .transient_for(&objects.window)
        .title(&gettext("Close application"))
        .text(&gettext("Project changes. Save?"))
        .modal(true)
        .message_type(gtk::MessageType::Question)
        .build();

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            (&gettext("_Yes"), gtk::ResponseType::Yes),
            (&gettext("_No"), gtk::ResponseType::No),
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
            (&gettext("_No"), gtk::ResponseType::No),
            (&gettext("_Yes"), gtk::ResponseType::Yes),
        ]);
    }

    let response = dialog.run();
    dialog.close();
    gtk::main_iteration_do(true);

    match response {
        gtk::ResponseType::Yes => {
            action_save_project(objects);
            objects.project.borrow_mut().reset_changed_flag();
            true
        },
        gtk::ResponseType::No => {
            objects.project.borrow_mut().reset_changed_flag();
            true
        },
        _ => {
            false
        },
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum ColIdx {
    None = -1,
    FileName = 0,
    FilePath,
    FileTime,
    Dim,
    Camera,
    IsoStr,
    ExpStr,
    FocLenStr,
    FNumber,
    NoiseStr,
    BgStr,
    StarsStr,
    FwhmStr,
    OvalityStr,
    Icon,
    Checkbox,
    CheckboxVis,
    Iso,
    Exp,
    FocLen,
    Noise,
    Bg,
    Stars,
    Fwhm,
    Ovality,
    Guid,
    ChangeCount,
    FileSort
}

fn get_prj_tree_store_columns() -> Vec::<(&'static str, &'static str, ColIdx, ColIdx, glib::Type)> {
    vec![
      // Column name in tree | ID       | Model column       | Sort model column | Model column type
        ("Project/File name", "filename", ColIdx::FileName,    ColIdx::FileSort,   String::static_type()),
        ("File path",         "path",     ColIdx::FilePath,    ColIdx::FilePath,   String::static_type()),
        ("File time",         "time",     ColIdx::FileTime,    ColIdx::FileTime,   String::static_type()),
        ("Dimensions",        "dims",     ColIdx::Dim,         ColIdx::Dim,        String::static_type()),
        ("Camera",            "camera",   ColIdx::Camera,      ColIdx::Camera,     String::static_type()),
        ("ISO/Gain",          "iso",      ColIdx::IsoStr,      ColIdx::Iso,        String::static_type()),
        ("Exposure",          "exp",      ColIdx::ExpStr,      ColIdx::Exp,        String::static_type()),
        ("Focal len",         "foclen",   ColIdx::FocLenStr,   ColIdx::FocLen,     String::static_type()),
        ("FNumber",           "fnumber",  ColIdx::FNumber,     ColIdx::FNumber,    String::static_type()),
        ("Noise",             "noise",    ColIdx::NoiseStr,    ColIdx::Noise,      String::static_type()),
        ("Background",        "bg",       ColIdx::BgStr,       ColIdx::Bg,         String::static_type()),
        ("Stars",             "stars",    ColIdx::StarsStr,    ColIdx::Stars,      String::static_type()),
        ("FWHM",              "fwhm",     ColIdx::FwhmStr,     ColIdx::Fwhm,       String::static_type()),
        ("Ovality",           "oval",     ColIdx::OvalityStr,  ColIdx::Ovality,    String::static_type()),

        // columns below are used for sorting, icons, checkboxes and lookup during tree building
        ("",                  "",         ColIdx::Icon,        ColIdx::None,       gdk_pixbuf::Pixbuf::static_type()),
        ("",                  "",         ColIdx::Checkbox,    ColIdx::None,       bool::static_type()),
        ("",                  "",         ColIdx::CheckboxVis, ColIdx::None,       bool::static_type()),
        ("",                  "",         ColIdx::Iso,         ColIdx::None,       u32::static_type()),
        ("",                  "",         ColIdx::Exp,         ColIdx::None,       f32::static_type()),
        ("",                  "",         ColIdx::FocLen,      ColIdx::None,       f32::static_type()),
        ("",                  "",         ColIdx::Noise,       ColIdx::None,       f32::static_type()),
        ("",                  "",         ColIdx::Bg,          ColIdx::None,       f32::static_type()),
        ("",                  "",         ColIdx::Stars,       ColIdx::None,       u32::static_type()),
        ("",                  "",         ColIdx::Fwhm,        ColIdx::None,       f32::static_type()),
        ("",                  "",         ColIdx::Ovality,     ColIdx::None,       f32::static_type()),
        ("",                  "",         ColIdx::Guid,        ColIdx::None,       String::static_type()),
        ("",                  "",         ColIdx::ChangeCount, ColIdx::None,       u32::static_type()),
        ("",                  "",         ColIdx::FileSort,    ColIdx::None,       String::static_type()),
    ]
}

fn update_project_tree(objects: &Rc<MainWindow>) {
    if objects.prj_tree_is_building.get() {
        return;
    }

    objects.prj_tree_is_building.set(true);

    let project = objects.project.borrow_mut();

    // project root
    let tree_store = match objects.prj_tree.model() {
        Some(model) => {
            let sorted_model = model.downcast::<gtk::TreeModelSort>().unwrap();
            sorted_model.model().downcast::<gtk::TreeStore>().unwrap()
        }
        None => {
            let col_types = get_prj_tree_store_columns()
                .iter()
                .map(|(_, _, _, _, tp)| *tp)
                .collect::<Vec<_>>();

            let result = gtk::TreeStore::new(&col_types);
            result.insert_with_values(None, None, &[
                (ColIdx::Icon as u32, &objects.icon_photo),
            ]);

            let sorted_model = gtk::TreeModelSort::new(&result);

            objects.prj_tree.set_model(Some(&sorted_model));
            result
        },
    };
    let project_iter = tree_store.iter_first().unwrap();

    let project_file_path = project.file_name()
        .as_ref()
        .and_then(|v|v.parent().and_then(|p| p.to_str()))
        .unwrap_or("");

    tree_store.set(&project_iter, &[
        (ColIdx::FileName as u32, &get_project_title(&project, true)),
        (ColIdx::FilePath as u32, &project_file_path),
    ]);

    // delete groups
    let mut tree_group_guids = HashSet::new();
    if let Some(group_iter) = tree_store.iter_children(Some(&project_iter)) {
        loop {
            let tree_group_guid = tree_store.value(&group_iter, ColIdx::Guid as i32).get::<String>().unwrap();
            let iter_valid = if !project.group_exists(&tree_group_guid) {
                tree_store.remove(&group_iter)
            } else {
                tree_group_guids.insert(tree_group_guid);
                tree_store.iter_next(&group_iter)
            };
            if !iter_valid { break; }
        }
    }

    // add groups
    for group in project.groups() {
        if tree_group_guids.contains(group.uuid()) { continue; }
        let iter = tree_store.insert_with_values(Some(&project_iter), None, &[
            (ColIdx::Icon        as u32, &objects.icon_folder),
            (ColIdx::Guid        as u32, &group.uuid()),
            (ColIdx::CheckboxVis as u32, &true),
        ]);
        for _ in 0..4 {
            tree_store.insert_with_values(
                Some(&iter), None,
                &[(ColIdx::Icon as u32, &objects.icon_folder)]
            );
        }

        objects.prj_tree.expand_to_path(&tree_store.path(&iter).unwrap());
    }

    // update groups
    let group_id_by_guid: HashMap<_,_> = project.groups()
        .into_iter()
        .enumerate()
        .map(|(i, g)| (g.uuid(), i))
        .collect();
    if let Some(group_iter) = tree_store.iter_children(Some(&project_iter)) {
        loop {
            let guid = tree_store.value(&group_iter, ColIdx::Guid as i32).get::<String>().unwrap();
            let group_index = *group_id_by_guid.get(guid.as_str()).unwrap();
            let group = &project.groups()[group_index];

            tree_store.set(&group_iter, &[
                (ColIdx::FileName as u32, &group.name(group_index)),
                (ColIdx::Checkbox as u32, &group.used()),
                (ColIdx::FileSort as u32, &""), // to disable sorting
            ]);

            let file_types = [
                (0, "Light files", &group.light_files, true),
                (1, "Dark files",  &group.dark_files,  true),
                (2, "Flat files",  &group.flat_files,  false),
                (3, "Bias files",  &group.bias_files,  false),
            ];

            for (i, caption, files, show_time) in file_types {
                let file_type_caption = gettext(caption);
                let files_iter = tree_store.iter_nth_child(Some(&group_iter), i).unwrap();

                // Files type node caption
                let folder_text = if files.list().is_empty() {
                    file_type_caption
                } else  {
                    let total_files = files.list().len();
                    let selected_files = files.get_checked_count();
                    let total_time = seconds_to_total_time_str(files.calc_total_exp_time(), false);

                    if show_time && selected_files == total_files {
                        format!(r#"{} <span alpha="50%">[{}] ({})</span>"#, file_type_caption, total_time, total_files)
                    } else if show_time && selected_files != total_files {
                        format!(r#"{} <span alpha="50%">[{}] ({}/{})</span>"#, file_type_caption, total_time, selected_files, total_files)
                    } else if !show_time && selected_files == total_files {
                        format!(r#"{} <span alpha="50%">({})</span>"#, file_type_caption, total_files)
                    } else {
                        format!(r#"{} <span alpha="50%">({}/{})</span>"#, file_type_caption, selected_files, total_files)
                    }
                };

                tree_store.set(&files_iter, &[
                    (ColIdx::FileName as u32, &folder_text),
                    (ColIdx::FileSort as u32, &""), // to disable sorting
                ]);

                let file_index_by_name: HashMap<_,_> = files.list().into_iter()
                    .enumerate()
                    .map(|(i, f)| (f.file_name().to_str().unwrap_or(""), i))
                    .collect();

                // delete files
                let mut tree_file_names = HashSet::new();
                if let Some(file_iter) = tree_store.iter_children(Some(&files_iter)) {
                    loop {
                        let file_name = tree_store.value(&file_iter, ColIdx::Guid as i32).get::<String>().unwrap();
                        let iter_valid = if file_index_by_name.get(file_name.as_str()).is_none() {
                            tree_store.remove(&file_iter)
                        } else {
                            tree_file_names.insert(file_name);
                            tree_store.iter_next(&file_iter)
                        };
                        if !iter_valid { break; }
                    }
                }

                // add files
                for file in files.list() {
                    let file_name = file.file_name().to_str().unwrap();
                    if tree_file_names.contains(file_name) { continue; }
                    tree_store.insert_with_values(Some(&files_iter), None, &[
                        (ColIdx::Guid        as u32, &file_name),
                        (ColIdx::CheckboxVis as u32, &true),
                        (ColIdx::ChangeCount as u32, &u32::MAX),
                    ]);
                }

                // update files
                if let Some(file_iter) = tree_store.iter_children(Some(&files_iter)) {
                    loop {
                        let file_name = tree_store
                            .value(&file_iter, ColIdx::Guid as i32)
                            .get::<String>()
                            .unwrap();
                        let file = &files.list()[*file_index_by_name.get(file_name.as_str()).unwrap()];
                        let tree_changes_count = tree_store
                            .value(&file_iter, ColIdx::ChangeCount as i32)
                            .get::<u32>()
                            .unwrap();
                        if file.change_count() != tree_changes_count {
                            let mut file_name = file
                                .file_name()
                                .file_name()
                                .and_then(|v| v.to_str())
                                .unwrap_or("Can't get file name")
                                .to_string();

                            if let Some(err_text) = file.get_error_test() {
                                file_name.push_str("/");
                                file_name.push_str(err_text);
                            }

                            let path = file
                                .file_name()
                                .parent()
                                .and_then(|v|v.to_str())
                                .unwrap_or("");

                            let dim_str = if let (Some(width), Some(height)) = (*file.width(), *file.height()) {
                                format!("{} x {}", width, height)
                            } else {
                                String::new()
                            };

                            let camera_str = if let Some(camera) = file.camera().as_ref() {
                                camera.as_str()
                            } else {
                                ""
                            };

                            let file_time_str = if let Some(file_time) = file.file_time() {
                                file_time.format("%Y-%m-%d %H:%M:%S").to_string()
                            } else {
                                String::new()
                            };

                            let (iso_str, iso) = if let Some(iso) = *file.iso() {
                                (format!("{}", iso), iso)
                            } else {
                                (String::new(), 0)
                            };

                            let (exp_str, exp) = match *file.exp() {
                                Some(exp) if exp > 0.5 =>
                                    (format!("{:.1}s", exp), exp),
                                Some(exp) if 0.0 < exp && exp < 0.5 =>
                                    (format!("1/{:.0}", 1.0/exp), exp),
                                _ =>
                                    (String::new(), 0.0)
                            };

                            let fnumber_str = match *file.fnumber() {
                                Some(fnumber) if fnumber != 0.0 =>
                                    format!("f/{:.1}", fnumber),
                                _ =>
                                    String::new(),
                            };

                            let (focal_len_str, focal_len) = match *file.focal_len() {
                                Some(focal_len) if focal_len != 0.0  =>
                                    (format!("{:.1}", focal_len), focal_len),
                                _ =>
                                    (String::new(), 0.0),
                            };

                            let (noise_str, noise, bg_str, bg, stars_cnt_str, stars_cnt,
                                fwhm_str, fwhm, star_r_dev_str, star_r_dev)
                                = if let Some(reg_info) = file.reg_info() {(
                                    format!("{:.3}%", reg_info.noise * 100.0),
                                    reg_info.noise,
                                    format!("{:.2}%", 100.0 * reg_info.background),
                                    reg_info.background,
                                    if reg_info.stars != 0 { format!("{}", reg_info.stars) } else { String::new() },
                                    reg_info.stars as u32,
                                    format!("{:.1}", reg_info.fwhm),
                                    reg_info.fwhm,
                                    format!("{:.3}", reg_info.stars_r_dev),
                                    reg_info.stars_r_dev,
                                )} else {(
                                    String::new(),
                                    0.0,
                                    String::new(),
                                    0.0,
                                    String::new(),
                                    0,
                                    String::new(),
                                    0.0,
                                    String::new(),
                                    0.0,
                                )};

                            let is_ref_file = Some(file.file_name()) == project.ref_image().as_ref();
                            let icon = if is_ref_file { &objects.icon_ref_image } else { &objects.icon_image };

                            let make_important = |s, flag: &FileFlags, mask: FileFlags| -> String {
                                if flag & mask == 0 {
                                    s
                                } else {
                                    format!(r##"<span color="#FF4040"><b>{}</b></span>"##, s)
                                }
                            };

                            let file_name = make_important(file_name, file.flags(), FILE_FLAG_ERROR);
                            let star_r_dev_str = make_important(star_r_dev_str, file.flags(), FILE_FLAG_CLEANUP_R_DEV);
                            let fwhm_str = make_important(fwhm_str, file.flags(), FILE_FLAG_CLEANUP_FWHM);
                            let stars_cnt_str = make_important(stars_cnt_str, file.flags(), FILE_FLAG_CLEANUP_STARS);
                            let noise_str = make_important(noise_str, file.flags(), FILE_FLAG_CLEANUP_NOISE);
                            let bg_str = make_important(bg_str, file.flags(), FILE_FLAG_CLEANUP_BG);

                            tree_store.set(&file_iter, &[
                                (ColIdx::Icon        as u32, icon),
                                (ColIdx::Checkbox    as u32, &file.used()),
                                (ColIdx::CheckboxVis as u32, &true),
                                (ColIdx::FileName    as u32, &file_name),
                                (ColIdx::FileSort    as u32, &file_name.to_lowercase()),
                                (ColIdx::FilePath    as u32, &path),
                                (ColIdx::FileTime    as u32, &file_time_str),
                                (ColIdx::Dim         as u32, &dim_str),
                                (ColIdx::Camera      as u32, &camera_str),
                                (ColIdx::IsoStr      as u32, &iso_str),
                                (ColIdx::Iso         as u32, &iso),
                                (ColIdx::ExpStr      as u32, &exp_str),
                                (ColIdx::Exp         as u32, &exp),
                                (ColIdx::FNumber     as u32, &fnumber_str),
                                (ColIdx::NoiseStr    as u32, &noise_str),
                                (ColIdx::Noise       as u32, &noise),
                                (ColIdx::BgStr       as u32, &bg_str),
                                (ColIdx::Bg          as u32, &bg),
                                (ColIdx::StarsStr    as u32, &stars_cnt_str),
                                (ColIdx::Stars       as u32, &stars_cnt),
                                (ColIdx::FwhmStr     as u32, &fwhm_str),
                                (ColIdx::Fwhm        as u32, &fwhm),
                                (ColIdx::OvalityStr  as u32, &star_r_dev_str),
                                (ColIdx::Ovality     as u32, &star_r_dev),
                                (ColIdx::FocLenStr   as u32, &focal_len_str),
                                (ColIdx::FocLen      as u32, &focal_len),
                                (ColIdx::ChangeCount as u32, &file.change_count())
                            ]);
                        }

                        if !tree_store.iter_next(&file_iter) { break; }
                    }
                }
            }

            if !tree_store.iter_next(&group_iter) { break; }
        }
    }

    objects.prj_tree_is_building.set(false);
}

fn update_project_name_and_time_in_gui(objects: &Rc<MainWindow>) {
    let app_descr_text = {
        let transl_descr = gettext("APP_DESCRIPTION");
        if transl_descr == "APP_DESCRIPTION" {
            env!("CARGO_PKG_DESCRIPTION").to_string()
        } else {
            transl_descr
        }
    };
    objects.window.set_title(&format!(
        "[{}] - Electra Stacking - {} v{}",
        get_project_title(&objects.project.borrow(), false),
        app_descr_text,
        env!("CARGO_PKG_VERSION"),
    ));
}

fn enable_actions(objects: &Rc<MainWindow>) {
    let mi_change_file_type = objects.builder.object::<gtk::MenuItem>("mi_change_file_type").unwrap();
    let mi_cpu_load_min     = objects.builder.object::<gtk::RadioMenuItem>("mi_cpu_load_min").unwrap();
    let mi_cpu_load_half    = objects.builder.object::<gtk::RadioMenuItem>("mi_cpu_load_half").unwrap();
    let mi_cpu_load_max     = objects.builder.object::<gtk::RadioMenuItem>("mi_cpu_load_max").unwrap();
    let recent_menu         = objects.builder.object::<gtk::RecentChooserMenu>("recent_menu").unwrap();


    let selection = get_current_selection(objects);
    let is_processing = objects.tasks_count.get() != 0;
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
    enable_action(&objects.window, "item_properties", support_item_properties && !is_processing);
    enable_action(&objects.window, "delete_item", support_delete && !is_processing);
    enable_action(&objects.window, "use_as_ref_image", support_use_as_ref_image && !is_processing);
    enable_action(&objects.window, "move_file_to_group", is_file && !is_processing);
    mi_change_file_type.set_sensitive(is_file);
    enable_action(
        &objects.window,
        "change_file_to_light",
        selection.file_type != Some(ProjectFileType::Light) && !is_processing
    );
    enable_action(
        &objects.window,
        "change_file_to_dark",
        selection.file_type != Some(ProjectFileType::Dark) && !is_processing
    );
    enable_action(
        &objects.window,
        "change_file_to_flat",
        selection.file_type != Some(ProjectFileType::Flat) && !is_processing
    );
    enable_action(
        &objects.window,
        "change_file_to_bias",
        selection.file_type != Some(ProjectFileType::Bias) && !is_processing
    );
    enable_action(&objects.window, "check_all_files", (is_file || is_file_type) && !is_processing);
    enable_action(&objects.window, "uncheck_all_files", (is_file || is_file_type) && !is_processing);
    enable_action(&objects.window, "check_selected_files", is_file && !is_processing);
    enable_action(&objects.window, "uncheck_selected_files", is_file && !is_processing);

    enable_action(&objects.window, "new_project", !is_processing);
    enable_action(&objects.window, "open_project", !is_processing);
    enable_action(&objects.window, "add_light_files", !is_processing);
    enable_action(&objects.window, "add_dark_files", !is_processing);
    enable_action(&objects.window, "add_flat_files", !is_processing);
    enable_action(&objects.window, "add_bias_files", !is_processing);
    enable_action(&objects.window, "new_group", !is_processing);
    enable_action(&objects.window, "register_light_files", !is_processing);
    enable_action(&objects.window, "stack_light_files", !is_processing);
    enable_action(&objects.window, "project_options", !is_processing);
    enable_action(&objects.window, "cleanup_light_files", !is_processing);

    recent_menu.set_sensitive(!is_processing);
    mi_cpu_load_min.set_sensitive(!is_processing);
    mi_cpu_load_half.set_sensitive(!is_processing);
    mi_cpu_load_max.set_sensitive(!is_processing);
}

fn create_file_filter_for_project() -> gtk::FileFilter {
    let ff = gtk::FileFilter::new();
    ff.set_name(Some("Electra stacking project"));
    ff.add_pattern("*.es_proj");
    ff
}

fn confirm_dialog<F: Fn() + 'static>(
    window:  &impl IsA<gtk::Window>,
    text:    String,
    yes_fun: F
) -> gtk::MessageDialog {
    let dialog = gtk::MessageDialog::builder()
        .modal(true)
        .transient_for(window)
        .message_type(gtk::MessageType::Question)
        .buttons(gtk::ButtonsType::OkCancel)
        .title(&gettext("Confirmation"))
        .text(&text)
        .build();
    dialog.connect_response(move |dialog, response| {
        dialog.close();
        if response == gtk::ResponseType::Ok {
            yes_fun();
        }
    });
    dialog
}

fn get_project_title(project: &Project, markup: bool) -> String {
    let mut result = String::new();

    if project.changed() {
        result.push_str("* ");
    }

    result +=
        project.config().name
        .clone()
        .unwrap_or_else(|| gettext("Unnamed project")).as_str();

    if markup {
        result = format!("<b>{}</b>", result);
    }

    if project.config().image_size == ImageSize::Bin2x2 {
        result.push_str(" - bin 2x2");
    }

    let time = project.calc_time();
    if time != 0.0 {
        result.push_str(&format!(
            " - {} total",
            seconds_to_total_time_str(time, false)
        ));
    }

    result
}

fn exec_and_show_progress<R, ExecFun, OkFun> (
    objects:  &Rc<MainWindow>,
    exec_fun: ExecFun,
    ok_fun:   OkFun
) where
    R:       Sized + Send + 'static,
    ExecFun: Fn(&ProgressTs, &IsCancelledFun) -> anyhow::Result<R> + Send + 'static,
    OkFun:   Fn(&Rc<MainWindow>, R) + 'static
{
    enum UiMessage<R: Sized> {
        ProgressStage{ text: String },
        ProgressPercent{ text: String, percent: usize },
        Finished(R),
        Error(String)
    }

    objects.tasks_count.set(objects.tasks_count.get() + 1);
    let cancel_flag = Arc::new(AtomicBool::new(false));

    let builder = gtk::Builder::from_string(include_str!("ui/progress_box.ui"));
    let progress_box = builder.object::<gtk::Grid>("progress_box").unwrap();
    let progress_text = builder.object::<gtk::Label>("progress_text").unwrap();
    let progress_bar = builder.object::<gtk::ProgressBar>("progress_bar").unwrap();
    let cancel_btn = builder.object::<gtk::Button>("cancel_btn").unwrap();

    let progress_container = objects.builder.object::<gtk::Box>("progress_container").unwrap();

    progress_container.add(&progress_box);

    cancel_btn.connect_clicked(clone!(@strong cancel_flag => move |_| {
        cancel_flag.store(true, Ordering::Relaxed);
    }));

    let global_cancel_flag = Arc::clone(&objects.global_cancel_flag);
    let is_cancelled_fun = move || {
        cancel_flag.load(Ordering::Relaxed) || global_cancel_flag.load(Ordering::Relaxed)
    };

    /* Worker */

    let (sender, receiver) = async_channel::unbounded();
    thread::spawn(move || {
        let sndr1 = sender.clone();
        let sndr2 = sender.clone();
        let progress = ProgressCallBack::new_ts(
            move |text: &str| {
                sndr1.send_blocking(UiMessage::ProgressStage {
                    text: text.to_string(),
                }).unwrap();
            },
            move |percent, text: &str| {
                sndr2.send_blocking(UiMessage::ProgressPercent {
                    text: text.to_string(),
                    percent
                }).unwrap();
            }
        );
        let result = exec_fun(&progress, &(Arc::new(is_cancelled_fun) as _));
        match result {
            Ok(result) => sender.send_blocking(UiMessage::Finished(result)).unwrap(),
            Err(error) => sender.send_blocking(UiMessage::Error(error.to_string())).unwrap(),
        }
    });

    /* Ui events */

    enable_actions(objects);
    glib::spawn_future_local(clone!(@strong objects => async move {
        while let Ok(message) = receiver.recv().await {
            match message {
                UiMessage::ProgressStage{ text } => {
                    progress_text.set_label(&text);
                    progress_bar.set_fraction(0.0);
                    progress_bar.set_text(None);
                },
                UiMessage::ProgressPercent { text, percent } => {
                    let text = format!("{} {}%", text, percent);
                    progress_bar.set_fraction(percent as f64 / 100.0);
                    progress_bar.set_text(Some(&text));
                },
                UiMessage::Finished(reg_info) => {
                    objects.tasks_count.set(objects.tasks_count.get() - 1);
                    if objects.trying_to_close.get() {
                        objects.window.close();
                    } else {
                        enable_actions(&objects);
                        ok_fun(&objects, reg_info);
                    }
                    progress_container.remove(&progress_box);
                    break;
                },
                UiMessage::Error(error) => {
                    objects.tasks_count.set(objects.tasks_count.get() - 1);
                    if objects.trying_to_close.get() {
                        objects.window.close();
                    } else {
                        enable_actions(&objects);
                        show_error_message(&objects.window, &gettext("Error"), &error);
                    }
                    progress_container.remove(&progress_box);
                    break;
                },
            }

        }
    }));
}

fn handler_project_tree_checked_changed(
    objects:     &Rc<MainWindow>,
    sorted_path: gtk::TreePath
) {
    let sorted_model = objects.prj_tree
        .model().unwrap()
        .downcast::<gtk::TreeModelSort>().unwrap();

    let path = sorted_model
        .convert_path_to_child_path(&sorted_path)
        .unwrap();

    let item = get_selection_for_path(&path);

    match item {
        SelectedItem {
            item_type: SelItemType::Group,
            group_idx: Some(group_idx),
            ..
        } => {
            let mut project = objects.project.borrow_mut();
            let group = project.group_by_index_mut(group_idx);
            group.set_used(!group.used());
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
                .group_by_index_mut(group_idx)
                .file_list_by_type_mut(file_type);
            let project_file = &mut file_list.file_by_index_mut(files[0]);
            project_file.set_used(!project_file.used());
        },
        _ => {
            return;
        },
    };

    update_project_tree(objects);
    update_project_name_and_time_in_gui(objects);
}

// widgets -> config
fn assign_config(objects: &Rc<MainWindow>) {
    let prj_img_paned  = objects.builder.object::<gtk::Paned>("prj_img_paned").unwrap();
    let mi_dark_theme  = objects.builder.object::<gtk::RadioMenuItem>("dark_theme_mi").unwrap();
    let mi_light_theme = objects.builder.object::<gtk::RadioMenuItem>("light_theme_mi").unwrap();

    let mut config = objects.config.borrow_mut();

    config.prj_tree_width = prj_img_paned.position();

    if mi_dark_theme.is_active() {
        config.theme = Theme::Dark;
    } else if mi_light_theme.is_active() {
        config.theme = Theme::Light;
    }

    let tree_columns = get_prj_tree_store_columns();

    config.prj_cols.clear();
    for (idx, col) in objects.prj_tree.columns().iter().enumerate() {
        config.prj_cols.insert(
            tree_columns[idx].1.to_string(),
            PrjTreeCol {
                width: col.width(),
                visible: col.is_visible(),
                pos: -1,
            }
        );
    }

    let (width, height) = objects.window.size();
    config.main_win_width = width;
    config.main_win_height = height;
    config.main_win_maximized = objects.window.is_maximized();
}

// config -> widgets
fn apply_config(objects: &Rc<MainWindow>) {
    let preview_img_gamma = objects.builder.object::<gtk::Scale>("preview_img_gamma").unwrap();
    let prj_img_paned     = objects.builder.object::<gtk::Paned>("prj_img_paned").unwrap();
    let preview_img_scale = objects.builder.object::<gtk::ComboBoxText>("preview_img_scale").unwrap();
    let mi_dark_theme     = objects.builder.object::<gtk::RadioMenuItem>("dark_theme_mi").unwrap();
    let mi_light_theme    = objects.builder.object::<gtk::RadioMenuItem>("light_theme_mi").unwrap();
    let mi_cpu_load_min   = objects.builder.object::<gtk::RadioMenuItem>("mi_cpu_load_min").unwrap();
    let mi_cpu_load_half  = objects.builder.object::<gtk::RadioMenuItem>("mi_cpu_load_half").unwrap();
    let mi_cpu_load_max   = objects.builder.object::<gtk::RadioMenuItem>("mi_cpu_load_max").unwrap();
    let preview_auto_min  = objects.builder.object::<gtk::CheckButton>("preview_auto_min").unwrap();
    let preview_auto_wb   = objects.builder.object::<gtk::CheckButton>("preview_auto_wb").unwrap();


    let config = objects.config.borrow();

    match config.theme {
        Theme::Dark => {
            action_dark_theme(objects);
            mi_dark_theme.set_active(true);
        },
        Theme::Light => {
            action_light_theme(objects);
            mi_light_theme.set_active(true);
        },
        Theme::Other(_) => (),
    }

    match config.cpu_load {
        CpuLoad::OneThread =>
            mi_cpu_load_min.set_active(true),
        CpuLoad::HalfCPUs =>
            mi_cpu_load_half.set_active(true),
        CpuLoad::AllCPUs =>
            mi_cpu_load_max.set_active(true),
        _ => {},
    };

    if config.prj_tree_width != -1 {
        prj_img_paned.set_position(config.prj_tree_width);
    }

    let tree_columns = get_prj_tree_store_columns();
    for i in 0..objects.prj_tree.n_columns() {
        let tree_col = objects.prj_tree.column(i as i32).unwrap();
        let id = tree_columns[i as usize].1;
        if let Some(col_conf) = config.prj_cols.get(id) {
            if col_conf.width > 0 {
                tree_col.set_fixed_width(col_conf.width);
            }
            tree_col.set_visible(col_conf.visible);
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
            preview_img_scale.set_active(Some(0)),
        ImgScale::FitWindow =>
            preview_img_scale.set_active(Some(1)),
    }

    preview_auto_min.set_active(config.preview_auto_min);
    preview_auto_wb.set_active(config.preview_auto_wb);

    preview_img_gamma.set_value(config.preview_gamma as f64);
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

fn get_current_selection(objects: &Rc<MainWindow>) -> SelectedItem {
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

fn action_new_project(objects: &Rc<MainWindow>) {
    let can_create_new_project = ask_user_to_save_project(objects);
    if !can_create_new_project { return; }
    let mut new_project = Project::default();
    new_project.make_default();
    *objects.project.borrow_mut() = new_project;
    update_project_tree(objects);
    log::info!("New project created");
}

fn action_open_project(objects: &Rc<MainWindow>) {
    let can_opened = ask_user_to_save_project(objects);
    if !can_opened { return; }

    let ff = create_file_filter_for_project();
    let fc = gtk::FileChooserDialog::builder()
        .action(gtk::FileChooserAction::Open)
        .title(&gettext("Select project file to open"))
        .filter(&ff)
        .modal(true)
        .transient_for(&objects.window)
        .build();

    fc.set_current_folder(objects.config.borrow().last_path.clone());

    add_ok_and_cancel_buttons(
        fc.upcast_ref(),
        &gettext("_Open"), gtk::ResponseType::Accept,
        &gettext("_Cancel"), gtk::ResponseType::Cancel
    );

    fc.connect_response(clone!(@strong objects => move |file_chooser, response| {
        if response == gtk::ResponseType::Accept {
            let file_name = file_chooser.file().expect("Can't get file_name");
            let path = file_name.path().unwrap();

            if let Some(cur_folder) = file_chooser.current_folder() {
                objects.config.borrow_mut().last_path = cur_folder;
            }

            open_project(&objects, &path);
        }
        file_chooser.close();
    }));

    fc.show();
}

fn open_project(objects: &Rc<MainWindow>, path: &Path) {
    let res = objects.project.borrow_mut().load(path);
    if let Err(err) = res {
        show_error_message(&objects.window, &gettext("Error"), &err.to_string());
        log::error!("'{}' during opening project", err.to_string());
    } else {
        update_project_tree(objects);
        update_project_name_and_time_in_gui(objects);
        log::info!("Project {} opened", path.to_str().unwrap_or(""));
    }
}

fn action_save_project(objects: &Rc<MainWindow>) {
    let file_name = objects.project.borrow().file_name().clone();
    if let Some(file_name) = file_name {
        save_project(objects, &file_name);
    } else {
        action_save_project_as(objects);
    }
}

fn action_save_project_as(objects: &Rc<MainWindow>) {
    let ff = create_file_filter_for_project();
    let fc = gtk::FileChooserDialog::builder()
        .action(gtk::FileChooserAction::Save)
        .title(&gettext("Select project file to save"))
        .filter(&ff)
        .modal(true)
        .transient_for(&objects.window)
        .build();

    add_ok_and_cancel_buttons(
        fc.upcast_ref(),
        &gettext("_Save"), gtk::ResponseType::Accept,
        &gettext("_Cancel"), gtk::ResponseType::Cancel
    );


    if let Some(file_name) = objects.project.borrow().file_name() {
        let _ = fc.set_file(&gio::File::for_path(file_name));
    } else {
        fc.set_current_folder(objects.config.borrow().last_path.clone());
    }

    let resp = fc.run();
    fc.close();

    if resp == gtk::ResponseType::Accept {
        let file_name = fc.file().expect("Can't get file_name");
        let path = file_name
            .path()
            .unwrap()
            .with_extension("es_proj");
        let project_name = path.with_extension("")
            .file_name()
            .and_then(|v| v.to_str())
            .unwrap_or("")
            .to_string();

        if let Some(cur_folder) = fc.current_folder() {
            objects.config.borrow_mut().last_path = cur_folder;
        }

        let mut new_conf = objects.project.borrow().config().clone();
        new_conf.name = Some(project_name);
        objects.project.borrow_mut().set_config(new_conf);

        let ok = save_project(objects, &path);
        if !ok { return; }

        update_project_tree(objects);
        update_project_name_and_time_in_gui(objects);
    }
}

fn save_project(objects: &Rc<MainWindow>, file_name: &Path) -> bool {
    let res = objects.project.borrow_mut().save(file_name);
    if let Err(err) = res {
        show_error_message(&objects.window, &gettext("Error"), &err.to_string());
        log::error!("'{}' during saving project", err.to_string());
        false
    } else {
        log::info!("Project {} saved", file_name.to_str().unwrap_or(""));
        update_project_tree(objects);
        update_project_name_and_time_in_gui(objects);
        true
    }
}

fn action_exit(objects: &Rc<MainWindow>) {
    log::info!("Exit called");
    objects.window.close();
}

fn action_add_light_files(objects: &Rc<MainWindow>) {
    select_and_add_files_into_project(
        objects,
        ProjectFileType::Light,
        gettext("Select LIGHT files"),
        gettext("Add light files"),
    );
}

fn action_add_dark_files(objects: &Rc<MainWindow>) {
    select_and_add_files_into_project(
        objects,
        ProjectFileType::Dark,
        gettext("Select DARK files"),
        gettext("Add dark files"),
    );
}

fn action_add_flat_files(objects: &Rc<MainWindow>) {
    select_and_add_files_into_project(
        objects,
        ProjectFileType::Flat,
        gettext("Select FLAT files"),
        gettext("Add flat files"),
    );
}

fn action_add_bias_files(objects: &Rc<MainWindow>) {
    select_and_add_files_into_project(
        objects,
        ProjectFileType::Bias,
        gettext("Select BIAS files"),
        gettext("Add bias files"),
    );
}

fn select_and_add_files_into_project(
    objects:          &Rc<MainWindow>,
    file_type:        ProjectFileType,
    files_dialog_cap: String,
    select_group_cap: String,
) {
    let fc = create_src_file_select_dialog(
        files_dialog_cap,
        objects,
        file_type == ProjectFileType::Light
    );
    fc.connect_response(clone!(@strong objects => move |file_chooser, response| {
        if response == gtk::ResponseType::Accept {
            let group_index = get_active_group_index(&objects, &select_group_cap).
                or_else(|| {
                    if objects.project.borrow().groups().is_empty() {
                        Some(0)
                    } else {
                        None
                    }
                });

            if let Some(group_index) = group_index {
                let user_selected_files = file_chooser.files().iter()
                    .filter_map(|f| f.path())
                    .collect();

                log::info!("Dialog '{}' confirmed", select_group_cap);
                add_files_into_project(
                    &user_selected_files,
                    &objects,
                    group_index,
                    file_type
                );
            }
            if let Some(cur_folder) = file_chooser.current_folder() {
                objects.config.borrow_mut().last_path = cur_folder;
            }
        }
        file_chooser.close();
    }));
    fc.show();

    fn create_src_file_select_dialog(
        title:       String,
        objects:     &Rc<MainWindow>,
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
        add_exts(FIT_EXTS);
        if light_files {
            add_exts(TIF_EXTS);
        }
        let fc = gtk::FileChooserDialog::builder()
            .action(gtk::FileChooserAction::Open)
            .title(&title)
            .filter(&ff)
            .modal(true)
            .transient_for(&objects.window)
            .select_multiple(true)
            .build();

        fc.set_current_folder(objects.config.borrow().last_path.clone());

        add_ok_and_cancel_buttons(
            fc.upcast_ref(),
            &gettext("_Open"), gtk::ResponseType::Accept,
            &gettext("_Cancel"), gtk::ResponseType::Cancel
        );

        fc
    }
}

fn handler_files_dropped(objects: &Rc<MainWindow>, sd: &gtk::SelectionData) {
    fn add_dir_to_list(file_name: PathBuf, files_list: &mut Vec<PathBuf>) {
        let paths = std::fs::read_dir(file_name);
        if let Ok(paths) = paths {
            for entry in paths.filter_map(|e| e.ok()) {
                add_file_to_list(entry.path(), files_list);
            }
        }
    }

    fn add_file_to_list(file_name: PathBuf, files_list: &mut Vec<PathBuf>) {
        if file_name.is_file() && is_source_file_name(&file_name) {
            files_list.push(file_name);
        } else if file_name.is_dir() {
            add_dir_to_list(file_name, files_list);
        }
    }

    fn ask_user_to_select_types_and_add_files(objects: &Rc<MainWindow>, files: Vec<PathBuf>) {
        if files.is_empty() { return; }
        let builder = gtk::Builder::from_string(include_str!("ui/dnd_files_type_dialog.ui"));
        let dialog = builder.object::<gtk::Dialog>("dnd_files_type_dialog").unwrap();
        let l_info = builder.object::<gtk::Label>("l_info").unwrap();
        let cb_group = builder.object::<gtk::ComboBoxText>("cb_group").unwrap();

        let rb_lights = builder.object::<gtk::RadioButton>("rb_lights").unwrap();
        let rb_darks = builder.object::<gtk::RadioButton>("rb_darks").unwrap();
        let rb_flats = builder.object::<gtk::RadioButton>("rb_flats").unwrap();

        l_info.set_label(&transl_and_replace(
            "Add {files} file(s) into project?",
            &[("{files}", files.len().to_string())]
        ));

        let project = objects.project.borrow();
        for (idx, group) in project.groups().iter().enumerate() {
            cb_group.append(None, group.name(idx).as_str());
        }
        cb_group.set_sensitive(project.groups().len() > 1);
        let selection = get_current_selection(objects);
        cb_group.set_active(Some(selection.group_idx.unwrap_or(0) as u32));

        dialog.set_transient_for(Some(&objects.window));
        if cfg!(target_os = "windows") {
            dialog.add_buttons(&[
                (&gettext("_Add"), gtk::ResponseType::Ok),
                (&gettext("_Cancel"), gtk::ResponseType::Cancel),
            ]);
        } else {
            dialog.add_buttons(&[
                (&gettext("_Cancel"), gtk::ResponseType::Cancel),
                (&gettext("_Add"), gtk::ResponseType::Ok),
            ]);
        }

        dialog.connect_response(clone!(@strong objects => move |dlg, resp| {
            if resp == gtk::ResponseType::Ok {
                let file_type = if rb_lights.is_active() {
                    ProjectFileType::Light
                } else if rb_darks.is_active() {
                    ProjectFileType::Dark
                } else if rb_flats.is_active() {
                    ProjectFileType::Flat
                } else {
                    ProjectFileType::Bias
                };
                add_files_into_project(
                    &files,
                    &objects,
                    cb_group.active().unwrap_or(0_u32) as usize,
                    file_type
                );
            }
            dlg.close();
        }));

        set_dialog_default_button(&dialog);
        dialog.show();
    }

    let mut files_list = Vec::new();
    for file in sd.uris() {
        let file_name = gio::File::for_uri(&file).path();
        if let Some(file_name) = file_name {
            add_file_to_list(file_name, &mut files_list);
        }
    }
    ask_user_to_select_types_and_add_files(objects, files_list);
}

fn add_files_into_project(
    file_names:       &Vec<PathBuf>,
    objects:          &Rc<MainWindow>,
    group_iter_index: usize,
    file_type:        ProjectFileType
) {
    let mut file_names = file_names.clone();
    let project = objects.project.borrow();
    let group = project.groups().get(group_iter_index);
    if let Some(group) = group {
        let files = &group.get_file_list_by_type(file_type);
        files.retain_files_if_they_are_not_here(&mut file_names);
    }
    drop(project);

    exec_and_show_progress(
        objects,
        move |progress, cancel_flag| {
            load_src_file_info_for_files(&file_names, cancel_flag, progress)
        },
        move |objects, result| {
            let mut project = objects.project.borrow_mut();
            project.add_default_group_if_empty();
            let group = project.group_by_index_mut(group_iter_index);
            let files = &mut group.file_list_by_type_mut(file_type);
            log::info!("Added {} files", result.len());
            files.add_files_from_src_file_info(result);
            drop(project);
            update_project_tree(objects);
            update_project_name_and_time_in_gui(objects);
        }
    );
}


fn action_register(objects: &Rc<MainWindow>) {
    if objects.project.borrow().total_light_files_count() == 0 {
        return;
    }
    log::info!("Registering light files started");
    let project_json = objects.project.borrow().to_json_string();
    let cpu_load = objects.config.borrow().cpu_load;
    exec_and_show_progress(
        objects,
        move |progress, is_canceled| {
            let project = Project::from_json_string(&project_json);
            project.register_light_files(progress, is_canceled, cpu_load)
        },
        move |objects, result| {
            objects.project.borrow_mut().update_light_files_reg_info(result);
            update_project_tree(objects);
            update_project_name_and_time_in_gui(objects);
        }
    );
}

fn action_light_theme(_: &Rc<MainWindow>) {
    let settings = gtk::Settings::default().unwrap();
    settings.set_property("gtk-application-prefer-dark-theme", false);
    log::info!("Light theme selected");
}

fn action_dark_theme(_: &Rc<MainWindow>) {
    let settings = gtk::Settings::default().unwrap();
    settings.set_property("gtk-application-prefer-dark-theme", true);
    log::info!("Dark theme selected");
}

fn preview_selected_file(objects: &Rc<MainWindow>) {
    let (file_name, mode) = {
        match get_current_selection(objects) {
            SelectedItem {
                item_type: SelItemType::File,
                group_idx: Some(group_idx),
                file_type: Some(file_type),
                files,
                ..
            } if !files.is_empty() => {
                let project = objects.project.borrow();
                let file_list = project.groups()[group_idx].get_file_list_by_type(file_type);
                let project_file = &file_list.list()[files[0]];

                let mode = match file_type {
                    ProjectFileType::Light =>
                        PreviewFileMode::LightFile,
                    ProjectFileType::Dark |
                    ProjectFileType::Bias =>
                        PreviewFileMode::BWCalibr,
                    ProjectFileType::Flat =>
                    PreviewFileMode::FlatFile,
                };

                (project_file.file_name().clone(), mode)
            },

            SelectedItem {
                item_type: SelItemType::Project,
                ..
            } => {
                let project = objects.project.borrow();
                match project.get_result_file_name() {
                    Ok(file_name) => (file_name, PreviewFileMode::ResultFile),
                    Err(_) => return,
                }
            },

            _ => return,
        }
    };

    if *objects.last_preview_file.borrow() == file_name {
        return;
    }

    preview_image_file(objects, &file_name, mode);
}

fn preview_image_after_change_view_opts(
    objects: &Rc<MainWindow>,
    recalc_params: bool
) {
    let image = objects.prev_preview_img.borrow();
    if image.is_empty() { return; }
    let config = objects.config.borrow();
    if recalc_params {
        *objects.prev_preview_params.borrow_mut() =
            image.calc_to_bytes_params(
                config.preview_auto_min,
                config.preview_auto_wb
            );
    }
    let bytes = image.to_rgb_bytes(
        &objects.prev_preview_params.borrow(),
        config.preview_gamma.min(20.0),
    );
    show_preview_image(
        objects,
        bytes,
        image.width() as i32,
        image.height() as i32,
        config.preview_scale
    );
}

#[derive(PartialEq)]
enum PreviewFileMode {
    ResultFile,
    LightFile,
    BWCalibr,
    FlatFile,
}

fn preview_image_file(
    objects:   &Rc<MainWindow>,
    file_name: &Path,
    mode:      PreviewFileMode,
) {
    enum UiMessage {
        Image{
            image:     LightFile,
            file_name: PathBuf
        },
        Error(String),
    }

    // cancel previous task in thread pool
    if let Some(flag) = &*objects.prev_preview_cancel_flags.borrow() {
        flag.store(true, Ordering::Relaxed);
    }

    let preview_file_name = objects.builder.object::<gtk::Label>("preview_file_name").unwrap();
    let star_img          = objects.builder.object::<gtk::Image>("img_star").unwrap();
    let star_scr          = objects.builder.object::<gtk::ScrolledWindow>("scr_star").unwrap();
    let preview_ctrls_box = objects.builder.object::<gtk::Widget>("preview_ctrls_box").unwrap();

    let (sender, receiver) = async_channel::unbounded();

    glib::spawn_future_local(clone!(@strong objects, @strong preview_file_name, @strong preview_ctrls_box => async move {
        while let Ok(message) = receiver.recv().await {
            match message {
                UiMessage::Image { image, file_name } => {
                    let config = objects.config.borrow();

                    // show preview image
                    let params = image.image.calc_to_bytes_params(
                        config.preview_auto_min,
                        config.preview_auto_wb
                    );

                    let bytes = image.image.to_rgb_bytes(&params, config.preview_gamma);
                    show_preview_image(
                        &objects,
                        bytes,
                        image.image.width() as i32,
                        image.image.height() as i32,
                        config.preview_scale
                    );

                    // Show common star image
                    if let Ok(mut star_stat) = image.stars_stat {
                        for v in star_stat.common_stars_img.as_slice_mut() {
                            *v = (*v - 0.5) * 10.0 + 0.5; // + contrast
                        }
                        let bytes = star_stat.common_stars_img.to_rgb_bytes(0.0, 1.0, 1.0);
                        let bytes = glib::Bytes::from_owned(bytes);
                        let pixbuf = gdk_pixbuf::Pixbuf::from_bytes(
                            &bytes,
                            gdk_pixbuf::Colorspace::Rgb,
                            false,
                            8,
                            star_stat.common_stars_img.width() as i32,
                            star_stat.common_stars_img.height() as i32,
                            star_stat.common_stars_img.width() as i32 * 3,
                        );

                        star_img.set_pixbuf(Some(&pixbuf));
                        star_scr.set_width_request(star_stat.common_stars_img.width() as i32 + 4);
                    } else {
                        star_img.set_pixbuf(None);
                    }

                    preview_file_name.set_label(file_name.to_str().unwrap_or(""));
                    *objects.last_preview_file.borrow_mut() = file_name;
                    *objects.prev_preview_img.borrow_mut() = image.image;
                    *objects.prev_preview_params.borrow_mut() = params;
                    preview_ctrls_box.set_sensitive(true);
                },

                UiMessage::Error(text) => {
                    preview_ctrls_box.set_sensitive(true);
                    preview_file_name.set_label(&text);
                    objects.preview_image.clear();
                    objects.prev_preview_img.borrow_mut().clear();
                    objects.last_preview_file.borrow_mut().clear();
                },
            }
            break; // while
        }
    }));

    let cancel_flag = Arc::new(AtomicBool::new(false));
    *objects.prev_preview_cancel_flags.borrow_mut() = Some(cancel_flag.clone());
    preview_file_name.set_label("???");
    preview_ctrls_box.set_sensitive(false);

    let file_name = file_name.to_path_buf();
    let project = objects.project.borrow();
    let bin = match project.config().image_size {
        ImageSize::Bin2x2 =>
            if mode == PreviewFileMode::ResultFile {1} else {2},
        ImageSize::Original =>
            1,
    };

    let mut raw_params = project.config().raw_params.clone();

    objects.preview_tp.spawn(clone!(@strong cancel_flag => move || {
        if cancel_flag.load(Ordering::Relaxed) { return; }
        let calibr_data = CalibrationData::new_empty();

        let flags = match mode {
            PreviewFileMode::ResultFile |
            PreviewFileMode::LightFile =>
                LoadLightFlags::STARS_STAT |
                LoadLightFlags::STARS |
                LoadLightFlags::NO_ERR_IF_NO_STARS,
            PreviewFileMode::BWCalibr =>
                LoadLightFlags::DO_NOT_DEMOSAIC,
            PreviewFileMode::FlatFile =>
                LoadLightFlags::empty(),
        };

        if mode == PreviewFileMode::ResultFile {
            raw_params.force_cfa = None;
        }

        let light_file = LightFile::load_and_calc_params(
            &file_name,
            &calibr_data,
            flags,
            OpenMode::Preview,
            bin,
            &raw_params
        );

        match light_file {
            Ok(light_file) => {
                sender.send_blocking(UiMessage::Image { image: light_file, file_name}).unwrap();
            },
            Err(error) => {
                sender.send_blocking(UiMessage::Error(error.to_string())).unwrap();
            }
        }
    }));
}

fn show_preview_image(
    objects:    &Rc<MainWindow>,
    img_bytes:  Vec<u8>,
    img_width:  i32,
    img_height: i32,
    scale:      ImgScale
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
        // TODO: get gtk::ScrolledWindows directly!
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

fn action_new_group(objects: &Rc<MainWindow>) {
    let def_group_options = GroupOptions::default();
    let dialog = group_options_dialog(
        objects,
        gettext("Add new group"),
        def_group_options,
        clone!(@strong objects => move |group_options| {
            objects.project.borrow_mut().add_new_group(group_options);
            update_project_tree(&objects);
            log::info!("New group created");
        })
    );

    set_dialog_default_button(&dialog);
    dialog.show();
}

fn action_delete_item(objects: &Rc<MainWindow>) {
    let selection = get_current_selection(objects);
    let project = objects.project.borrow();
    match selection {
        SelectedItem {
            item_type: SelItemType::Group,
            group_idx: Some(group_idx),
            ..
        } => {
            let group = &project.groups()[group_idx];
            let dialog = confirm_dialog(
                &objects.window,
                transl_and_replace(
                    "Remove group '{group}' from project?",
                    &[("{group}", group.name(group_idx))]
                ),
                clone!(@strong objects => move || {
                    let group = objects.project.borrow_mut().remove_group(group_idx);
                    update_project_tree(&objects);
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
            files.sort_unstable();
            let dialog_text = if files.len() == 1 {
                let group = &project.groups()[group_idx];
                let folder = &group.get_file_list_by_type(file_type);
                let file = &folder.list()[files[0]];
                transl_and_replace(
                    "Remove file '{fn}' from project?",
                    &[("{fn}", path_to_string(file.file_name()))]
                )
            } else {
                transl_and_replace(
                    "Remove {cnt} files from project?",
                    &[("{cnt}", files.len().to_string())]
                )
            };
            let dialog = confirm_dialog(
                &objects.window,
                dialog_text,
                clone!(@strong objects => move || {
                    objects.prj_tree.selection().unselect_all();
                    objects.project.borrow_mut()
                        .group_by_index_mut(group_idx)
                        .file_list_by_type_mut(file_type)
                        .remove_files_by_idx(files.clone());
                    update_project_tree(&objects);
                })
            );
            dialog.show()
        },

        _ => (),
    }
}

fn action_item_properties(objects: &Rc<MainWindow>) {
    let selection = get_current_selection(objects);

    match selection {
        SelectedItem {
            item_type: SelItemType::Group,
            group_idx: Some(group_idx),
            ..
        } => {
            let dialog = group_options_dialog(
                objects,
                gettext("Group properties"),
                objects.project.borrow().groups()[group_idx].options().clone(),
                clone!(@strong objects => move |new_options| {
                    let mut project = objects.project.borrow_mut();
                    let group = project.group_by_index_mut(group_idx);
                    log::info!("Group '{}' options changed to {:?}", group.name(group_idx), new_options);
                    group.set_options(new_options);
                    drop(project);
                    update_project_tree(&objects);
                })
            );
            set_dialog_default_button(&dialog);
            dialog.show();
        },

        SelectedItem {item_type: SelItemType::Project, ..} => {
            action_project_options(objects);
        },

        _ => (),
    }
}

fn group_options_dialog<F: Fn(GroupOptions) + 'static>(
    objects:       &Rc<MainWindow>,
    title:         String,
    group_options: GroupOptions,
    fun:           F
) -> gtk::Dialog {
    let builder = gtk::Builder::from_string(include_str!("ui/group_options_dialog.ui"));
    let dialog          = builder.object::<gtk::Dialog>("group_options_dialog").unwrap();
    let rb_default_name = builder.object::<gtk::RadioButton>("rb_default_name").unwrap();
    let rb_custom_name  = builder.object::<gtk::RadioButton>("rb_custom_name").unwrap();
    let e_name          = builder.object::<gtk::Entry>("e_name").unwrap();

    dialog.set_title(&title);
    dialog.set_transient_for(Some(&objects.window));

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            (&gettext("_Ok"), gtk::ResponseType::Ok),
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
            (&gettext("_Ok"), gtk::ResponseType::Ok),
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

fn get_active_group_index(objects: &Rc<MainWindow>, title: &str) -> Option<usize> {
    match objects.project.borrow().groups().len() {
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

    for (idx, group) in objects.project.borrow().groups().iter().enumerate() {
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

fn action_project_options(objects: &Rc<MainWindow>) {
    configure_project_options(
        objects,
        objects.project.borrow().config().clone(),
        clone!(@strong objects => move |new_config| {
            log::info!("New project options:\n{:#?}", new_config);
            objects.project.borrow_mut().set_new_config(new_config);
            update_project_tree(&objects);
            update_project_name_and_time_in_gui(&objects);

            *objects.last_preview_file.borrow_mut() = PathBuf::new();
            preview_selected_file(&objects);
        })
    );
}

fn configure_project_options<F: Fn(ProjectConfig) + 'static>(
    objects:        &Rc<MainWindow>,
    project_config: ProjectConfig,
    set_fun:        F,
) {
    let builder = gtk::Builder::from_string(include_str!("ui/project_options_dialog.ui"));
    let dialog = builder.object::<gtk::Dialog>("project_options_dialog").unwrap();
    let project_name = builder.object::<gtk::Entry>("project_name").unwrap();
    let img_size = builder.object::<gtk::ComboBoxText>("img_size").unwrap();
    let res_img_type = builder.object::<gtk::ComboBoxText>("res_img_type").unwrap();
    let align_rgb = builder.object::<gtk::CheckButton>("chb_align_rgb").unwrap();
    let align_rgb_each = builder.object::<gtk::CheckButton>("chb_align_rgb_each").unwrap();
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

    let cb_cfa_array = builder.object::<gtk::ComboBoxText>("cb_cfa_array").unwrap();
    let chb_apply_wb = builder.object::<gtk::CheckButton>("chb_apply_wb").unwrap();
    let chb_apply_color = builder.object::<gtk::CheckButton>("chb_apply_color").unwrap();

    let chb_save_calibrated_img = builder.object::<gtk::CheckButton>("chb_save_calibrated_img").unwrap();
    let chb_save_common_star_img = builder.object::<gtk::CheckButton>("chb_save_common_star_img").unwrap();

    project_name.set_text(project_config.name.as_ref().unwrap_or(&String::new()).as_str());

    img_size.set_active(Some(match project_config.image_size {
        ImageSize::Original => 0,
        ImageSize::Bin2x2 => 1,
    }));

    res_img_type.set_active(Some(match project_config.res_img_type {
        ResFileType::Fit => 0,
        ResFileType::Tif => 1,
    }));

    align_rgb.set_active(project_config.align_rgb);
    align_rgb_each.set_active(project_config.align_rgb_each);

    let show_calc_opts = |opts: &CalcOpts, mode: &gtk::ComboBoxText, kappa: &gtk::Entry, steps: &gtk::Entry| {
        mode.append_text("Kappa-Sigma clipping");
        mode.append_text(&gettext("Median"));
        mode.append_text(&gettext("Mean"));

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

    chb_save_calibrated_img.set_active(project_config.save_aligned_img);
    chb_save_common_star_img.set_active(project_config.save_common_star_img);

    cb_cfa_array.set_active(Some(match project_config.raw_params.force_cfa {
        None                => 0,
        Some(CfaType::GBRG) => 1,
        Some(CfaType::RGGB) => 2,
        Some(CfaType::BGGR) => 3,
        Some(CfaType::GRBG) => 4,
    }));

    chb_apply_wb.set_active(project_config.raw_params.apply_wb);
    chb_apply_color.set_active(project_config.raw_params.apply_color);
    chb_apply_color.set_sensitive(chb_apply_wb.is_active());

    chb_apply_wb.connect_active_notify(clone!(@strong chb_apply_color => move |v| {
        chb_apply_color.set_sensitive(v.is_active());
    }));

    dialog.set_transient_for(Some(&objects.window));

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            (&gettext("_Ok"), gtk::ResponseType::Ok),
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
            (&gettext("_Ok"), gtk::ResponseType::Ok),
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

            project_config.align_rgb = align_rgb.is_active();
            project_config.align_rgb_each = align_rgb_each.is_active();

            project_config.save_aligned_img = chb_save_calibrated_img.is_active();
            project_config.save_common_star_img = chb_save_common_star_img.is_active();

            project_config.raw_params.force_cfa = match cb_cfa_array.active() {
                Some(0) => None,
                Some(1) => Some(CfaType::GBRG),
                Some(2) => Some(CfaType::RGGB),
                Some(3) => Some(CfaType::BGGR),
                Some(4) => Some(CfaType::GRBG),
                _ => panic!("Wrong cb_cfa_array.active(): {:?}", cb_cfa_array.active()),
            };

            project_config.raw_params.apply_wb = chb_apply_wb.is_active();
            project_config.raw_params.apply_color = chb_apply_color.is_active();

            set_fun(project_config);
        }
        dialog.close();
    }));

    set_dialog_default_button(&dialog);
    dialog.show();
}

fn check_all_light_files_are_registered(objects: &Rc<MainWindow>) -> bool {
    let project = objects.project.borrow();
    if !project.is_any_used_light_file() {
        return false;
    }
    if !project.is_all_light_files_are_registered() {
        show_error_message(
            &objects.window, &gettext("Error"),
            &gettext("You have execute register light files first!")
        );
        return false;
    }
    true
}

fn action_cleanup_light_files(objects: &Rc<MainWindow>) {
    if !check_all_light_files_are_registered(objects) {
        return;
    }

    let builder = gtk::Builder::from_string(include_str!("ui/cleanup_dialog.ui"));
    let dialog = builder.object::<gtk::Dialog>("cleanup_dialog").unwrap();
    let chbt_check_before = builder.object::<gtk::CheckButton>("check_before").unwrap();

    let project = objects.project.borrow();
    chbt_check_before.set_active(project.cleanup_conf().check_before_execute);

    let show_line = |
        item: &CleanupConfItem,
        chk_name: &str,
        mode_name: &str,
        kappa_name: &str,
        repeats_name: &str,
        percent_name: &str,
        min_name: &str,
        max_name: &str,
    | -> (gtk::CheckButton, gtk::ComboBoxText, gtk::Entry, gtk::Entry, gtk::Entry, gtk::Entry, gtk::Entry) {
        let chk_w = builder.object::<gtk::CheckButton>(chk_name).unwrap();
        let mode_w = builder.object::<gtk::ComboBoxText>(mode_name).unwrap();
        let kappa_w = builder.object::<gtk::Entry>(kappa_name).unwrap();
        let repeats_w = builder.object::<gtk::Entry>(repeats_name).unwrap();
        let percent_w = builder.object::<gtk::Entry>(percent_name).unwrap();
        let min_w = builder.object::<gtk::Entry>(min_name).unwrap();
        let max_w = builder.object::<gtk::Entry>(max_name).unwrap();

        let correct_sensivity = |
            chk_w: &gtk::CheckButton,
            mode_w: &gtk::ComboBoxText,
            kappa_w: &gtk::Entry,
            repeats_w: &gtk::Entry,
            percent_w: &gtk::Entry,
            min_w: &gtk::Entry,
            max_w: &gtk::Entry,
        | {
            let is_used = chk_w.is_active();
            let sigma_clip = mode_w.active() == Some(0);
            let percent = mode_w.active() == Some(1);
            let min_max = mode_w.active() == Some(2);

            mode_w.set_sensitive(is_used);
            kappa_w.set_sensitive(is_used && sigma_clip);
            repeats_w.set_sensitive(is_used && sigma_clip);
            percent_w.set_sensitive(is_used && percent);
            min_w.set_sensitive(is_used && min_max);
            max_w.set_sensitive(is_used && min_max);
        };

        chk_w.connect_active_notify(clone!(@weak mode_w, @weak kappa_w, @weak repeats_w, @weak percent_w, @weak min_w, @weak max_w => move |chk| {
            correct_sensivity(chk, &mode_w, &kappa_w, &repeats_w, &percent_w, &min_w, &max_w);
        }));
        chk_w.set_active(!item.used);
        chk_w.set_active(item.used);

        mode_w.connect_changed(clone!(@weak chk_w, @weak kappa_w, @weak repeats_w, @weak percent_w, @weak min_w, @weak max_w => move |cbt| {
            correct_sensivity(&chk_w, cbt, &kappa_w, &repeats_w, &percent_w, &min_w, &max_w);
        }));
        mode_w.append_text(&gettext("Sigma clipping"));
        mode_w.append_text(&gettext("Percent"));
        mode_w.append_text(&gettext("Min/Max"));
        match item.mode {
            CleanupMode::SigmaClipping => mode_w.set_active(Some(0)),
            CleanupMode::Percent => mode_w.set_active(Some(1)),
            CleanupMode::MinMax => mode_w.set_active(Some(2)),
        }

        kappa_w.set_text(&format!("{:.1}", item.kappa));
        repeats_w.set_text(&format!("{}", item.repeats));
        percent_w.set_text(&format!("{}", item.percent));

        min_w.set_text(if let Some(min) = item.min { format!("{:.2}", min) } else { "".to_string() }.as_str());
        max_w.set_text(if let Some(max) = item.max { format!("{:.2}", max) } else { "".to_string() }.as_str());

        (chk_w, mode_w, kappa_w, repeats_w, percent_w, min_w, max_w)
    };

    let (chb_rdev, cbt_rdev, e_rdev_kappa, e_rdev_repeats, e_rdev_percent, e_rdev_min, e_rdev_max) =
        show_line(&project.cleanup_conf().stars_r_dev, "chb_rdev", "cbt_rdev", "e_rdev_kappa", "e_rdev_repeats", "e_rdev_percent", "e_rdev_min", "e_rdev_max");
    let (chb_fwhm, cbt_fwhm, e_fwhm_kappa, e_fwhm_repeats, e_fwhm_percent, e_fwhm_min, e_fwhm_max) =
        show_line(&project.cleanup_conf().stars_fwhm, "chb_fwhm", "cbt_fwhm", "e_fwhm_kappa", "e_fwhm_repeats", "e_fwhm_percent", "e_fwhm_min", "e_fwhm_max");
    let (chb_stars, cbt_stars, e_stars_kappa, e_stars_repeats, e_stars_percent, e_stars_min, e_stars_max) =
        show_line(&project.cleanup_conf().stars_count, "chb_stars", "cbt_stars", "e_stars_kappa", "e_stars_repeats", "e_stars_percent", "e_stars_min", "e_stars_max");
    let (chb_noise, cbt_noise, e_noise_kappa, e_noise_repeats, e_noise_percent, e_noise_min, e_noise_max) =
        show_line(&project.cleanup_conf().noise, "chb_noise", "cbt_noise", "e_noise_kappa", "e_noise_repeats", "e_noise_percent", "e_noise_min", "e_noise_max");
    let (chb_bg, cbt_bg, e_bg_kappa, e_bg_repeats, e_bg_percent, e_bg_min, e_bg_max) =
        show_line(&project.cleanup_conf().background, "chb_bg", "cbt_bg", "e_bg_kappa", "e_bg_repeats", "e_bg_percent", "e_bg_min", "e_bg_max");

    drop(project);

    dialog.set_transient_for(Some(&objects.window));
    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            (&gettext("_Cleanup"), gtk::ResponseType::Ok),
            (&gettext("_Close"), gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            (&gettext("_Close"), gtk::ResponseType::Cancel),
            (&gettext("_Cleanup"), gtk::ResponseType::Ok),
        ]);
    }

    dialog.connect_response(clone!(@strong objects => move |dialog, response| {
        if response == gtk::ResponseType::Ok {
            let mut project = objects.project.borrow_mut();

            let mut cleanup_conf = project.cleanup_conf().clone();

            cleanup_conf.check_before_execute = chbt_check_before.is_active();

            let get_line = |
                item:      &mut CleanupConfItem,
                chb_use:   &gtk::CheckButton,
                cbt_mode:  &gtk::ComboBoxText,
                e_kappa:   &gtk::Entry,
                e_repeats: &gtk::Entry,
                e_percent: &gtk::Entry,
                e_min:     &gtk::Entry,
                e_max:     &gtk::Entry,
            | {
                item.used = chb_use.is_active();
                item.mode = match cbt_mode.active() {
                    Some(0) => CleanupMode::SigmaClipping,
                    Some(1) => CleanupMode::Percent,
                    Some(2) => CleanupMode::MinMax,
                    _       => panic!("Wrong cbt_mode.active(): {:?}", cbt_mode.active()),
                };
                item.kappa = e_kappa.text().as_str().parse().unwrap_or(item.kappa);
                item.repeats = e_repeats.text().as_str().parse().unwrap_or(item.repeats);
                item.percent = e_percent.text().as_str().parse().unwrap_or(item.percent);
                item.min = e_min.text().as_str().parse().ok();
                item.max = e_max.text().as_str().parse().ok();
            };

            get_line(&mut cleanup_conf.stars_r_dev,   &chb_rdev,  &cbt_rdev,  &e_rdev_kappa,  &e_rdev_repeats,  &e_rdev_percent,  &e_rdev_min,  &e_rdev_max);
            get_line(&mut cleanup_conf.stars_fwhm,    &chb_fwhm,  &cbt_fwhm,  &e_fwhm_kappa,  &e_fwhm_repeats,  &e_fwhm_percent,  &e_fwhm_min,  &e_fwhm_max);
            get_line(&mut cleanup_conf.stars_count,   &chb_stars, &cbt_stars, &e_stars_kappa, &e_stars_repeats, &e_stars_percent, &e_stars_min, &e_stars_max);
            get_line(&mut cleanup_conf.noise,         &chb_noise, &cbt_noise, &e_noise_kappa, &e_noise_repeats, &e_noise_percent, &e_noise_min, &e_noise_max);
            get_line(&mut cleanup_conf.background,    &chb_bg,    &cbt_bg,    &e_bg_kappa,    &e_bg_repeats,    &e_bg_percent,    &e_bg_min,    &e_bg_max);

            project.set_cleanup_conf(cleanup_conf);

            drop(project);

            let result = objects.project.borrow_mut().cleanup_light_files();
            match result {
                Ok(cleaned_up_count) => {
                    let total_files = objects.project.borrow().total_light_files_count();

                    let message = transl_and_replace(
                        "Cleaned up {cleaned} files of {total} ({percent}%)", &[
                        ("{cleaned}", cleaned_up_count.to_string()),
                        ("{total}",   total_files.to_string()),
                        ("{percent}", format!("{:.1}", 100.0 * cleaned_up_count as f64 / total_files as f64)),
                    ]);

                    show_message(
                        &objects.window,
                        &gettext("Cleanup light files result"),
                        &message,
                        gtk::MessageType::Info,
                    );
                },
                Err(error) =>
                    show_error_message(&objects.window, &gettext("Error"), &error.to_string()),
            }
            update_project_tree(&objects);
            update_project_name_and_time_in_gui(&objects);
        }
        else {
            dialog.close();
        }
    }));

    set_dialog_default_button(&dialog);
    dialog.show();
}

fn ask_user_to_assign_ref_light_image_auto(
    objects:              &Rc<MainWindow>,
    start_stacking_after: bool
){
    let builder = gtk::Builder::from_string(include_str!("ui/assign_ref_light_frame.ui"));
    let dialog = builder.object::<gtk::Dialog>("assign_key_light_frame_dialog").unwrap();

    let rb_smallest_stars = builder.object::<gtk::RadioButton>("rb_smallest_stars").unwrap();
    let rb_roundest_stars = builder.object::<gtk::RadioButton>("rb_roundest_stars").unwrap();
    let rb_min_bg = builder.object::<gtk::RadioButton>("rb_min_bg").unwrap();
    let rb_min_noise = builder.object::<gtk::RadioButton>("rb_min_noise").unwrap();

    dialog.set_transient_for(Some(&objects.window));
    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            (&gettext("_Assign"), gtk::ResponseType::Ok),
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
            (&gettext("_Assign"), gtk::ResponseType::Ok),
        ]);
    }

    match objects.project.borrow().config().ref_image_auto_mode {
        RefImageAutoMode::SmallestStars => rb_smallest_stars.set_active(true),
        RefImageAutoMode::RoundestStars => rb_roundest_stars.set_active(true),
        RefImageAutoMode::MinBg         => rb_min_bg.set_active(true),
        RefImageAutoMode::NinNoise      => rb_min_noise.set_active(true),
    }

    dialog.connect_response(clone!(@strong objects => move |dialog, response| {
        if response == gtk::ResponseType::Ok {
            let mut project = objects.project.borrow_mut();
            let mut config = project.config().clone();
            config.ref_image_auto_mode =
                if rb_smallest_stars.is_active()      { RefImageAutoMode::SmallestStars }
                else if rb_roundest_stars.is_active() { RefImageAutoMode::RoundestStars }
                else if rb_min_bg.is_active()         { RefImageAutoMode::MinBg }
                else                                  { RefImageAutoMode::NinNoise };

            project.set_config(config);
            project.assign_ref_light_frame_automatically();

            drop(project);

            update_project_tree(&objects);
            update_project_name_and_time_in_gui(&objects);

            if start_stacking_after {
                action_stack(&objects);
            }
        }
        dialog.close();
    }));

    set_dialog_default_button(&dialog);
    dialog.show();
}

fn action_assign_ref_light_image(objects: &Rc<MainWindow>) {
    if !check_all_light_files_are_registered(objects) {
        return;
    }

    if !objects.project.borrow().is_possible_assign_ref_light_frame_automatically() {
        show_error_message(
            &objects.window, &gettext("Error"),
            &gettext("It is not possible to assign reference light image automatically")
        );
        return;
    }

    ask_user_to_assign_ref_light_image_auto(objects, false);
}

fn action_stack(objects: &Rc<MainWindow>) {
    let project = objects.project.borrow();
    if !project.is_any_used_light_file() {
        return;
    }
    if !project.is_ref_image_assigned()
    && project.is_possible_assign_ref_light_frame_automatically() {
        ask_user_to_assign_ref_light_image_auto(objects, true);
        return;
    }
    match project.can_exec_stack_light_files() {
        CanExecStackLightsRes::Ok => (),
        CanExecStackLightsRes::NoRefFile => {
            show_error_message(
                &objects.window, &gettext("Error"),
                &gettext("Reference image is not defined")
            );
            return;
        },
    }
    log::info!("Stacking light files started");
    let project_json = project.to_json_string();
    let cpu_load = objects.config.borrow().cpu_load;
    drop(project);
    exec_and_show_progress(
        objects,
        move|progress, is_canceled| {
            let project = Project::from_json_string(&project_json);
            project.stack_light_files(progress, is_canceled, cpu_load)
        },
        move |objects, result| {
            preview_image_file(objects, &result.file_name, PreviewFileMode::ResultFile);
            show_message(
                &objects.window,
                &gettext("Finished"),
                &transl_and_replace(
                    "Result file saved to {fn}",
                    &[("{fn}", path_to_string(&result.file_name))]
                ),
                gtk::MessageType::Info,
            )
        }
    );
}

fn action_use_as_reference_image(objects: &Rc<MainWindow>) {
    if let SelectedItem {
        item_type: SelItemType::File,
        file_type: Some(ProjectFileType::Light),
        group_idx: Some(group_idx),
        files,
    } = get_current_selection(objects) {
        if files.len() == 1 {
            let mut project = objects.project.borrow_mut();
            let file_name =  project.groups()[group_idx]
                .light_files.list()[files[0]]
                .file_name()
                .clone();
            project.set_ref_image(file_name);
        } else {
            return;
        }

        update_project_tree(objects);
    }
}

fn action_change_file_to_light(objects: &Rc<MainWindow>) {
    change_selected_files_type(objects, ProjectFileType::Light);
}

fn action_change_file_to_dark(objects: &Rc<MainWindow>) {
    change_selected_files_type(objects, ProjectFileType::Dark);
}

fn action_change_file_to_flat(objects: &Rc<MainWindow>) {
    change_selected_files_type(objects, ProjectFileType::Flat);
}

fn action_change_file_to_bias(objects: &Rc<MainWindow>) {
    change_selected_files_type(objects, ProjectFileType::Bias);
}

fn change_selected_files_type(
    objects:  &Rc<MainWindow>,
    new_type: ProjectFileType
) {
    let selection = get_current_selection(objects);
    if selection.item_type != SelItemType::File {
        return;
    }

    let message = transl_and_replace(
        "Change type of {count} file(s) from {from_type} into {into_type}?", &[
        ("{count}",     selection.files.len().to_string()),
        ("{from_type}", debug_to_str(&selection.file_type.unwrap())),
        ("{into_type}", debug_to_str(&new_type))
    ]);

    let dialog = gtk::MessageDialog::builder()
        .transient_for(&objects.window)
        .title(&gettext("Change file types"))
        .text(&message)
        .modal(true)
        .message_type(gtk::MessageType::Question)
        .build();

    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            (&gettext("_Yes"), gtk::ResponseType::Yes),
            (&gettext("_No"), gtk::ResponseType::No),
        ]);
    } else {
        dialog.add_buttons(&[
            (&gettext("_No"), gtk::ResponseType::No),
            (&gettext("_Yes"), gtk::ResponseType::Yes),
        ]);
    }

    dialog.connect_response(clone!(@strong objects => move |dlg, resp| {
        if resp == gtk::ResponseType::Yes {
            let mut project = objects.project.borrow_mut();
            let group = project.group_by_index_mut(selection.group_idx.unwrap());
            group.change_file_types(
                selection.file_type.unwrap(),
                new_type,
                selection.files.clone()
            );
            drop(project);
            update_project_tree(&objects);
            update_project_name_and_time_in_gui(&objects);
        }
        dlg.close();
    }));

    set_dialog_default_button(&dialog);
    dialog.show();
}

fn action_move_file_to_group(objects: &Rc<MainWindow>) {
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
            (&gettext("_Ok"), gtk::ResponseType::Ok),
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
        ]);
    } else {
        dialog.add_buttons(&[
            (&gettext("_Cancel"), gtk::ResponseType::Cancel),
            (&gettext("_Ok"), gtk::ResponseType::Ok),
        ]);
    }

    let project = objects.project.borrow();
    for (idx, group) in project.groups().iter().enumerate() {
        if Some(idx) != selection.group_idx {
            cbx_existing_groups.append(Some(group.uuid()), &group.name(idx));
        }
    }
    let move_to_group_last_uuid = objects.move_to_group_last_uuid.borrow();
    if !move_to_group_last_uuid.is_empty() {
        cbx_existing_groups.set_active_id(Some(&*move_to_group_last_uuid));
    }

    if project.groups().len() <= 1 {
        rbtn_existing_group.set_sensitive(false);
        rbtn_new_group.set_active(true);
    } else {
        rbtn_existing_group.set_active(true);
    }

    drop(project);

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
            let mut project = objects.project.borrow_mut();
            let group_id = if rbtn_new_group.is_active() {
                let mut group_options = GroupOptions::default();
                let new_group_name = e_new_group.text().to_string().trim().to_string();
                if !new_group_name.is_empty() {
                    group_options.name = Some(new_group_name);
                }
                project.add_new_group(group_options);
                project.groups().last().unwrap().uuid().to_string()
            } else {
                match cbx_existing_groups.active_id() {
                    Some(s) => s.to_string(),
                    _ => return,
                }
            };

            let from_group = project.group_by_index_mut(selection.group_idx.unwrap());
            let from_folder = from_group.file_list_by_type_mut(selection.file_type.unwrap());
            let files_to_move = from_folder.remove_files_by_idx(selection.files.clone());

            let to_group = project.find_group_by_uuid_mut(&group_id).unwrap();
            let to_folder = to_group.file_list_by_type_mut(selection.file_type.unwrap());

            to_folder.add_files(files_to_move);
            drop(project);

            update_project_tree(&objects);
            update_project_name_and_time_in_gui(&objects);
        }
        dlg.close();
    }));

    set_dialog_default_button(&dialog);
    dialog.show();
}

fn action_check_all_files(objects: &Rc<MainWindow>) {
    check_all_files(objects, true);
}

fn action_uncheck_all_files(objects: &Rc<MainWindow>) {
    check_all_files(objects, false);
}

fn check_all_files(objects: &Rc<MainWindow>, value: bool) {
    let s = get_current_selection(objects);
    let (group_idx, file_type) = match (s.group_idx, s.file_type) {
        (Some(group_idx), Some(file_type)) => (group_idx, file_type),
        _ => return,
    };

    objects.project.borrow_mut()
        .group_by_index_mut(group_idx)
        .file_list_by_type_mut(file_type)
        .check_all(value);
    update_project_tree(objects);
    update_project_name_and_time_in_gui(objects);
}

fn action_check_selected_files(objects: &Rc<MainWindow>) {
    check_selected_files(objects, true);
}

fn action_uncheck_selected_files(objects: &Rc<MainWindow>) {
    check_selected_files(objects, false);
}

fn check_selected_files(objects: &Rc<MainWindow>, value: bool) {
    let s = get_current_selection(objects);
    let (group_idx, file_type, files) = match (s.group_idx, s.file_type, s.files) {
        (Some(group_idx), Some(file_type), files) if !files.is_empty() =>
            (group_idx, file_type, files),
        _ =>
            return,
    };

    objects.project.borrow_mut()
        .group_by_index_mut(group_idx)
        .file_list_by_type_mut(file_type)
        .check_by_indices(&files, value);
    update_project_tree(objects);
    update_project_name_and_time_in_gui(objects);
}

fn action_about(objects: &Rc<MainWindow>) {
    let builder = gtk::Builder::from_string(include_str!("ui/about_dialog.ui"));
    let dialog = builder.object::<gtk::Dialog>("dialog").unwrap();
    let image = builder.object::<gtk::Image>("image").unwrap();
    let btn_close = builder.object::<gtk::Button>("btn_close").unwrap();
    let l_version = builder.object::<gtk::Label>("l_version").unwrap();
    let l_app_descr = builder.object::<gtk::Label>("l_app_descr").unwrap();
    let logo_image = gdk_pixbuf::Pixbuf::from_read(include_bytes!(
        r"ui/electra_128x128.png"
    ).as_slice()).unwrap();
    if l_app_descr.label() == "APP_DESCRIPTION" {
        l_app_descr.set_label(env!("CARGO_PKG_DESCRIPTION"))
    }
    image.set_pixbuf(Some(&logo_image));
    l_version.set_label(&format!("v{}", env!("CARGO_PKG_VERSION")));
    if gettext("cur_lang") == "ru" {
        let lb_lb_discussion = builder.object::<gtk::LinkButton>("lb_discussion").unwrap();
        lb_lb_discussion.set_label("Обсудить на форуме astronomy.ru");
        lb_lb_discussion.set_uri("https://astronomy.ru/forum/index.php/topic,201076.0.html");
    }
    dialog.set_transient_for(Some(&objects.window));
    set_dialog_default_button(&dialog);
    dialog.show();
    btn_close.connect_clicked(move |_| dialog.close());
}

fn action_project_columns(objects: &Rc<MainWindow>) {
    let builder = gtk::Builder::from_string(include_str!("ui/columns_selector.ui"));
    let dialog = builder.object::<gtk::Dialog>("columns_delector").unwrap();
    let list = builder.object::<gtk::TreeView>("lv_list").unwrap();
    let btn_close = builder.object::<gtk::Button>("btn_close").unwrap();
    const COLUMN_CHECK: i32 = 0;
    const COLUMN_NAME: i32 = 1;
    const COLUMN_ID: i32 = 2;
    let model = gtk::ListStore::new(&[
        bool::static_type(),
        String::static_type(),
        String::static_type(),
    ]);
    let col = gtk::TreeViewColumn::builder()
        .title("Name")
        .resizable(true)
        .clickable(true)
        .build();
    let cell_check = gtk::CellRendererToggle::builder()
        .activatable(true)
        .mode(gtk::CellRendererMode::Activatable)
        .sensitive(true)
        .build();
    let cell_text = gtk::CellRendererText::new();
    TreeViewColumnExt::pack_start(&col, &cell_check, false);
    TreeViewColumnExt::pack_start(&col, &cell_text, true);
    TreeViewColumnExt::add_attribute(&col, &cell_text, "text", COLUMN_NAME);
    TreeViewColumnExt::add_attribute(&col, &cell_check, "active", COLUMN_CHECK);
    list.append_column(&col);
    let tree_columns = get_prj_tree_store_columns();
    for i in 0..objects.prj_tree.n_columns() {
        let tree_col = objects.prj_tree.column(i as i32).unwrap();
        let name = tree_columns[i as usize].0;
        let id = tree_columns[i as usize].1;
        model.insert_with_values(None, &[
            (COLUMN_CHECK as u32, &tree_col.is_visible()),
            (COLUMN_NAME as u32, &gettext(name)),
            (COLUMN_ID as u32, &id),
        ]);
    }
    list.set_model(Some(&model));
    cell_check.connect_toggled(clone!(@weak objects, @weak model => move |_, path| {
        let values = path.indices();
        let tree_col = objects.prj_tree.column(values[0]).unwrap();
        tree_col.set_visible(!tree_col.is_visible());
        let iter = model.iter(&path).unwrap();
        model.set(&iter, &[(COLUMN_CHECK as u32, &tree_col.is_visible())])
    }));
    dialog.set_transient_for(Some(&objects.window));
    set_dialog_default_button(&dialog);
    dialog.show();
    btn_close.connect_clicked(move |_| dialog.close());
}
