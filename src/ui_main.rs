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
use macros::FromBuilder;
use crate::ui_about_dialog::show_about_dialog;
use crate::ui_move_file_to_group_dialog::MoveFileToGroupDialog;
use crate::ui_prj_columns_dialog::PrjColumnsDialog;
use crate::ui_project_options_dialog::ProjectOptionsDialog;
use crate::{
    gtk_utils::*,
    image_io::*,
    image_raw::*,
    stacking_utils::*,
    light_file::*,
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


    // Creating main window and getting widgets

    let widgets = Widgets::from_builder_str(include_str!(r"ui/main_window.ui"));

    // Icons

    let icon_theme = gtk::IconTheme::default().unwrap();
    let icons = Icons {
        folder:    icon_theme.load_icon("folder", 16, gtk::IconLookupFlags::empty()).ok().flatten(),
        image:     icon_theme.load_icon("image-x-generic", 16, gtk::IconLookupFlags::empty()).ok().flatten(),
        photo:     icon_theme.load_icon("camera-photo-symbolic.symbolic", 16, gtk::IconLookupFlags::empty()).ok().flatten(),
        ref_image: gdk_pixbuf::Pixbuf::from_read(include_bytes!(r"ui/key.png").as_slice()).ok(),
    };

    let preview_tp = rayon::ThreadPoolBuilder::new()
        .num_threads(2)
        .build()
        .unwrap();

    // Shared objects for main window

    let main_window = Rc::new(MainWindow {
        widgets,
        icons,
        project: Rc::new(RefCell::new(project)),
        config: RefCell::new(config),
        prj_tree_is_building: Cell::new(false),
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

        main_window.widgets.project_tree.append_column(&col);
    }

    cell_check.connect_toggled(clone!(@weak main_window => move |_, path| {
        if !main_window.prj_tree_is_building.get() {
            main_window.handler_project_tree_checked_changed(path);
        }
    }));

    let window = &main_window.widgets.window;


    // Load and apply config

    let res = main_window.config.borrow_mut().load();
    if let Err(error) = res {
        show_error_message(&main_window.widgets.window, &gettext("Error"), &error.to_string());
    }
    main_window.apply_config();

    // Font (only for MS Windows)

    if cfg!(target_os = "windows") {
        let settings = gtk::Settings::default().unwrap();
        settings.set_property("gtk-font-name", "Tahoma 9");
    }

    // Show empty project in tree

    main_window.update_project_tree();
    main_window.update_project_name_and_time_in_gui();

    main_window.connect_ui_events();
    main_window.enable_actions();


    // Show main window

    window.set_application(Some(application));
    window.show_all();
}

#[derive(FromBuilder)]
struct Widgets {
    prj_tree_menu:       gtk::Menu,
    mi_change_file_type: gtk::MenuItem,
    window:              gtk::ApplicationWindow,
    menu_bar:            gtk::MenuBar,
    recent_menu:         gtk::RecentChooserMenu,
    mi_theme:            gtk::MenuItem,
    mi_dark_theme:       gtk::RadioMenuItem,
    mi_light_theme:      gtk::RadioMenuItem,
    mi_cpu_load:         gtk::MenuItem,
    mi_cpu_load_min:     gtk::RadioMenuItem,
    mi_cpu_load_half:    gtk::RadioMenuItem,
    mi_cpu_load_max:     gtk::RadioMenuItem,
    prj_img_paned:       gtk::Paned,
    project_tree:        gtk::TreeView,
    preview_fname:       gtk::Label,
    preview_ctrls_box:   gtk::Box,
    preview_img_scale:   gtk::ComboBox,
    preview_img_gamma:   gtk::Scale,
    preview_auto_min:    gtk::CheckButton,
    preview_auto_wb:     gtk::CheckButton,
    scr_star:            gtk::ScrolledWindow,
    img_star:            gtk::Image,
    scr_preview_image:   gtk::ScrolledWindow,
    eb_preview:          gtk::EventBox,
    img_preview:         gtk::Image,
    progress_box:        gtk::Box,
}

struct Icons {
    folder:    Option<gdk_pixbuf::Pixbuf>,
    image:     Option<gdk_pixbuf::Pixbuf>,
    photo:     Option<gdk_pixbuf::Pixbuf>,
    ref_image: Option<gdk_pixbuf::Pixbuf>,
}

struct MainWindow {
    widgets: Widgets,
    icons: Icons,
    project: Rc<RefCell<Project>>,
    config: RefCell<Config>,
    last_preview_file: RefCell<PathBuf>,
    preview_tp: rayon::ThreadPool,
    prev_preview_cancel_flags: RefCell<Option<Arc<AtomicBool>>>,
    prev_preview_img: RefCell<image::Image>,
    prev_preview_params: RefCell<image::ToRgbBytesParams>,
    preview_scroll_pos: RefCell<Option<((f64, f64), (f64, f64))>>,
    global_cancel_flag: Arc<AtomicBool>,
    move_to_group_last_uuid: RefCell<String>,
    prj_tree_is_building: Cell<bool>,
    tasks_count: Cell<u32>,
    trying_to_close: Cell<bool>,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum ColIdx {
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

#[derive(PartialEq, Clone)]
pub enum SelItemType {
    None,
    Project,
    Group,
    FileType,
    File
}

#[derive(Clone)]
pub struct SelectedItem {
    pub item_type: SelItemType,
    pub group_idx: Option<usize>,
    pub file_type: Option<ProjectFileType>,
    pub files: Vec<usize>,
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

#[derive(PartialEq)]
enum PreviewFileMode {
    ResultFile,
    LightFile,
    BWCalibr,
    FlatFile,
}

pub fn get_prj_tree_store_columns() -> Vec::<(&'static str, &'static str, ColIdx, ColIdx, glib::Type)> {
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

impl MainWindow {
    fn connect_ui_events(self: &Rc<Self>) {

        // Drag-n-drop for files
        let targets = vec![
            gtk::TargetEntry::new("text/uri-list", gtk::TargetFlags::OTHER_APP, 0),
        ];
        self.widgets.window.drag_dest_set(gtk::DestDefaults::DROP, &targets, gtk::gdk::DragAction::COPY);
        self.widgets.window.drag_dest_set_track_motion(true);
        self.widgets.window.connect_drag_data_received(
            clone!(@strong self as self_ => move |_w, _, _, _, sd, _, _| {
                self_.handler_files_dropped(sd);
            })
        );

        self.widgets.project_tree.connect_button_press_event(
            clone!(@weak self as self_ => @default-return glib::Propagation::Proceed, move |_, evt| {
                if (evt.button() == gtk::gdk::ffi::GDK_BUTTON_SECONDARY as u32)
                && self_.widgets.prj_tree_menu.is_sensitive()
                && self_.widgets.project_tree.model().is_some() {
                    self_.widgets.prj_tree_menu.set_attach_widget(Some(&self_.widgets.project_tree));
                    self_.widgets.prj_tree_menu.popup_easy(evt.button(), evt.time());
                    if self_.widgets.project_tree.selection().count_selected_rows() > 1 {
                        return glib::Propagation::Stop;
                    }
                }
                glib::Propagation::Proceed
            })
        );

        self.widgets.project_tree.connect_destroy(
            clone!{ @weak self as self_ => move |_| {
                self_.widgets.prj_tree_menu.unparent();
            }}
        );

        self.widgets.project_tree.selection().connect_changed(
            clone!{ @weak self as self_ => move |_| {
                if !self_.prj_tree_is_building.get() {
                    self_.enable_actions();
                    self_.preview_selected_file();
                }
            }}
        );

        self.widgets.mi_dark_theme.connect_activate(
            clone!(@weak self as self_ => move |mi| {
                if mi.is_active() {
                    self_.action_dark_theme();
                }
            })
        );

        self.widgets.mi_light_theme.connect_activate(
            clone!(@weak self as self_ => move |mi| {
                if mi.is_active() {
                    self_.action_light_theme();
                }
            })
        );

        self.widgets.mi_cpu_load_min.connect_activate(
            clone!(@weak self as self_ => move |mi| {
                if mi.is_active() {
                    self_.config.borrow_mut().cpu_load = CpuLoad::OneThread;
                }
            })
        );

        self.widgets.mi_cpu_load_half.connect_activate(
            clone!(@weak self as self_ => move |mi| {
                if mi.is_active() {
                    self_.config.borrow_mut().cpu_load = CpuLoad::HalfCPUs;
                }
            })
        );

        self.widgets.mi_cpu_load_max.connect_activate(
            clone!(@weak self as self_ => move |mi| {
                if mi.is_active() {
                    self_.config.borrow_mut().cpu_load = CpuLoad::AllCPUs;
                }
            })
        );

        self.widgets.preview_img_scale.connect_changed(
            clone!(@weak self as self_ => move |cb| {
                self_.config.borrow_mut().preview_scale = match cb.active() {
                    Some(0) => ImgScale::Original,
                    Some(1) => ImgScale::FitWindow,
                    _       => return,
                };
                self_.preview_image_after_change_view_opts(false);
            })
        );

        self.widgets.preview_auto_min.connect_clicked(
            clone!(@weak self as self_ => move |chb| {
                self_.config.borrow_mut().preview_auto_min = chb.is_active();
                self_.preview_image_after_change_view_opts(true);
            })
        );

        self.widgets.preview_auto_wb.connect_clicked(
            clone!(@weak self as self_ => move |chb| {
                self_.config.borrow_mut().preview_auto_wb = chb.is_active();
                self_.preview_image_after_change_view_opts(true);
            })
        );

        self.widgets.preview_img_gamma.connect_change_value(
            clone!(@weak self as self_ => @default-return glib::Propagation::Proceed, move |_, _, value| {
                self_.config.borrow_mut().preview_gamma = value as f32;
                self_.preview_image_after_change_view_opts(false);
                glib::Propagation::Proceed
            })
        );

        self.widgets.window.connect_delete_event(
            clone!(@weak self as self_ => @default-return glib::Propagation::Proceed, move |_, _| {
                if self_.trying_to_close.get() {
                    return if self_.tasks_count.get() != 0 {
                        glib::Propagation::Stop
                    } else {
                        glib::Propagation::Proceed
                    };
                }

                let can_close = self_.ask_user_to_save_project();

                if can_close && self_.tasks_count.get() != 0 {
                    self_.trying_to_close.set(true);
                    self_.global_cancel_flag.store(true, Ordering::Relaxed);
                    return glib::Propagation::Stop;
                }

                return if can_close {
                    glib::Propagation::Proceed
                } else {
                    glib::Propagation::Stop
                };
            })
        );

        self.widgets.window.connect_hide(
            clone!(@weak self as self_ => move |_| {
                self_.assign_config();
                let _ = self_.config.borrow().save();
            })
        );

        self.widgets.recent_menu.connect_item_activated(
            clone!(@weak self as self_ => move |item| {
                let file_name = item
                    .current_item()
                    .and_then(|info| info.uri())
                    .and_then(|uri| gio::File::for_uri(&uri).path());
                if let Some(file_name) = file_name {
                    let can_open = self_.ask_user_to_save_project();
                    if can_open {
                        self_.open_project(&file_name);
                    }
                }
            })
        );

        self.widgets.eb_preview.connect_button_press_event(
            clone!(@weak self as self_ => @default-return glib::Propagation::Proceed, move |_, evt| {
                if evt.button() == gtk::gdk::ffi::GDK_BUTTON_PRIMARY as u32 {
                    let hadjustment = self_.widgets.scr_preview_image.hadjustment();
                    let vadjustment = self_.widgets.scr_preview_image.vadjustment();
                    *self_.preview_scroll_pos.borrow_mut() = Some((
                        evt.root(),
                        (hadjustment.value(), vadjustment.value())
                    ));
                }
                glib::Propagation::Proceed
            })
        );

        self.widgets.eb_preview.connect_button_release_event(
            clone!(@weak self as self_ => @default-return glib::Propagation::Proceed, move |_, evt| {
                if evt.button() == gtk::gdk::ffi::GDK_BUTTON_PRIMARY as u32 {
                    *self_.preview_scroll_pos.borrow_mut() = None;
                }
                glib::Propagation::Proceed
            })
        );

        self.widgets.eb_preview.connect_motion_notify_event(
            clone!(@weak self as self_ => @default-return glib::Propagation::Proceed, move |_, evt| {
                const SCROLL_SPEED: f64 = 2.0;
                if let Some((start_mouse_pos, start_scroll_pos)) = &*self_.preview_scroll_pos.borrow() {
                    let new_pos = evt.root();
                    let move_x = new_pos.0 - start_mouse_pos.0;
                    let move_y = new_pos.1 - start_mouse_pos.1;
                    let hadjustment = self_.widgets.scr_preview_image.hadjustment();
                    hadjustment.set_value(start_scroll_pos.0 - SCROLL_SPEED*move_x);
                    let vadjustment = self_.widgets.scr_preview_image.vadjustment();
                    vadjustment.set_value(start_scroll_pos.1 - SCROLL_SPEED*move_y);
                }
                glib::Propagation::Proceed
            })
        );

        let window = &self.widgets.window;
        connect_action(window, self, "new_project",            Self::action_new_project);
        connect_action(window, self, "open_project",           Self::action_open_project);
        connect_action(window, self, "save_project_as",        Self::action_save_project_as);
        connect_action(window, self, "save_project",           Self::action_save_project);
        connect_action(window, self, "exit",                   Self::action_exit);
        connect_action(window, self, "add_light_files",        Self::action_add_light_files);
        connect_action(window, self, "add_dark_files",         Self::action_add_dark_files);
        connect_action(window, self, "add_flat_files",         Self::action_add_flat_files);
        connect_action(window, self, "add_bias_files",         Self::action_add_bias_files);
        connect_action(window, self, "new_group",              Self::action_new_group);
        connect_action(window, self, "delete_item",            Self::action_delete_item);
        connect_action(window, self, "use_as_ref_image",       Self::action_use_as_reference_image);
        connect_action(window, self, "item_properties",        Self::action_item_properties);
        connect_action(window, self, "register_light_files",   Self::action_register);
        connect_action(window, self, "stack_light_files",      Self::action_stack);
        connect_action(window, self, "project_options",        Self::action_project_options);
        connect_action(window, self, "light_theme",            Self::action_light_theme);
        connect_action(window, self, "dark_theme",             Self::action_dark_theme);
        connect_action(window, self, "cleanup_light_files",    Self::action_cleanup_light_files);
        connect_action(window, self, "change_file_to_light",   Self::action_change_file_to_light);
        connect_action(window, self, "change_file_to_dark",    Self::action_change_file_to_dark);
        connect_action(window, self, "change_file_to_flat",    Self::action_change_file_to_flat);
        connect_action(window, self, "change_file_to_bias",    Self::action_change_file_to_bias);
        connect_action(window, self, "move_file_to_group",     Self::action_move_file_to_group);
        connect_action(window, self, "check_all_files",        Self::action_check_all_files);
        connect_action(window, self, "uncheck_all_files",      Self::action_uncheck_all_files);
        connect_action(window, self, "check_selected_files",   Self::action_check_selected_files);
        connect_action(window, self, "uncheck_selected_files", Self::action_uncheck_selected_files);
        connect_action(window, self, "about",                  Self::action_about);
        connect_action(window, self, "project_columns",        Self::action_project_columns);
        connect_action(window, self, "assign_ref_light_image", Self::action_assign_ref_light_image);

    }

    fn ask_user_to_save_project(self: &Rc<Self>) -> bool {
        if !self.project.borrow().changed() { return true; }

        let dialog = gtk::MessageDialog::builder()
            .transient_for(&self.widgets.window)
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
                self.action_save_project();
                self.project.borrow_mut().reset_changed_flag();
                true
            },
            gtk::ResponseType::No => {
                self.project.borrow_mut().reset_changed_flag();
                true
            },
            _ => {
                false
            },
        }
    }

    fn update_project_tree(self: &Rc<Self>) {
        if self.prj_tree_is_building.get() {
            return;
        }

        self.prj_tree_is_building.set(true);

        let project = self.project.borrow_mut();

        // project root
        let tree_store = match self.widgets.project_tree.model() {
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
                    (ColIdx::Icon as u32, &self.icons.photo),
                ]);

                let sorted_model = gtk::TreeModelSort::new(&result);

                self.widgets.project_tree.set_model(Some(&sorted_model));
                result
            },
        };
        let project_iter = tree_store.iter_first().unwrap();

        let project_file_path = project.file_name()
            .as_ref()
            .and_then(|v|v.parent().and_then(|p| p.to_str()))
            .unwrap_or("");

        tree_store.set(&project_iter, &[
            (ColIdx::FileName as u32, &Self::get_project_title(&project, true)),
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
                (ColIdx::Icon        as u32, &self.icons.folder),
                (ColIdx::Guid        as u32, &group.uuid()),
                (ColIdx::CheckboxVis as u32, &true),
            ]);
            for _ in 0..4 {
                tree_store.insert_with_values(
                    Some(&iter), None,
                    &[(ColIdx::Icon as u32, &self.icons.folder)]
                );
            }

            self.widgets.project_tree.expand_to_path(&tree_store.path(&iter).unwrap());
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
                                let icon = if is_ref_file { &self.icons.ref_image } else { &self.icons.image };

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

        self.prj_tree_is_building.set(false);
    }

    fn update_project_name_and_time_in_gui(self: &Rc<Self>) {
        let app_descr_text = {
            let transl_descr = gettext("APP_DESCRIPTION");
            if transl_descr == "APP_DESCRIPTION" {
                env!("CARGO_PKG_DESCRIPTION").to_string()
            } else {
                transl_descr
            }
        };
        self.widgets.window.set_title(&format!(
            "[{}] - Electra Stacking - {} v{}",
            Self::get_project_title(&self.project.borrow(), false),
            app_descr_text,
            env!("CARGO_PKG_VERSION"),
        ));
    }

    fn enable_actions(self: &Rc<Self>) {
        let selection = self.get_current_selection();
        let is_processing = self.tasks_count.get() != 0;
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
        enable_action(&self.widgets.window, "item_properties", support_item_properties && !is_processing);
        enable_action(&self.widgets.window, "delete_item", support_delete && !is_processing);
        enable_action(&self.widgets.window, "use_as_ref_image", support_use_as_ref_image && !is_processing);
        enable_action(&self.widgets.window, "move_file_to_group", is_file && !is_processing);
        self.widgets.mi_change_file_type.set_sensitive(is_file);
        enable_action(
            &self.widgets.window,
            "change_file_to_light",
            selection.file_type != Some(ProjectFileType::Light) && !is_processing
        );
        enable_action(
            &self.widgets.window,
            "change_file_to_dark",
            selection.file_type != Some(ProjectFileType::Dark) && !is_processing
        );
        enable_action(
            &self.widgets.window,
            "change_file_to_flat",
            selection.file_type != Some(ProjectFileType::Flat) && !is_processing
        );
        enable_action(
            &self.widgets.window,
            "change_file_to_bias",
            selection.file_type != Some(ProjectFileType::Bias) && !is_processing
        );
        enable_action(&self.widgets.window, "check_all_files", (is_file || is_file_type) && !is_processing);
        enable_action(&self.widgets.window, "uncheck_all_files", (is_file || is_file_type) && !is_processing);
        enable_action(&self.widgets.window, "check_selected_files", is_file && !is_processing);
        enable_action(&self.widgets.window, "uncheck_selected_files", is_file && !is_processing);

        enable_action(&self.widgets.window, "new_project", !is_processing);
        enable_action(&self.widgets.window, "open_project", !is_processing);
        enable_action(&self.widgets.window, "add_light_files", !is_processing);
        enable_action(&self.widgets.window, "add_dark_files", !is_processing);
        enable_action(&self.widgets.window, "add_flat_files", !is_processing);
        enable_action(&self.widgets.window, "add_bias_files", !is_processing);
        enable_action(&self.widgets.window, "new_group", !is_processing);
        enable_action(&self.widgets.window, "register_light_files", !is_processing);
        enable_action(&self.widgets.window, "stack_light_files", !is_processing);
        enable_action(&self.widgets.window, "project_options", !is_processing);
        enable_action(&self.widgets.window, "cleanup_light_files", !is_processing);

        self.widgets.recent_menu.set_sensitive(!is_processing);
        self.widgets.mi_cpu_load_min.set_sensitive(!is_processing);
        self.widgets.mi_cpu_load_half.set_sensitive(!is_processing);
        self.widgets.mi_cpu_load_max.set_sensitive(!is_processing);
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
        self:     &Rc<Self>,
        exec_fun: ExecFun,
        ok_fun:   OkFun
    ) where
        R:       Sized + Send + 'static,
        ExecFun: Fn(&ProgressTs, &IsCancelledFun) -> anyhow::Result<R> + Send + 'static,
        OkFun:   Fn(&Rc<Self>, R) + 'static
    {
        enum UiMessage<R: Sized> {
            ProgressStage{ text: String },
            ProgressPercent{ text: String, percent: usize },
            Finished(R),
            Error(String)
        }

        self.tasks_count.set(self.tasks_count.get() + 1);
        let cancel_flag = Arc::new(AtomicBool::new(false));

        let builder = gtk::Builder::from_string(include_str!("ui/progress_box.ui"));
        let progress_box = builder.object::<gtk::Grid>("progress_box").unwrap();
        let progress_text = builder.object::<gtk::Label>("progress_text").unwrap();
        let progress_bar = builder.object::<gtk::ProgressBar>("progress_bar").unwrap();
        let cancel_btn = builder.object::<gtk::Button>("cancel_btn").unwrap();

        self.widgets.progress_box.add(&progress_box);

        cancel_btn.connect_clicked(clone!(@strong cancel_flag => move |_| {
            cancel_flag.store(true, Ordering::Relaxed);
        }));

        let global_cancel_flag = Arc::clone(&self.global_cancel_flag);
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

        self.enable_actions();
        glib::spawn_future_local(clone!(@strong self as self_ => async move {
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
                        self_.tasks_count.set(self_.tasks_count.get() - 1);
                        if self_.trying_to_close.get() {
                            self_.widgets.window.close();
                        } else {
                            self_.enable_actions();
                            ok_fun(&self_, reg_info);
                        }
                        self_.widgets.progress_box.remove(&progress_box);
                        break;
                    },
                    UiMessage::Error(error) => {
                        self_.tasks_count.set(self_.tasks_count.get() - 1);
                        if self_.trying_to_close.get() {
                            self_.widgets.window.close();
                        } else {
                            self_.enable_actions();
                            show_error_message(&self_.widgets.window, &gettext("Error"), &error);
                        }
                        self_.widgets.progress_box.remove(&progress_box);
                        break;
                    },
                }

            }
        }));
    }

    fn handler_project_tree_checked_changed(
        self:        &Rc<Self>,
        sorted_path: gtk::TreePath
    ) {
        let sorted_model = self.widgets.project_tree
            .model().unwrap()
            .downcast::<gtk::TreeModelSort>().unwrap();

        let path = sorted_model
            .convert_path_to_child_path(&sorted_path)
            .unwrap();

        let item = Self::get_selection_for_path(&path);

        match item {
            SelectedItem {
                item_type: SelItemType::Group,
                group_idx: Some(group_idx),
                ..
            } => {
                let mut project = self.project.borrow_mut();
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
                let mut project = self.project.borrow_mut();
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

        self.update_project_tree();
        self.update_project_name_and_time_in_gui();
    }

    // widgets -> config
    fn assign_config(self: &Rc<Self>) {
        let mut config = self.config.borrow_mut();

        config.prj_tree_width = self.widgets.prj_img_paned.position();

        if self.widgets.mi_dark_theme.is_active() {
            config.theme = Theme::Dark;
        } else if self.widgets.mi_light_theme.is_active() {
            config.theme = Theme::Light;
        }

        let tree_columns = get_prj_tree_store_columns();

        config.prj_cols.clear();
        for (idx, col) in self.widgets.project_tree.columns().iter().enumerate() {
            config.prj_cols.insert(
                tree_columns[idx].1.to_string(),
                PrjTreeCol {
                    width: col.width(),
                    visible: col.is_visible(),
                    pos: -1,
                }
            );
        }

        let (width, height) = self.widgets.window.size();
        config.main_win_width = width;
        config.main_win_height = height;
        config.main_win_maximized = self.widgets.window.is_maximized();
    }

    // config -> widgets
    fn apply_config(self: &Rc<Self>) {
        let config = self.config.borrow();

        match config.theme {
            Theme::Dark => {
                self.action_dark_theme();
                self.widgets.mi_dark_theme.set_active(true);
            },
            Theme::Light => {
                self.action_light_theme();
                self.widgets.mi_light_theme.set_active(true);
            },
            Theme::Other(_) => (),
        }

        match config.cpu_load {
            CpuLoad::OneThread =>
                self.widgets.mi_cpu_load_min.set_active(true),
            CpuLoad::HalfCPUs =>
                self.widgets.mi_cpu_load_half.set_active(true),
            CpuLoad::AllCPUs =>
                self.widgets.mi_cpu_load_max.set_active(true),
            _ => {},
        };

        if config.prj_tree_width != -1 {
            self.widgets.prj_img_paned.set_position(config.prj_tree_width);
        }

        let tree_columns = get_prj_tree_store_columns();
        for i in 0..self.widgets.project_tree.n_columns() {
            let tree_col = self.widgets.project_tree.column(i as i32).unwrap();
            let id = tree_columns[i as usize].1;
            if let Some(col_conf) = config.prj_cols.get(id) {
                if col_conf.width > 0 {
                    tree_col.set_fixed_width(col_conf.width);
                }
                tree_col.set_visible(col_conf.visible);
            }
        }

        if config.main_win_width != -1 && config.main_win_height != -1 {
            self.widgets.window.resize(config.main_win_width, config.main_win_height);
        }

        if config.main_win_maximized {
            self.widgets.window.maximize();
        }

        match config.preview_scale {
            ImgScale::Original =>
                self.widgets.preview_img_scale.set_active(Some(0)),
            ImgScale::FitWindow =>
                self.widgets.preview_img_scale.set_active(Some(1)),
        }

        self.widgets.preview_auto_min.set_active(config.preview_auto_min);
        self.widgets.preview_auto_wb.set_active(config.preview_auto_wb);
        self.widgets.preview_img_gamma.set_value(config.preview_gamma as f64);
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

    fn get_current_selection(self: &Rc<Self>) -> SelectedItem {
        let model = match self.widgets.project_tree.model() {
            Some(model) =>
                model.downcast::<gtk::TreeModelSort>().unwrap(),
            None =>
                return SelectedItem::new_empty(),
        };

        let items = self.widgets.project_tree
            .selection()
            .selected_rows().0
            .iter()
            .filter_map(|path| model.convert_path_to_child_path(path))
            .map(|path| Self::get_selection_for_path(&path))
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

    fn action_new_project(self: &Rc<Self>) {
        let can_create_new_project = self.ask_user_to_save_project();
        if !can_create_new_project { return; }
        let mut new_project = Project::default();
        new_project.make_default();
        *self.project.borrow_mut() = new_project;
        self.update_project_tree();
        log::info!("New project created");
    }

    fn action_open_project(self: &Rc<Self>) {
        let can_opened = self.ask_user_to_save_project();
        if !can_opened { return; }

        let ff = Self::create_file_filter_for_project();
        let fc = gtk::FileChooserDialog::builder()
            .action(gtk::FileChooserAction::Open)
            .title(&gettext("Select project file to open"))
            .filter(&ff)
            .modal(true)
            .transient_for(&self.widgets.window)
            .build();

        fc.set_current_folder(self.config.borrow().last_path.clone());

        add_ok_and_cancel_buttons(
            fc.upcast_ref(),
            &gettext("_Open"), gtk::ResponseType::Accept,
            &gettext("_Cancel"), gtk::ResponseType::Cancel
        );

        fc.connect_response(clone!(@strong self as self_ => move |file_chooser, response| {
            if response == gtk::ResponseType::Accept {
                let file_name = file_chooser.file().expect("Can't get file_name");
                let path = file_name.path().unwrap();

                if let Some(cur_folder) = file_chooser.current_folder() {
                    self_.config.borrow_mut().last_path = cur_folder;
                }

                self_.open_project(&path);
            }
            file_chooser.close();
        }));

        fc.show();
    }

    fn open_project(self: &Rc<Self>, path: &Path) {
        let res = self.project.borrow_mut().load(path);
        if let Err(err) = res {
            show_error_message(&self.widgets.window, &gettext("Error"), &err.to_string());
            log::error!("'{}' during opening project", err.to_string());
        } else {
            self.update_project_tree();
            self.update_project_name_and_time_in_gui();
            log::info!("Project {} opened", path.to_str().unwrap_or(""));
        }
    }

    fn action_save_project(self: &Rc<Self>) {
        let file_name = self.project.borrow().file_name().clone();
        if let Some(file_name) = file_name {
            self.save_project(&file_name);
        } else {
            self.action_save_project_as();
        }
    }

    fn action_save_project_as(self: &Rc<Self>) {
        let ff = Self::create_file_filter_for_project();
        let fc = gtk::FileChooserDialog::builder()
            .action(gtk::FileChooserAction::Save)
            .title(&gettext("Select project file to save"))
            .filter(&ff)
            .modal(true)
            .transient_for(&self.widgets.window)
            .build();

        add_ok_and_cancel_buttons(
            fc.upcast_ref(),
            &gettext("_Save"), gtk::ResponseType::Accept,
            &gettext("_Cancel"), gtk::ResponseType::Cancel
        );


        if let Some(file_name) = self.project.borrow().file_name() {
            let _ = fc.set_file(&gio::File::for_path(file_name));
        } else {
            fc.set_current_folder(self.config.borrow().last_path.clone());
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
                self.config.borrow_mut().last_path = cur_folder;
            }

            let mut new_conf = self.project.borrow().config().clone();
            new_conf.name = Some(project_name);
            self.project.borrow_mut().set_config(new_conf);

            let ok = self.save_project(&path);
            if !ok { return; }

            self.update_project_tree();
            self.update_project_name_and_time_in_gui();
        }
    }

    fn save_project(self: &Rc<Self>, file_name: &Path) -> bool {
        let res = self.project.borrow_mut().save(file_name);
        if let Err(err) = res {
            show_error_message(&self.widgets.window, &gettext("Error"), &err.to_string());
            log::error!("'{}' during saving project", err.to_string());
            false
        } else {
            log::info!("Project {} saved", file_name.to_str().unwrap_or(""));
            self.update_project_tree();
            self.update_project_name_and_time_in_gui();
            true
        }
    }

    fn action_exit(self: &Rc<Self>) {
        log::info!("Exit called");
        self.widgets.window.close();
    }

    fn action_add_light_files(self: &Rc<Self>) {
        self.select_and_add_files_into_project(
            ProjectFileType::Light,
            gettext("Select LIGHT files"),
            gettext("Add light files"),
        );
    }

    fn action_add_dark_files(self: &Rc<Self>) {
        self.select_and_add_files_into_project(
            ProjectFileType::Dark,
            gettext("Select DARK files"),
            gettext("Add dark files"),
        );
    }

    fn action_add_flat_files(self: &Rc<Self>) {
        self.select_and_add_files_into_project(
            ProjectFileType::Flat,
            gettext("Select FLAT files"),
            gettext("Add flat files"),
        );
    }

    fn action_add_bias_files(self: &Rc<Self>) {
        self.select_and_add_files_into_project(
            ProjectFileType::Bias,
            gettext("Select BIAS files"),
            gettext("Add bias files"),
        );
    }

    fn select_and_add_files_into_project(
        self:          &Rc<Self>,
        file_type:        ProjectFileType,
        files_dialog_cap: String,
        select_group_cap: String,
    ) {
        let fc = self.create_src_file_select_dialog(
            files_dialog_cap,
            file_type == ProjectFileType::Light
        );
        fc.connect_response(clone!(@strong self as self_ => move |file_chooser, response| {
            if response == gtk::ResponseType::Accept {
                let group_index = self_.get_active_group_index(&select_group_cap).
                    or_else(|| {
                        if self_.project.borrow().groups().is_empty() {
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
                    self_.add_files_into_project(
                        &user_selected_files,
                        group_index,
                        file_type
                    );
                }
                if let Some(cur_folder) = file_chooser.current_folder() {
                    self_.config.borrow_mut().last_path = cur_folder;
                }
            }
            file_chooser.close();
        }));
        fc.show();
    }

    fn create_src_file_select_dialog(
        self:        &Rc<Self>,
        title:       String,
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
            .transient_for(&self.widgets.window)
            .select_multiple(true)
            .build();

        fc.set_current_folder(self.config.borrow().last_path.clone());

        add_ok_and_cancel_buttons(
            fc.upcast_ref(),
            &gettext("_Open"), gtk::ResponseType::Accept,
            &gettext("_Cancel"), gtk::ResponseType::Cancel
        );

        fc
    }

    fn handler_files_dropped(self: &Rc<Self>, sd: &gtk::SelectionData) {
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

        let ask_user_to_select_types_and_add_files = |files: Vec<PathBuf>| {
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

            let project = self.project.borrow();
            for (idx, group) in project.groups().iter().enumerate() {
                cb_group.append(None, group.name(idx).as_str());
            }
            cb_group.set_sensitive(project.groups().len() > 1);
            let selection = self.get_current_selection();
            cb_group.set_active(Some(selection.group_idx.unwrap_or(0) as u32));

            dialog.set_transient_for(Some(&self.widgets.window));
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

            dialog.connect_response(clone!(@strong self as self_ => move |dlg, resp| {
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
                    self_.add_files_into_project(
                        &files,
                        cb_group.active().unwrap_or(0_u32) as usize,
                        file_type
                    );
                }
                dlg.close();
            }));

            set_dialog_default_button(&dialog);
            dialog.show();
        };

        let mut files_list = Vec::new();
        for file in sd.uris() {
            let file_name = gio::File::for_uri(&file).path();
            if let Some(file_name) = file_name {
                add_file_to_list(file_name, &mut files_list);
            }
        }
        ask_user_to_select_types_and_add_files(files_list);
    }

    fn add_files_into_project(
        self:             &Rc<Self>,
        file_names:       &Vec<PathBuf>,
        group_iter_index: usize,
        file_type:        ProjectFileType
    ) {
        let mut file_names = file_names.clone();
        let project = self.project.borrow();
        let group = project.groups().get(group_iter_index);
        if let Some(group) = group {
            let files = &group.get_file_list_by_type(file_type);
            files.retain_files_if_they_are_not_here(&mut file_names);
        }
        drop(project);

        self.exec_and_show_progress(
            move |progress, cancel_flag| {
                load_src_file_info_for_files(&file_names, cancel_flag, progress)
            },
            move |self_, result| {
                let mut project = self_.project.borrow_mut();
                project.add_default_group_if_empty();
                let group = project.group_by_index_mut(group_iter_index);
                let files = &mut group.file_list_by_type_mut(file_type);
                log::info!("Added {} files", result.len());
                files.add_files_from_src_file_info(result);
                drop(project);
                self_.update_project_tree();
                self_.update_project_name_and_time_in_gui();
            }
        );
    }


    fn action_register(self: &Rc<Self>) {
        if self.project.borrow().total_light_files_count() == 0 {
            return;
        }
        log::info!("Registering light files started");
        let project_json = self.project.borrow().to_json_string();
        let cpu_load = self.config.borrow().cpu_load;
        self.exec_and_show_progress(
            move |progress, is_canceled| {
                let project = Project::from_json_string(&project_json);
                project.register_light_files(progress, is_canceled, cpu_load)
            },
            move |self_, result| {
                self_.project.borrow_mut().update_light_files_reg_info(result);
                self_.update_project_tree();
                self_.update_project_name_and_time_in_gui();
            }
        );
    }

    fn action_light_theme(self: &Rc<Self>) {
        let settings = gtk::Settings::default().unwrap();
        settings.set_property("gtk-application-prefer-dark-theme", false);
        log::info!("Light theme selected");
    }

    fn action_dark_theme(self: &Rc<Self>) {
        let settings = gtk::Settings::default().unwrap();
        settings.set_property("gtk-application-prefer-dark-theme", true);
        log::info!("Dark theme selected");
    }

    fn preview_selected_file(self: &Rc<Self>) {
        let (file_name, mode) = {
            match self.get_current_selection() {
                SelectedItem {
                    item_type: SelItemType::File,
                    group_idx: Some(group_idx),
                    file_type: Some(file_type),
                    files,
                    ..
                } if !files.is_empty() => {
                    let project = self.project.borrow();
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
                    let project = self.project.borrow();
                    match project.get_result_file_name() {
                        Ok(file_name) => (file_name, PreviewFileMode::ResultFile),
                        Err(_) => return,
                    }
                },

                _ => return,
            }
        };

        if *self.last_preview_file.borrow() == file_name {
            return;
        }

        self.preview_image_file(&file_name, mode);
    }

    fn preview_image_after_change_view_opts(
        self:          &Rc<Self>,
        recalc_params: bool
    ) {
        let image = self.prev_preview_img.borrow();
        if image.is_empty() { return; }
        let config = self.config.borrow();
        if recalc_params {
            *self.prev_preview_params.borrow_mut() =
                image.calc_to_bytes_params(
                    config.preview_auto_min,
                    config.preview_auto_wb
                );
        }
        let bytes = image.to_rgb_bytes(
            &self.prev_preview_params.borrow(),
            config.preview_gamma.min(20.0),
        );
        self.show_preview_image(
            bytes,
            image.width() as i32,
            image.height() as i32,
            config.preview_scale
        );
    }

    fn preview_image_file(
        self:      &Rc<Self>,
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
        if let Some(flag) = &*self.prev_preview_cancel_flags.borrow() {
            flag.store(true, Ordering::Relaxed);
        }

        let (sender, receiver) = async_channel::unbounded();

        glib::spawn_future_local(clone!(@strong self as self_ => async move {
            while let Ok(message) = receiver.recv().await {
                match message {
                    UiMessage::Image { image, file_name } => {
                        let config = self_.config.borrow();

                        // show preview image
                        let params = image.image.calc_to_bytes_params(
                            config.preview_auto_min,
                            config.preview_auto_wb
                        );

                        let bytes = image.image.to_rgb_bytes(&params, config.preview_gamma);
                        self_.show_preview_image(
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

                            self_.widgets.img_star.set_pixbuf(Some(&pixbuf));
                            self_.widgets.scr_star.set_width_request(star_stat.common_stars_img.width() as i32 + 4);
                        } else {
                            self_.widgets.img_star.set_pixbuf(None);
                        }

                        self_.widgets.preview_fname.set_label(file_name.to_str().unwrap_or(""));
                        *self_.last_preview_file.borrow_mut() = file_name;
                        *self_.prev_preview_img.borrow_mut() = image.image;
                        *self_.prev_preview_params.borrow_mut() = params;
                        self_.widgets.preview_ctrls_box.set_sensitive(true);
                    },

                    UiMessage::Error(text) => {
                        self_.widgets.preview_ctrls_box.set_sensitive(true);
                        self_.widgets.preview_fname.set_label(&text);
                        self_.widgets.img_preview.clear();
                        self_.prev_preview_img.borrow_mut().clear();
                        self_.last_preview_file.borrow_mut().clear();
                    },
                }
                break; // while
            }
        }));

        let cancel_flag = Arc::new(AtomicBool::new(false));
        *self.prev_preview_cancel_flags.borrow_mut() = Some(cancel_flag.clone());
        self.widgets.preview_fname.set_label("???");
        self.widgets.preview_ctrls_box.set_sensitive(false);

        let file_name = file_name.to_path_buf();
        let project = self.project.borrow();
        let bin = match project.config().image_size {
            ImageSize::Bin2x2 =>
                if mode == PreviewFileMode::ResultFile {1} else {2},
            ImageSize::Original =>
                1,
        };

        let mut raw_params = project.config().raw_params.clone();

        self.preview_tp.spawn(clone!(@strong cancel_flag => move || {
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
        self:       &Rc<Self>,
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
            let parent = self.widgets.img_preview
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

        self.widgets.img_preview.set_pixbuf(Some(&pixbuf));
    }

    fn action_new_group(self: &Rc<Self>) {
        let def_group_options = GroupOptions::default();
        let dialog = self.group_options_dialog(
            gettext("Add new group"),
            def_group_options,
            clone!(@strong self as self_ => move |group_options| {
                self_.project.borrow_mut().add_new_group(group_options);
                self_.update_project_tree();
                log::info!("New group created");
            })
        );

        set_dialog_default_button(&dialog);
        dialog.show();
    }

    fn action_delete_item(self: &Rc<Self>) {
        let selection = self.get_current_selection();
        let project = self.project.borrow();
        match selection {
            SelectedItem {
                item_type: SelItemType::Group,
                group_idx: Some(group_idx),
                ..
            } => {
                let group = &project.groups()[group_idx];
                let dialog = Self::confirm_dialog(
                    &self.widgets.window,
                    transl_and_replace(
                        "Remove group '{group}' from project?",
                        &[("{group}", group.name(group_idx))]
                    ),
                    clone!(@strong self as self_ => move || {
                        let group = self_.project.borrow_mut().remove_group(group_idx);
                        self_.update_project_tree();
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
                let dialog = Self::confirm_dialog(
                    &self.widgets.window,
                    dialog_text,
                    clone!(@strong self as self_ => move || {
                        self_.widgets.project_tree.selection().unselect_all();
                        self_.project.borrow_mut()
                            .group_by_index_mut(group_idx)
                            .file_list_by_type_mut(file_type)
                            .remove_files_by_idx(files.clone());
                        self_.update_project_tree();
                    })
                );
                dialog.show()
            },

            _ => (),
        }
    }

    fn action_item_properties(self: &Rc<Self>) {
        let selection = self.get_current_selection();

        match selection {
            SelectedItem {
                item_type: SelItemType::Group,
                group_idx: Some(group_idx),
                ..
            } => {
                let dialog = self.group_options_dialog(
                    gettext("Group properties"),
                    self.project.borrow().groups()[group_idx].options().clone(),
                    clone!(@strong self as self_ => move |new_options| {
                        let mut project = self_.project.borrow_mut();
                        let group = project.group_by_index_mut(group_idx);
                        log::info!("Group '{}' options changed to {:?}", group.name(group_idx), new_options);
                        group.set_options(new_options);
                        drop(project);
                        self_.update_project_tree();
                    })
                );
                set_dialog_default_button(&dialog);
                dialog.show();
            },

            SelectedItem {item_type: SelItemType::Project, ..} => {
                self.action_project_options();
            },

            _ => (),
        }
    }

    fn group_options_dialog<F: Fn(GroupOptions) + 'static>(
        self:          &Rc<Self>,
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
        dialog.set_transient_for(Some(&self.widgets.window));

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

    fn get_active_group_index(self: &Rc<Self>, title: &str) -> Option<usize> {
        match self.project.borrow().groups().len() {
            0 => return None,
            1 => return Some(0),
            _ => {},
        };

        let selection = self.get_current_selection();
        let cur_group = selection.group_idx;

        let builder = gtk::Builder::from_string(include_str!("ui/select_group_dialog.ui"));
        let dialog = builder.object::<gtk::Dialog>("select_group_dialog").unwrap();
        let groups_list = builder.object::<gtk::ComboBoxText>("groups_list").unwrap();

        dialog.set_title(title);
        dialog.set_transient_for(Some(&self.widgets.window));

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

        for (idx, group) in self.project.borrow().groups().iter().enumerate() {
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

    fn action_project_options(self: &Rc<Self>) {
        let dialog = ProjectOptionsDialog::new(
            Some(&self.widgets.window),
            &self.project
        );
        let self_ = Rc::clone(&self);
        dialog.exec(move || {
            self_.update_project_tree();
            self_.update_project_name_and_time_in_gui();
            *self_.last_preview_file.borrow_mut() = PathBuf::new();
            self_.preview_selected_file();
        });
    }

    fn check_all_light_files_are_registered(self: &Rc<Self>) -> bool {
        let project = self.project.borrow();
        if !project.is_any_used_light_file() {
            return false;
        }
        if !project.is_all_light_files_are_registered() {
            show_error_message(
                &self.widgets.window, &gettext("Error"),
                &gettext("You have execute register light files first!")
            );
            return false;
        }
        true
    }

    fn action_cleanup_light_files(self: &Rc<Self>) {
        if !self.check_all_light_files_are_registered() {
            return;
        }

        let builder = gtk::Builder::from_string(include_str!("ui/cleanup_dialog.ui"));
        let dialog = builder.object::<gtk::Dialog>("cleanup_dialog").unwrap();
        let chbt_check_before = builder.object::<gtk::CheckButton>("check_before").unwrap();

        let project = self.project.borrow();
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

        dialog.set_transient_for(Some(&self.widgets.window));
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

        dialog.connect_response(clone!(@strong self as self_ => move |dialog, response| {
            if response == gtk::ResponseType::Ok {
                let mut project = self_.project.borrow_mut();

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

                let result = self_.project.borrow_mut().cleanup_light_files();
                match result {
                    Ok(cleaned_up_count) => {
                        let total_files = self_.project.borrow().total_light_files_count();

                        let message = transl_and_replace(
                            "Cleaned up {cleaned} files of {total} ({percent}%)", &[
                            ("{cleaned}", cleaned_up_count.to_string()),
                            ("{total}",   total_files.to_string()),
                            ("{percent}", format!("{:.1}", 100.0 * cleaned_up_count as f64 / total_files as f64)),
                        ]);

                        show_message(
                            &self_.widgets.window,
                            &gettext("Cleanup light files result"),
                            &message,
                            gtk::MessageType::Info,
                        );
                    },
                    Err(error) =>
                        show_error_message(&self_.widgets.window, &gettext("Error"), &error.to_string()),
                }
                self_.update_project_tree();
                self_.update_project_name_and_time_in_gui();
            }
            else {
                dialog.close();
            }
        }));

        set_dialog_default_button(&dialog);
        dialog.show();
    }

    fn ask_user_to_assign_ref_light_image_auto(
        self   :              &Rc<Self>,
        start_stacking_after: bool
    ){
        let builder = gtk::Builder::from_string(include_str!("ui/assign_ref_light_frame.ui"));
        let dialog = builder.object::<gtk::Dialog>("assign_key_light_frame_dialog").unwrap();

        let rb_smallest_stars = builder.object::<gtk::RadioButton>("rb_smallest_stars").unwrap();
        let rb_roundest_stars = builder.object::<gtk::RadioButton>("rb_roundest_stars").unwrap();
        let rb_min_bg = builder.object::<gtk::RadioButton>("rb_min_bg").unwrap();
        let rb_min_noise = builder.object::<gtk::RadioButton>("rb_min_noise").unwrap();

        dialog.set_transient_for(Some(&self.widgets.window));
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

        match self.project.borrow().config().ref_image_auto_mode {
            RefImageAutoMode::SmallestStars => rb_smallest_stars.set_active(true),
            RefImageAutoMode::RoundestStars => rb_roundest_stars.set_active(true),
            RefImageAutoMode::MinBg         => rb_min_bg.set_active(true),
            RefImageAutoMode::NinNoise      => rb_min_noise.set_active(true),
        }

        dialog.connect_response(clone!(@strong self as self_ => move |dialog, response| {
            if response == gtk::ResponseType::Ok {
                let mut project = self_.project.borrow_mut();
                let mut config = project.config().clone();
                config.ref_image_auto_mode =
                    if rb_smallest_stars.is_active()      { RefImageAutoMode::SmallestStars }
                    else if rb_roundest_stars.is_active() { RefImageAutoMode::RoundestStars }
                    else if rb_min_bg.is_active()         { RefImageAutoMode::MinBg }
                    else                                  { RefImageAutoMode::NinNoise };

                project.set_config(config);
                project.assign_ref_light_frame_automatically();

                drop(project);

                self_.update_project_tree();
                self_.update_project_name_and_time_in_gui();

                if start_stacking_after {
                    self_.action_stack();
                }
            }
            dialog.close();
        }));

        set_dialog_default_button(&dialog);
        dialog.show();
    }

    fn action_assign_ref_light_image(self: &Rc<Self>) {
        if !self.check_all_light_files_are_registered() {
            return;
        }

        if !self.project.borrow().is_possible_assign_ref_light_frame_automatically() {
            show_error_message(
                &self.widgets.window, &gettext("Error"),
                &gettext("It is not possible to assign reference light image automatically")
            );
            return;
        }

        self.ask_user_to_assign_ref_light_image_auto(false);
    }

    fn action_stack(self: &Rc<Self>) {
        let project = self.project.borrow();
        if !project.is_any_used_light_file() {
            return;
        }
        if !project.is_ref_image_assigned()
        && project.is_possible_assign_ref_light_frame_automatically() {
            self.ask_user_to_assign_ref_light_image_auto(true);
            return;
        }
        match project.can_exec_stack_light_files() {
            CanExecStackLightsRes::Ok => (),
            CanExecStackLightsRes::NoRefFile => {
                show_error_message(
                    &self.widgets.window, &gettext("Error"),
                    &gettext("Reference image is not defined")
                );
                return;
            },
        }
        log::info!("Stacking light files started");
        let project_json = project.to_json_string();
        let cpu_load = self.config.borrow().cpu_load;
        drop(project);
        self.exec_and_show_progress(
            move|progress, is_canceled| {
                let project = Project::from_json_string(&project_json);
                project.stack_light_files(progress, is_canceled, cpu_load)
            },
            move |self_, result| {
                self_.preview_image_file(&result.file_name, PreviewFileMode::ResultFile);
                show_message(
                    &self_.widgets.window,
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

    fn action_use_as_reference_image(self: &Rc<Self>) {
        if let SelectedItem {
            item_type: SelItemType::File,
            file_type: Some(ProjectFileType::Light),
            group_idx: Some(group_idx),
            files,
        } = self.get_current_selection() {
            if files.len() == 1 {
                let mut project = self.project.borrow_mut();
                let file_name =  project.groups()[group_idx]
                    .light_files.list()[files[0]]
                    .file_name()
                    .clone();
                project.set_ref_image(file_name);
            } else {
                return;
            }

            self.update_project_tree();
        }
    }

    fn action_change_file_to_light(self: &Rc<Self>) {
        self.change_selected_files_type(ProjectFileType::Light);
    }

    fn action_change_file_to_dark(self: &Rc<Self>) {
        self.change_selected_files_type(ProjectFileType::Dark);
    }

    fn action_change_file_to_flat(self: &Rc<Self>) {
        self.change_selected_files_type(ProjectFileType::Flat);
    }

    fn action_change_file_to_bias(self: &Rc<Self>) {
        self.change_selected_files_type(ProjectFileType::Bias);
    }

    fn change_selected_files_type(
        self:     &Rc<Self>,
        new_type: ProjectFileType
    ) {
        let selection = self.get_current_selection();
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
            .transient_for(&self.widgets.window)
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

        dialog.connect_response(clone!(@strong self as self_ => move |dlg, resp| {
            if resp == gtk::ResponseType::Yes {
                let mut project = self_.project.borrow_mut();
                let group = project.group_by_index_mut(selection.group_idx.unwrap());
                group.change_file_types(
                    selection.file_type.unwrap(),
                    new_type,
                    selection.files.clone()
                );
                drop(project);
                self_.update_project_tree();
                self_.update_project_name_and_time_in_gui();
            }
            dlg.close();
        }));

        set_dialog_default_button(&dialog);
        dialog.show();
    }

    fn action_move_file_to_group(self: &Rc<Self>) {
        let selection = self.get_current_selection();
        if selection.item_type != SelItemType::File {
            return;
        }
        let dialog = MoveFileToGroupDialog::new(
            Some(&self.widgets.window),
            &self.project
        );
        let self_ = Rc::clone(&self);

        let ok_fun = move || {
            self_.update_project_tree();
            self_.update_project_name_and_time_in_gui();
        };

        let move_to_group_last_uuid = self.move_to_group_last_uuid.borrow().clone();
        dialog.exec(
            selection,
            &move_to_group_last_uuid,
            ok_fun
        );
    }

    fn action_check_all_files(self: &Rc<Self>) {
        self.check_all_files(true);
    }

    fn action_uncheck_all_files(self: &Rc<Self>) {
        self.check_all_files(false);
    }

    fn check_all_files(self: &Rc<Self>, value: bool) {
        let s = self.get_current_selection();
        let (group_idx, file_type) = match (s.group_idx, s.file_type) {
            (Some(group_idx), Some(file_type)) => (group_idx, file_type),
            _ => return,
        };

        self.project.borrow_mut()
            .group_by_index_mut(group_idx)
            .file_list_by_type_mut(file_type)
            .check_all(value);
        self.update_project_tree();
        self.update_project_name_and_time_in_gui();
    }

    fn action_check_selected_files(self: &Rc<Self>) {
        self.check_selected_files(true);
    }

    fn action_uncheck_selected_files(self: &Rc<Self>) {
        self.check_selected_files(false);
    }

    fn check_selected_files(self: &Rc<Self>, value: bool) {
        let s = self.get_current_selection();
        let (group_idx, file_type, files) = match (s.group_idx, s.file_type, s.files) {
            (Some(group_idx), Some(file_type), files) if !files.is_empty() =>
                (group_idx, file_type, files),
            _ =>
                return,
        };

        self.project.borrow_mut()
            .group_by_index_mut(group_idx)
            .file_list_by_type_mut(file_type)
            .check_by_indices(&files, value);
        self.update_project_tree();
        self.update_project_name_and_time_in_gui();
    }

    fn action_about(self: &Rc<Self>) {
        show_about_dialog(Some(&self.widgets.window));
    }

    fn action_project_columns(self: &Rc<Self>) {
        let dialog = PrjColumnsDialog::new(
            Some(&self.widgets.window),
            &self.widgets.project_tree
        );
        dialog.exec();
    }

}