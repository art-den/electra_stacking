use std::rc::Rc;

use gettextrs::gettext;
use gtk::{prelude::*, glib::clone, glib};

use macros::FromBuilder;

use crate::{gtk_utils::set_dialog_default_button, ui_main::get_prj_tree_store_columns};


pub struct PrjColumnsDialog {
    widgets:      Widgets,
    project_tree: gtk::TreeView,
}

#[derive(FromBuilder)]
struct Widgets {
    dialog:    gtk::Dialog,
    lv_list:   gtk::TreeView,
    btn_close: gtk::Button,
}

impl PrjColumnsDialog {
    pub fn new(parent: Option<&impl IsA<gtk::Window>>, project_tree: &gtk::TreeView) -> Rc<Self> {
        let widgets = Widgets::from_builder_str(include_str!("ui/columns_selector.ui"));
        widgets.dialog.set_transient_for(parent);
        set_dialog_default_button(&widgets.dialog);
        let dialog = widgets.dialog.clone();
        widgets.btn_close.connect_clicked(move |_| dialog.close());
        Rc::new(PrjColumnsDialog {
            widgets,
            project_tree: project_tree.clone(),
        })
    }

    pub fn exec(self: &Rc<Self>) {
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
        self.widgets.lv_list.append_column(&col);
        let tree_columns = get_prj_tree_store_columns();
        for i in 0..self.project_tree.n_columns() {
            let tree_col = self.project_tree.column(i as i32).unwrap();
            let name = tree_columns[i as usize].0;
            let id = tree_columns[i as usize].1;
            model.insert_with_values(None, &[
                (COLUMN_CHECK as u32, &tree_col.is_visible()),
                (COLUMN_NAME as u32, &gettext(name)),
                (COLUMN_ID as u32, &id),
            ]);
        }
        self.widgets.lv_list.set_model(Some(&model));
        cell_check.connect_toggled(clone!(@strong self as self_, @weak model => move |_, path| {
            let values = path.indices();
            let tree_col = self_.project_tree.column(values[0]).unwrap();
            tree_col.set_visible(!tree_col.is_visible());
            let iter = model.iter(&path).unwrap();
            model.set(&iter, &[(COLUMN_CHECK as u32, &tree_col.is_visible())])
        }));
        self.widgets.dialog.show();
    }
}
