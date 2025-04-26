use std::{cell::RefCell, rc::Rc};

use gettextrs::gettext;
use gtk::{prelude::*, glib::clone};

use macros::FromBuilder;

use crate::{gtk_utils::*, project::{Project, ProjectFileType}, str_utils::transl_and_replace, ui_main::SelectedItem};

pub struct DndFilesTypeDialog {
    widgets: Widgets,
    project: Rc<RefCell<Project>>,
}

#[derive(FromBuilder)]
struct Widgets {
    dialog: gtk::Dialog,
    l_info: gtk::Label,
    cb_group: gtk::ComboBoxText,
    rb_lights: gtk::RadioButton,
    rb_darks: gtk::RadioButton,
    rb_flats: gtk::RadioButton,
    rb_biases: gtk::RadioButton,
}

impl DndFilesTypeDialog {
    pub fn new(
        parent:  Option<&impl IsA<gtk::Window>>,
        project: &Rc<RefCell<Project>>
    ) -> Rc<Self> {
        let widgets = Widgets::from_builder_str(include_str!("ui/dnd_files_type_dialog.ui"));
        widgets.dialog.set_transient_for(parent);

        add_ok_and_cancel_buttons(
            &widgets.dialog,
            &gettext("_Add"), gtk::ResponseType::Ok,
            &gettext("_Cancel"), gtk::ResponseType::Cancel,
        );

        set_dialog_default_button(&widgets.dialog);

        Rc::new(Self {
            widgets,
            project: Rc::clone(project),
        })
    }

    pub fn exec(
        self:   &Rc<Self>,
        selection: SelectedItem,
        files_count: usize,
        ok_fun: impl Fn(ProjectFileType, usize) + 'static
    ) {
        self.widgets.l_info.set_label(&transl_and_replace(
            "Add {files} file(s) into project?",
            &[("{files}", files_count.to_string())]
        ));

        let project = self.project.borrow();
        for (idx, group) in project.groups().iter().enumerate() {
            self.widgets.cb_group.append(None, group.name(idx).as_str());
        }
        self.widgets.cb_group.set_sensitive(project.groups().len() > 1);
        self.widgets.cb_group.set_active(Some(selection.group_idx.unwrap_or(0) as u32));

        self.widgets.dialog.connect_response(clone!(@strong self as self_ => move |dlg, resp| {
            if resp == gtk::ResponseType::Ok {
                let file_type = if self_.widgets.rb_lights.is_active() {
                    ProjectFileType::Light
                } else if self_.widgets.rb_darks.is_active() {
                    ProjectFileType::Dark
                } else if self_.widgets.rb_flats.is_active() {
                    ProjectFileType::Flat
                } else {
                    ProjectFileType::Bias
                };

                let group_index = self_.widgets.cb_group.active().unwrap_or(0_u32) as usize;

                ok_fun(file_type, group_index);
            }
            dlg.close();
        }));

        self.widgets.dialog.show();

    }
}


/*



*/