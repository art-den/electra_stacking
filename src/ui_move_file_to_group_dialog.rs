use std::{cell::RefCell, rc::Rc};

use gettextrs::gettext;
use gtk::{prelude::*, glib::clone};

use macros::FromBuilder;

use crate::{gtk_utils::{add_ok_and_cancel_buttons, set_dialog_default_button}, project::{GroupOptions, Project}, ui_main::SelectedItem};

pub struct MoveFileToGroupDialog {
    widgets: Widgets,
    project: Rc<RefCell<Project>>,
}

#[derive(FromBuilder)]
struct Widgets {
    dialog:              gtk::Dialog,
    rbtn_existing_group: gtk::RadioButton,
    rbtn_new_group:      gtk::RadioButton,
    cbx_existing_groups: gtk::ComboBoxText,
    e_new_group:         gtk::Entry,
}

impl MoveFileToGroupDialog {
    pub fn new(
        parent:  Option<&impl IsA<gtk::Window>>,
        project: &Rc<RefCell<Project>>
    ) -> Rc<Self> {
        let widgets = Widgets::from_builder_str(include_str!("ui/move_files_to_group_dialog.ui"));

        widgets.dialog.set_transient_for(parent);

        add_ok_and_cancel_buttons(
            &widgets.dialog,
            &gettext("_Ok"), gtk::ResponseType::Ok,
            &gettext("_Cancel"), gtk::ResponseType::Cancel
        );

        set_dialog_default_button(&widgets.dialog);

        Rc::new(MoveFileToGroupDialog{
            widgets,
            project: Rc::clone(project)
        })
    }

    pub fn exec(
        self: &Rc<Self>,
        selection: SelectedItem,
        move_to_group_last_uuid: &str,
        ok_fun: impl Fn() + 'static
    ) {
        let project = self.project.borrow();
        let mut groups_in_cb = Vec::new();
        for (idx, group) in project.groups().iter().enumerate() {
            if Some(idx) != selection.group_idx {
                self.widgets.cbx_existing_groups.append(Some(group.uuid()), &group.name(idx));
                groups_in_cb.push(group.uuid());
            }
        }
        if !move_to_group_last_uuid.is_empty() {
            self.widgets.cbx_existing_groups.set_active_id(Some(&*move_to_group_last_uuid));
        }

        if self.widgets.cbx_existing_groups.active_id().is_none() && groups_in_cb.len() == 1 {
            self.widgets.cbx_existing_groups.set_active_id(Some(groups_in_cb[0]));
        }

        if project.groups().len() <= 1 {
            self.widgets.rbtn_existing_group.set_sensitive(false);
            self.widgets.rbtn_new_group.set_active(true);
        } else {
            self.widgets.rbtn_existing_group.set_active(true);
        }

        drop(project);

        self.widgets.cbx_existing_groups.set_sensitive(self.widgets.rbtn_existing_group.is_active());
        self.widgets.e_new_group.set_sensitive(self.widgets.rbtn_new_group.is_active());

        self.widgets.rbtn_existing_group.connect_clicked(clone!(@strong self as self_ => move |rb| {
            self_.widgets.cbx_existing_groups.set_sensitive(rb.is_active());
        }));

        self.widgets.rbtn_new_group.connect_clicked(clone!(@strong self as self_ => move |rb| {
            self_.widgets.e_new_group.set_sensitive(rb.is_active());
        }));

        self.widgets.dialog.connect_response(clone!(@strong self as self_ => move |dlg, resp| {
            if resp == gtk::ResponseType::Ok {
                let mut project = self_.project.borrow_mut();
                let group_id = if self_.widgets.rbtn_new_group.is_active() {
                    let mut group_options = GroupOptions::default();
                    let new_group_name = self_.widgets.e_new_group.text().to_string().trim().to_string();
                    if !new_group_name.is_empty() {
                        group_options.name = Some(new_group_name);
                    }
                    project.add_new_group(group_options);
                    project.groups().last().unwrap().uuid().to_string()
                } else {
                    match self_.widgets.cbx_existing_groups.active_id() {
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

                ok_fun();
            }
            dlg.close();
        }));

        self.widgets.dialog.show();
    }
}
