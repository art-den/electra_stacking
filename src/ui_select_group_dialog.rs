use std::{cell::RefCell, rc::Rc};

use gettextrs::gettext;
use gtk::prelude::*;

use macros::FromBuilder;

use crate::{gtk_utils::*, project::Project};

pub struct SelectGroupDialog {
    widgets: Widgets,
    project: Rc<RefCell<Project>>,
}

#[derive(FromBuilder)]
struct Widgets {
    dialog: gtk::Dialog,
    groups_list: gtk::ComboBoxText,
}

impl SelectGroupDialog {
    pub fn new(
        parent:  Option<&impl IsA<gtk::Window>>,
        project: &Rc<RefCell<Project>>,
        title:   &str,
    ) -> Rc<Self> {
        let widgets = Widgets::from_builder_str(include_str!("ui/select_group_dialog.ui"));
        widgets.dialog.set_title(title);
        widgets.dialog.set_transient_for(parent);
        add_ok_and_cancel_buttons(
            &widgets.dialog,
            &gettext("_Ok"), gtk::ResponseType::Ok,
            &gettext("_Cancel"), gtk::ResponseType::Cancel,
        );
        set_dialog_default_button(&widgets.dialog);
        Rc::new(Self {
            widgets,
            project: Rc::clone(project),
        })
    }

    pub fn exec(self: &Rc<Self>, cur_group: Option<usize>) -> Option<usize> {
        for (idx, group) in self.project.borrow().groups().iter().enumerate() {
            self.widgets.groups_list.append(None, &group.name(idx));
        }

        if let Some(cur_group) = cur_group {
            self.widgets.groups_list.set_active(Some(cur_group as u32));
        }

        let resp = self.widgets.dialog.run();
        self.widgets.dialog.close();

        if resp != gtk::ResponseType::Ok {
            return None;
        }

        self.widgets.groups_list.active().map(|v| v as usize)
    }
}
