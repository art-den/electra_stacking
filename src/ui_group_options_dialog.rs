use std::rc::Rc;

use gettextrs::gettext;
use gtk::{prelude::*, glib::clone};

use macros::FromBuilder;

use crate::{gtk_utils::*, project::GroupOptions};

pub struct GroupOptionsDialog {
    widgets: Widgets,
}

#[derive(FromBuilder)]
pub struct Widgets {
    dialog:          gtk::Dialog,
    rb_default_name: gtk::RadioButton,
    rb_custom_name:  gtk::RadioButton,
    e_name:          gtk::Entry,
}

impl GroupOptionsDialog {
    pub fn new(
        parent: Option<&impl IsA<gtk::Window>>,
        title:  &str,
    ) -> Rc<Self> {
        let widgets = Widgets::from_builder_str(include_str!("ui/group_options_dialog.ui"));
        widgets.dialog.set_title(&title);
        widgets.dialog.set_transient_for(parent);
        add_ok_and_cancel_buttons(
            &widgets.dialog,
            &gettext("_Ok"), gtk::ResponseType::Ok,
            &gettext("_Cancel"), gtk::ResponseType::Cancel,
        );
        set_dialog_default_button(&widgets.dialog);
        let result = Rc::new(Self {widgets});
        result.widgets.rb_custom_name.connect_clicked(
            clone!(@strong result => move |rb| {
                result.widgets.e_name.set_sensitive(rb.is_active());
            })
        );
        result
    }

    pub fn exec(
        self:          &Rc<Self>,
        group_options: GroupOptions,
        ok_fun:        impl Fn(GroupOptions) + 'static,
    ) {
        if let Some(name) = &group_options.name {
            self.widgets.rb_custom_name.set_active(true);
            self.widgets.e_name.set_text(name);
            self.widgets.e_name.set_sensitive(true);
        } else {
            self.widgets.rb_default_name.set_active(true);
            self.widgets.e_name.set_sensitive(false);
        }
        let self_ = Rc::clone(&self);
        self.widgets.dialog.connect_response(move |dialog, response| {
            dialog.close();
            if response == gtk::ResponseType::Ok {
                let mut group_options = group_options.clone();
                group_options.name = if self_.widgets.rb_custom_name.is_active() {
                    Some(self_.widgets.e_name.text().to_string())
                } else {
                    None
                };
                ok_fun(group_options);
            }
        });
        self.widgets.dialog.show();
    }
}