use std::{cell::RefCell, rc::Rc};

use gettextrs::gettext;
use gtk::{prelude::*, glib::clone};

use macros::FromBuilder;

use crate::{gtk_utils::*, project::{Project, RefImageAutoMode}};

pub struct AssignRefFrameDialog {
    widgets: Widgets,
    project: Rc<RefCell<Project>>,
}

#[derive(FromBuilder)]
struct Widgets {
    dialog:            gtk::Dialog,
    rb_smallest_stars: gtk::RadioButton,
    rb_roundest_stars: gtk::RadioButton,
    rb_min_bg:         gtk::RadioButton,
    rb_min_noise:      gtk::RadioButton,
}

impl AssignRefFrameDialog {
    pub fn new(
        parent:  Option<&impl IsA<gtk::Window>>,
        project: &Rc<RefCell<Project>>
    ) -> Rc<Self> {
        let widgets = Widgets::from_builder_str(include_str!("ui/assign_ref_light_frame.ui"));
        widgets.dialog.set_transient_for(parent);
        add_ok_and_cancel_buttons(
            &widgets.dialog,
            &gettext("_Assign"), gtk::ResponseType::Ok,
            &gettext("_Cancel"), gtk::ResponseType::Cancel
        );
        set_dialog_default_button(&widgets.dialog);
        Rc::new(Self {
            widgets,
            project: Rc::clone(project)
        })
    }

    pub fn exec(
        self: &Rc<Self>,
        ok_fun: impl Fn() + 'static
    ) {
        match self.project.borrow().config().ref_image_auto_mode {
            RefImageAutoMode::SmallestStars => self.widgets.rb_smallest_stars.set_active(true),
            RefImageAutoMode::RoundestStars => self.widgets.rb_roundest_stars.set_active(true),
            RefImageAutoMode::MinBg         => self.widgets.rb_min_bg.set_active(true),
            RefImageAutoMode::NinNoise      => self.widgets.rb_min_noise.set_active(true),
        }
        self.widgets.dialog.connect_response(
            clone!(@strong self as self_ => move |dialog, response| {
                if response == gtk::ResponseType::Ok {
                    let mut project = self_.project.borrow_mut();
                    let mut config = project.config().clone();
                    config.ref_image_auto_mode =
                        if self_.widgets.rb_smallest_stars.is_active()      { RefImageAutoMode::SmallestStars }
                        else if self_.widgets.rb_roundest_stars.is_active() { RefImageAutoMode::RoundestStars }
                        else if self_.widgets.rb_min_bg.is_active()         { RefImageAutoMode::MinBg }
                        else                                                { RefImageAutoMode::NinNoise };
                    project.set_config(config);
                    project.assign_ref_light_frame_automatically();
                    drop(project);
                    ok_fun();
                }
                dialog.close();
            })
        );
        self.widgets.dialog.show();
    }
}
