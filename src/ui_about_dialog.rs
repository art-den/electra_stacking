use gettextrs::gettext;
use gtk::{prelude::*, gdk_pixbuf};

use macros::FromBuilder;

use crate::gtk_utils::set_dialog_default_button;


#[derive(FromBuilder)]
struct Widgets {
    dialog:        gtk::Dialog,
    image:         gtk::Image,
    btn_close:     gtk::Button,
    l_version:     gtk::Label,
    l_app_descr:   gtk::Label,
    lb_discussion: gtk::LinkButton,
}

pub fn show_about_dialog(parent: Option<&impl IsA<gtk::Window>>) {
    let widgets = Widgets::from_builder_str(include_str!("ui/about_dialog.ui"));
    let logo_image = gdk_pixbuf::Pixbuf::from_read(include_bytes!(
        r"ui/electra_128x128.png"
    ).as_slice()).unwrap();
    if widgets.l_app_descr.label() == "APP_DESCRIPTION" {
        widgets.l_app_descr.set_label(env!("CARGO_PKG_DESCRIPTION"))
    }
    widgets.image.set_pixbuf(Some(&logo_image));
    widgets.l_version.set_label(&format!("v{}", env!("CARGO_PKG_VERSION")));
    if gettext("cur_lang") == "ru" {

        widgets.lb_discussion.set_label("Обсудить на форуме astronomy.ru");
        widgets.lb_discussion.set_uri("https://astronomy.ru/forum/index.php/topic,201076.0.html");
    }
    widgets.dialog.set_transient_for(parent);
    set_dialog_default_button(&widgets.dialog);
    widgets.dialog.show();
    let dialog = widgets.dialog.clone();
    widgets.btn_close.connect_clicked(move |_| dialog.close());
}
