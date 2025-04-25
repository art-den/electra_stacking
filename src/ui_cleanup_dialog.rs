use std::{cell::RefCell, rc::Rc};

use gettextrs::gettext;
use gtk::{prelude::*, glib::clone, glib};

use macros::FromBuilder;

use crate::{gtk_utils::*, project::{CleanupConfItem, CleanupMode, Project}, str_utils::transl_and_replace};

pub struct CleanupDialog {
    widgets: Widgets,
    project: Rc<RefCell<Project>>,
}

#[derive(FromBuilder)]
struct Widgets {
    dialog: gtk::Dialog,
    check_before: gtk::CheckButton,
    chb_rdev: gtk::CheckButton,
    cbt_rdev: gtk::ComboBoxText,
    e_rdev_kappa: gtk::Entry,
    e_rdev_repeats: gtk::Entry,
    e_rdev_percent: gtk::Entry,
    e_rdev_min: gtk::Entry,
    e_rdev_max: gtk::Entry,

    chb_fwhm: gtk::CheckButton,
    cbt_fwhm: gtk::ComboBoxText,
    e_fwhm_kappa: gtk::Entry,
    e_fwhm_repeats: gtk::Entry,
    e_fwhm_percent: gtk::Entry,
    e_fwhm_min: gtk::Entry,
    e_fwhm_max: gtk::Entry,

    chb_stars: gtk::CheckButton,
    cbt_stars: gtk::ComboBoxText,
    e_stars_kappa: gtk::Entry,
    e_stars_repeats: gtk::Entry,
    e_stars_percent: gtk::Entry,
    e_stars_min: gtk::Entry,
    e_stars_max: gtk::Entry,

    chb_noise: gtk::CheckButton,
    cbt_noise: gtk::ComboBoxText,
    e_noise_kappa: gtk::Entry,
    e_noise_repeats: gtk::Entry,
    e_noise_percent: gtk::Entry,
    e_noise_min: gtk::Entry,
    e_noise_max: gtk::Entry,

    chb_bg: gtk::CheckButton,
    cbt_bg: gtk::ComboBoxText,
    e_bg_kappa: gtk::Entry,
    e_bg_repeats: gtk::Entry,
    e_bg_percent: gtk::Entry,
    e_bg_min: gtk::Entry,
    e_bg_max: gtk::Entry,

}

impl CleanupDialog {
    pub fn new(
        parent:  Option<&impl IsA<gtk::Window>>,
        project: &Rc<RefCell<Project>>
    ) -> Rc<Self> {
        let widgets = Widgets::from_builder_str(include_str!("ui/cleanup_dialog.ui"));
        widgets.dialog.set_transient_for(parent);
        add_ok_and_cancel_buttons(
            &widgets.dialog,
            &gettext("_Cleanup"), gtk::ResponseType::Ok,
            &gettext("_Close"), gtk::ResponseType::Cancel
        );
        set_dialog_default_button(&widgets.dialog);
        Rc::new(Self {
            widgets,
            project: Rc::clone(project)
        })
    }

    pub fn exec(
        self:   &Rc<Self>,
        ok_fun: impl Fn() + 'static
    ) {
        let project = self.project.borrow();
        self.widgets.check_before.set_active(project.cleanup_conf().check_before_execute);

        let show_line = |
            item: &CleanupConfItem,
            chk_w: &gtk::CheckButton,
            mode_w: &gtk::ComboBoxText,
            kappa_w: &gtk::Entry,
            repeats_w: &gtk::Entry,
            percent_w: &gtk::Entry,
            min_w: &gtk::Entry,
            max_w: &gtk::Entry,
        | {
            let correct_sensivity = |
                chk_w:     &gtk::CheckButton,
                mode_w:    &gtk::ComboBoxText,
                kappa_w:   &gtk::Entry,
                repeats_w: &gtk::Entry,
                percent_w: &gtk::Entry,
                min_w:     &gtk::Entry,
                max_w:     &gtk::Entry,
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
        };

        show_line(
            &project.cleanup_conf().stars_r_dev,
            &self.widgets.chb_rdev,
            &self.widgets.cbt_rdev,
            &self.widgets.e_rdev_kappa,
            &self.widgets.e_rdev_repeats,
            &self.widgets.e_rdev_percent,
            &self.widgets.e_rdev_min,
            &self.widgets.e_rdev_max
        );

        show_line(
            &project.cleanup_conf().stars_fwhm,
            &self.widgets.chb_fwhm,
            &self.widgets.cbt_fwhm,
            &self.widgets.e_fwhm_kappa,
            &self.widgets.e_fwhm_repeats,
            &self.widgets.e_fwhm_percent,
            &self.widgets.e_fwhm_min,
            &self.widgets.e_fwhm_max
        );

        show_line(
            &project.cleanup_conf().stars_count,
            &self.widgets.chb_stars,
            &self.widgets.cbt_stars,
            &self.widgets.e_stars_kappa,
            &self.widgets.e_stars_repeats,
            &self.widgets.e_stars_percent,
            &self.widgets.e_stars_min,
            &self.widgets.e_stars_max
        );

        show_line(
            &project.cleanup_conf().noise,
            &self.widgets.chb_noise,
            &self.widgets.cbt_noise,
            &self.widgets.e_noise_kappa,
            &self.widgets.e_noise_repeats,
            &self.widgets.e_noise_percent,
            &self.widgets.e_noise_min,
            &self.widgets.e_noise_max
        );

        show_line(
            &project.cleanup_conf().background,
            &self.widgets.chb_bg,
            &self.widgets.cbt_bg,
            &self.widgets.e_bg_kappa,
            &self.widgets.e_bg_repeats,
            &self.widgets.e_bg_percent,
            &self.widgets.e_bg_min,
            &self.widgets.e_bg_max
        );

        drop(project);

        self.widgets.dialog.connect_response(clone!(@strong self as self_ => move |dialog, response| {
            if response == gtk::ResponseType::Ok {
                let mut project = self_.project.borrow_mut();

                let mut cleanup_conf = project.cleanup_conf().clone();

                cleanup_conf.check_before_execute = self_.widgets.check_before.is_active();

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

                get_line(
                    &mut cleanup_conf.stars_r_dev,
                    &self_.widgets.chb_rdev,
                    &self_.widgets.cbt_rdev,
                    &self_.widgets.e_rdev_kappa,
                    &self_.widgets.e_rdev_repeats,
                    &self_.widgets.e_rdev_percent,
                    &self_.widgets.e_rdev_min,
                    &self_.widgets.e_rdev_max
                );

                get_line(
                    &mut cleanup_conf.stars_fwhm,
                    &self_.widgets.chb_fwhm,
                    &self_.widgets.cbt_fwhm,
                    &self_.widgets.e_fwhm_kappa,
                    &self_.widgets.e_fwhm_repeats,
                    &self_.widgets.e_fwhm_percent,
                    &self_.widgets.e_fwhm_min,
                    &self_.widgets.e_fwhm_max
                );

                get_line(
                    &mut cleanup_conf.stars_count,
                    &self_.widgets.chb_stars,
                    &self_.widgets.cbt_stars,
                    &self_.widgets.e_stars_kappa,
                    &self_.widgets.e_stars_repeats,
                    &self_.widgets.e_stars_percent,
                    &self_.widgets.e_stars_min,
                    &self_.widgets.e_stars_max
                );

                get_line(
                    &mut cleanup_conf.noise,
                    &self_.widgets.chb_noise,
                    &self_.widgets.cbt_noise,
                    &self_.widgets.e_noise_kappa,
                    &self_.widgets.e_noise_repeats,
                    &self_.widgets.e_noise_percent,
                    &self_.widgets.e_noise_min,
                    &self_.widgets.e_noise_max
                );

                get_line(
                    &mut cleanup_conf.background,
                    &self_.widgets.chb_bg,
                    &self_.widgets.cbt_bg,
                    &self_.widgets.e_bg_kappa,
                    &self_.widgets.e_bg_repeats,
                    &self_.widgets.e_bg_percent,
                    &self_.widgets.e_bg_min,
                    &self_.widgets.e_bg_max
                );

                project.set_cleanup_conf(cleanup_conf);

                drop(project);

                let result = self_.project.borrow_mut().cleanup_light_files();
                let window = self_.widgets.dialog.transient_for().unwrap();
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
                            &window,
                            &gettext("Cleanup light files result"),
                            &message,
                            gtk::MessageType::Info,
                        );
                    },
                    Err(error) =>
                        show_error_message(&window, &gettext("Error"), &error.to_string()),
                }
                ok_fun();
            }
            else {
                dialog.close();
            }
        }));

        self.widgets.dialog.show();
    }
}
