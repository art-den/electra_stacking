use std::{cell::RefCell, rc::Rc};

use gettextrs::gettext;
use gtk::{prelude::*, glib::clone, glib};

use macros::FromBuilder;

use crate::{calc::{MIN_KAPPA, MIN_REPEATS}, gtk_utils::*, project::{CleanupConfItem, CleanupMode, Project}, str_utils::transl_and_replace};

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
        for cbt in [
            &widgets.cbt_rdev, &widgets.cbt_fwhm, &widgets.cbt_stars,
            &widgets.cbt_noise, &widgets.cbt_bg,
        ] {
            cbt.append_text(&gettext("Sigma clipping"));
            cbt.append_text(&gettext("Percent"));
            cbt.append_text(&gettext("Min/Max"));
        }
        Rc::new(Self {
            widgets,
            project: Rc::clone(project)
        })
    }

    /// Single-use: call exactly once per instance (response handler is connected here).
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
            let correct_sensitivity = |
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
                correct_sensitivity(chk, &mode_w, &kappa_w, &repeats_w, &percent_w, &min_w, &max_w);
            }));
            chk_w.set_active(!item.used);
            chk_w.set_active(item.used);

            mode_w.connect_changed(clone!(@weak chk_w, @weak kappa_w, @weak repeats_w, @weak percent_w, @weak min_w, @weak max_w => move |cbt| {
                correct_sensitivity(&chk_w, cbt, &kappa_w, &repeats_w, &percent_w, &min_w, &max_w);
            }));
            match item.mode {
                CleanupMode::SigmaClipping => mode_w.set_active(Some(0)),
                CleanupMode::Percent => mode_w.set_active(Some(1)),
                CleanupMode::MinMax => mode_w.set_active(Some(2)),
            }

            // Default Display format: shortest round-trip, no precision loss on OK.
            kappa_w.set_text(&format!("{}", item.kappa));
            repeats_w.set_text(&format!("{}", item.repeats));
            percent_w.set_text(&format!("{}", item.percent));

            min_w.set_text(if let Some(min) = item.min { format!("{}", min) } else { "".to_string() }.as_str());
            max_w.set_text(if let Some(max) = item.max { format!("{}", max) } else { "".to_string() }.as_str());
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

        connect_response_self_disconnecting(&self.widgets.dialog,
            clone!(@strong self as self_ => move |_dialog, response| {
            if response == gtk::ResponseType::Ok {
                let mut project = self_.project.borrow_mut();

                let mut cleanup_conf = project.cleanup_conf().clone();

                cleanup_conf.check_before_execute = self_.widgets.check_before.is_active();

                let mut warnings = Vec::<String>::new();

                let mut get_line = |
                    item:      &mut CleanupConfItem,
                    label:     &str,
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
                        _       => item.mode.clone(),
                    };
                    item.kappa = parse_clamped_f32(
                        e_kappa, item.kappa, MIN_KAPPA, &format!("{}: {}", label, gettext("Kappa")), &mut warnings);
                    item.repeats = parse_clamped_u32(
                        e_repeats, item.repeats, MIN_REPEATS, &format!("{}: {}", label, gettext("Repeats")), &mut warnings);
                    item.percent = match e_percent.text().as_str().trim().parse::<u32>() {
                        Ok(v) => u32::clamp(v, 0, 100),
                        Err(_) => {
                            warnings.push(keep_prev_warning(
                                &format!("{}: {}", label, gettext("Percent")), e_percent.text().trim()));
                            item.percent
                        }
                    };
                    item.min = parse_optional_f32(
                        e_min, item.min, &format!("{}: {}", label, gettext("Min")), &mut warnings);
                    item.max = parse_optional_f32(
                        e_max, item.max, &format!("{}: {}", label, gettext("Max")), &mut warnings);
                };

                get_line(
                    &mut cleanup_conf.stars_r_dev,
                    &gettext("By stars ovality"),
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
                    &gettext("By stars FWHM"),
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
                    &gettext("By stars count"),
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
                    &gettext("By noise"),
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
                    &gettext("By background"),
                    &self_.widgets.chb_bg,
                    &self_.widgets.cbt_bg,
                    &self_.widgets.e_bg_kappa,
                    &self_.widgets.e_bg_repeats,
                    &self_.widgets.e_bg_percent,
                    &self_.widgets.e_bg_min,
                    &self_.widgets.e_bg_max
                );

                show_warnings(&self_.widgets.dialog, &warnings);

                project.set_cleanup_conf(cleanup_conf);

                drop(project);

                let result = self_.project.borrow_mut().cleanup_light_files();
                let window = self_.widgets.dialog.transient_for()
                    .unwrap_or_else(|| self_.widgets.dialog.clone().upcast::<gtk::Window>());
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
                // Keep the dialog open after a cleanup run (previous behaviour).
                return false;
            }
            true
        }));

        self.widgets.dialog.show();
    }
}
