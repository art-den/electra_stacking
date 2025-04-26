use std::{cell::RefCell, rc::Rc};

use gettextrs::gettext;
use gtk::{prelude::*, glib::clone};

use macros::FromBuilder;

use crate::{calc::{CalcMode, CalcOpts}, gtk_utils::{add_ok_and_cancel_buttons, set_dialog_default_button}, image_raw::CfaType, project::*};

pub struct ProjectOptionsDialog {
    widgets: Widgets,
    project: Rc<RefCell<Project>>,
}

#[derive(FromBuilder)]
struct Widgets {
    dialog:                   gtk::Dialog,
    project_name:             gtk::Entry,
    img_size:                 gtk::ComboBoxText,
    res_img_type:             gtk::ComboBoxText,
    chb_align_rgb:            gtk::CheckButton,
    chb_align_rgb_each:       gtk::CheckButton,
    lights_stack_mode:        gtk::ComboBoxText,
    lights_stack_kappa:       gtk::Entry,
    lights_stack_steps:       gtk::Entry,
    darks_stack_mode:         gtk::ComboBoxText,
    darks_stack_kappa:        gtk::Entry,
    darks_stack_steps:        gtk::Entry,
    flats_stack_mode:         gtk::ComboBoxText,
    flats_stack_kappa:        gtk::Entry,
    flats_stack_steps:        gtk::Entry,
    bias_stack_mode:          gtk::ComboBoxText,
    bias_stack_kappa:         gtk::Entry,
    bias_stack_steps:         gtk::Entry,
    chb_save_calibrated_img:  gtk::CheckButton,
    chb_save_common_star_img: gtk::CheckButton,
    cb_cfa_array:             gtk::ComboBoxText,
    chb_apply_wb:             gtk::CheckButton,
    chb_apply_color:          gtk::CheckButton,
}

impl ProjectOptionsDialog {
    pub fn new(
        parent:  Option<&impl IsA<gtk::Window>>,
        project: &Rc<RefCell<Project>>,
    ) -> Rc<Self> {
        let widgets = Widgets::from_builder_str(include_str!("ui/project_options_dialog.ui"));
        widgets.dialog.set_transient_for(parent);

        add_ok_and_cancel_buttons(
            &widgets.dialog,
            &gettext("_Ok"), gtk::ResponseType::Ok,
            &gettext("_Cancel"), gtk::ResponseType::Cancel
        );

        set_dialog_default_button(&widgets.dialog);

        let result = Rc::new(Self { widgets, project: Rc::clone(project) });
        result.connect_ui_events();
        result
    }

    fn connect_ui_events(self: &Rc<Self>) {
        self.widgets.chb_apply_wb.connect_active_notify(
            clone!(@strong self as self_ => move |v| {
                self_.widgets.chb_apply_color.set_sensitive(v.is_active());
            })
        );
    }

    pub fn exec(
        self:   &Rc<Self>,
        ok_fun: impl Fn() + 'static
    ) {
        let project = self.project.borrow();
        let project_config = project.config();
        self.show_options(project_config);
        drop(project);

        self.widgets.dialog.connect_response(
            clone!(@strong self as self_ => move |dialog, response| {
                if response == gtk::ResponseType::Ok {
                    let mut project = self_.project.borrow_mut();
                    let mut project_config = project.config().clone();
                    self_.get_option(&mut project_config);
                    project.set_config(project_config);
                    drop(project);
                    ok_fun();
                }
                dialog.close();
            }
        ));

        self.widgets.dialog.show();
    }

    fn show_options(&self, project_config: &ProjectConfig) {
        let widgets = &self.widgets;
        widgets.project_name.set_text(project_config.name.as_ref().unwrap_or(&String::new()).as_str());

        widgets.img_size.set_active(Some(match project_config.image_size {
            ImageSize::Original => 0,
            ImageSize::Bin2x2 => 1,
        }));

        widgets.res_img_type.set_active(Some(match project_config.res_img_type {
            ResFileType::Fit => 0,
            ResFileType::Tif => 1,
        }));

        widgets.chb_align_rgb.set_active(project_config.align_rgb);
        widgets.chb_align_rgb_each.set_active(project_config.align_rgb_each);

        let show_calc_opts = |opts: &CalcOpts, mode: &gtk::ComboBoxText, kappa: &gtk::Entry, steps: &gtk::Entry| {
            mode.append_text("Kappa-Sigma clipping");
            mode.append_text(&gettext("Median"));
            mode.append_text(&gettext("Mean"));

            mode.set_active(Some(match opts.mode {
                CalcMode::CappaSigma => 0,
                CalcMode::Median => 1,
                CalcMode::Mean => 2,
            }));
            kappa.set_text(&format!("{:.1}", opts.kappa));
            kappa.set_sensitive(opts.mode == CalcMode::CappaSigma);
            steps.set_text(&format!("{}", opts.repeats));
            steps.set_sensitive(opts.mode == CalcMode::CappaSigma);

            mode.connect_changed(clone!(@strong kappa, @strong steps => move |cb| {
                kappa.set_sensitive(cb.active() == Some(0));
                steps.set_sensitive(cb.active() == Some(0));
            }));
        };

        show_calc_opts(
            &project_config.light_calc_opts,
            &widgets.lights_stack_mode,
            &widgets.lights_stack_kappa,
            &widgets.lights_stack_steps
        );

        show_calc_opts(
            &project_config.dark_calc_opts,
            &widgets.darks_stack_mode,
            &widgets.darks_stack_kappa,
            &widgets.darks_stack_steps
        );

        show_calc_opts(
            &project_config.flat_calc_opts,
            &widgets.flats_stack_mode,
            &widgets.flats_stack_kappa,
            &widgets.flats_stack_steps
        );

        show_calc_opts(
            &project_config.bias_calc_opts,
            &widgets.bias_stack_mode,
            &widgets.bias_stack_kappa,
            &widgets.bias_stack_steps
        );

        widgets.chb_save_calibrated_img.set_active(project_config.save_aligned_img);
        widgets.chb_save_common_star_img.set_active(project_config.save_common_star_img);

        widgets.cb_cfa_array.set_active(Some(match project_config.raw_params.force_cfa {
            None                => 0,
            Some(CfaType::GBRG) => 1,
            Some(CfaType::RGGB) => 2,
            Some(CfaType::BGGR) => 3,
            Some(CfaType::GRBG) => 4,
        }));

        widgets.chb_apply_wb.set_active(project_config.raw_params.apply_wb);
        widgets.chb_apply_color.set_active(project_config.raw_params.apply_color);
        widgets.chb_apply_color.set_sensitive(widgets.chb_apply_wb.is_active());
    }

    fn get_option(&self, project_config: &mut ProjectConfig) {
        let get_calc_opts = |opts: &mut CalcOpts, mode: &gtk::ComboBoxText, kappa: &gtk::Entry, steps: &gtk::Entry| {
            opts.mode = match mode.active() {
                Some(0) => CalcMode::CappaSigma,
                Some(1) => CalcMode::Median,
                Some(2) => CalcMode::Mean,
                _ => panic!("Wrong mode.active(): {:?}", mode.active()),
            };
            opts.kappa = kappa.text().as_str().parse().unwrap_or(opts.kappa);
            opts.repeats = steps.text().as_str().parse().unwrap_or(opts.repeats);
        };

        let widgets = &self.widgets;

        get_calc_opts(
            &mut project_config.light_calc_opts,
            &widgets.lights_stack_mode,
            &widgets.lights_stack_kappa,
            &widgets.lights_stack_steps
        );

        get_calc_opts(
            &mut project_config.dark_calc_opts,
            &widgets.darks_stack_mode,
            &widgets.darks_stack_kappa,
            &widgets.darks_stack_steps
        );

        get_calc_opts(
            &mut project_config.flat_calc_opts,
            &widgets.flats_stack_mode,
            &widgets.flats_stack_kappa,
            &widgets.flats_stack_steps
        );

        get_calc_opts(
            &mut project_config.bias_calc_opts,
            &widgets.bias_stack_mode,
            &widgets.bias_stack_kappa,
            &widgets.bias_stack_steps
        );

        let name = widgets.project_name.text();
        project_config.name = if !name.is_empty() { Some(name.to_string()) } else { None };

        project_config.image_size = match widgets.img_size.active() {
            Some(0) => ImageSize::Original,
            Some(1) => ImageSize::Bin2x2,
            _ => panic!("Wrong img_size.active(): {:?}", widgets.img_size.active()),
        };

        project_config.res_img_type = match widgets.res_img_type.active() {
            Some(0) => ResFileType::Fit,
            Some(1) => ResFileType::Tif,
            _ => panic!("Wrong res_img_type.active(): {:?}", widgets.res_img_type.active()),
        };

        project_config.align_rgb = widgets.chb_align_rgb.is_active();
        project_config.align_rgb_each = widgets.chb_align_rgb_each.is_active();

        project_config.save_aligned_img = widgets.chb_save_calibrated_img.is_active();
        project_config.save_common_star_img = widgets.chb_save_common_star_img.is_active();

        project_config.raw_params.force_cfa = match widgets.cb_cfa_array.active() {
            Some(0) => None,
            Some(1) => Some(CfaType::GBRG),
            Some(2) => Some(CfaType::RGGB),
            Some(3) => Some(CfaType::BGGR),
            Some(4) => Some(CfaType::GRBG),
            _ => panic!("Wrong cb_cfa_array.active(): {:?}", widgets.cb_cfa_array.active()),
        };

        project_config.raw_params.apply_wb = widgets.chb_apply_wb.is_active();
        project_config.raw_params.apply_color = widgets.chb_apply_color.is_active();
    }
}
