use std::{rc::Rc, path::{Path, PathBuf}};
use gettextrs::gettext;
use gtk::{prelude::*, gio, glib, glib::clone};

use crate::str_utils::transl_and_replace;

pub fn set_dialog_default_button(dialog: &gtk::Dialog) {
    use gtk::ResponseType::*;
    for resp in [Ok, Yes, Accept, Apply] {
        if let Some(btn) = dialog.widget_for_response(resp) {
            btn.grab_default();
            btn.style_context().add_class("suggested-action");
            break;
        }
    }
}

pub fn add_ok_and_cancel_buttons(
    dialog:      &gtk::Dialog,
    ok_cap:      &str,
    ok_type:     gtk::ResponseType,
    cancel_cap:  &str,
    cancel_type: gtk::ResponseType,
) {
    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[(ok_cap, ok_type), (cancel_cap, cancel_type)]);
    } else {
        dialog.add_buttons(&[(cancel_cap, cancel_type), (ok_cap, ok_type)]);
    }
}

pub fn add_yes_no_and_cancel_buttons(
    dialog:      &gtk::Dialog,
    yes_cap:     &str,
    yes_type:    gtk::ResponseType,
    no_cap:      &str,
    no_type:     gtk::ResponseType,
    cancel_cap:  &str,
    cancel_type: gtk::ResponseType,
) {
    if cfg!(target_os = "windows") {
        dialog.add_buttons(&[
            (yes_cap, yes_type),
            (no_cap, no_type),
            (cancel_cap, cancel_type),
        ]);
    } else {
        dialog.add_buttons(&[
            (cancel_cap, cancel_type),
            (no_cap, no_type),
            (yes_cap, yes_type),
        ]);
    }
}


pub fn disable_scroll_for_most_of_widgets(builder: &gtk::Builder) {
    for object in builder.objects() {
        if let Some(spin) = object.downcast_ref::<gtk::SpinButton>() {
            spin.connect_scroll_event(|_, _| {
                glib::Propagation::Stop
            });
        }
        if let Some(cb) = object.downcast_ref::<gtk::ComboBox>() {
            cb.connect_scroll_event(|_, _| {
                glib::Propagation::Stop
            });
        }
        if let Some(scale) = object.downcast_ref::<gtk::Scale>() {
            scale.connect_scroll_event(|_, _| {
                glib::Propagation::Stop
            });
        }
        if let Some(btn) = object.downcast_ref::<gtk::FileChooserButton>() {
            btn.connect_scroll_event(|_, _| {
                glib::Propagation::Stop
             });
        }
    }
}

pub fn connect_action<Fun, T>(
    action_map: &impl IsA<gio::ActionMap>,
    data:       &Rc<T>,
    act_name:   &str,
    fun:        Fun
) where
    Fun: Fn(&Rc<T>) + 'static,
    T: 'static,
{
    let action = gio::SimpleAction::new(act_name, None);
    action.connect_activate(clone!(@weak data => move |_, _|
        fun(&data);
    ));
    action_map.add_action(&action);
}

pub fn enable_action(
    action_map: &impl IsA<gio::ActionMap>,
    action:     &str,
    enabled:    bool,
) {
    if let Some(action) = action_map.lookup_action(action) {
        let sa = action
            .downcast::<gio::SimpleAction>()
            .expect("Is not gio::SimpleAction");
        if sa.is_enabled() != enabled {
            sa.set_enabled(enabled);
        }
    } else {
        panic!("Action {} not found", action);
    }
}

pub fn enable_actions(
    window: &gtk::ApplicationWindow,
    items:  &[(&str, bool)]
) {
    for &(action, enabled) in items {
        enable_action(window, action, enabled);
    }
}

pub fn show_message(
    window:   &impl IsA<gtk::Window>,
    title:    &str,
    text:     &str,
    msg_type: gtk::MessageType,
) {
    let dialog = gtk::MessageDialog::builder()
        .transient_for(window)
        .title(title)
        .text(text)
        .modal(true)
        .message_type(msg_type)
        .buttons(gtk::ButtonsType::Close)
        .build();

    dialog.show();

    dialog.connect_response(move |dlg, _| {
        dlg.close();
    });
}

pub fn show_error_message(
    window:  &impl IsA<gtk::Window>,
    title:   &str,
    message: &str,
) {
    show_message(window, title, message, gtk::MessageType::Error);
}

/// Parses the entry text as `f32`; valid values are finite and `>= min`.
/// Out-of-range input is clamped to `min`, written back into the entry and
/// reported in `warnings`. Unparseable text keeps `prev` and is reported in
/// `warnings`.
pub fn parse_clamped_f32(
    entry:    &gtk::Entry,
    prev:     f32,
    min:      f32,
    label:    &str,
    warnings: &mut Vec<String>,
) -> f32 {
    let (value, rewrite) = parse_clamped_f32_text(entry.text().as_str(), prev, min, label, warnings);
    if let Some(new_text) = rewrite {
        entry.set_text(&new_text);
    }
    value
}

/// Pure core of `parse_clamped_f32`; the second tuple element is the corrected
/// text to write back into the entry, if any.
fn parse_clamped_f32_text(
    text:     &str,
    prev:     f32,
    min:      f32,
    label:    &str,
    warnings: &mut Vec<String>,
) -> (f32, Option<String>) {
    let text = text.trim();
    match text.parse::<f32>() {
        Ok(v) if f32::is_finite(v) && v >= min => (v, None),
        Ok(v) => {
            warnings.push(clamp_warning(label, &v.to_string(), &format!("{min:.1}")));
            (min, Some(format!("{min:.1}")))
        }
        Err(_) => {
            warnings.push(keep_prev_warning(label, text));
            (prev, None)
        }
    }
}

fn clamp_warning(label: &str, value: &str, min_str: &str) -> String {
    transl_and_replace(
        "{label}: '{value}' is invalid, must be >= {min}. Using {min}.",
        &[
            ("{label}", label.to_string()),
            ("{value}", value.to_string()),
            ("{min}",    min_str.to_string()),
        ],
    )
}

/// Shared warning for "input rejected, previous value kept".
pub fn keep_prev_warning(label: &str, value: &str) -> String {
    transl_and_replace(
        "{label}: '{value}' is invalid. Using previous value.",
        &[("{label}", label.to_string()), ("{value}", value.to_string())],
    )
}

/// Same as `parse_clamped_f32`, for integer (`u32`) entries.
pub fn parse_clamped_u32(
    entry:    &gtk::Entry,
    prev:     u32,
    min:      u32,
    label:    &str,
    warnings: &mut Vec<String>,
) -> u32 {
    let (value, rewrite) = parse_clamped_u32_text(entry.text().as_str(), prev, min, label, warnings);
    if let Some(new_text) = rewrite {
        entry.set_text(&new_text);
    }
    value
}

/// Pure core of `parse_clamped_u32`; the second tuple element is the corrected
/// text to write back into the entry, if any.
fn parse_clamped_u32_text(
    text:     &str,
    prev:     u32,
    min:      u32,
    label:    &str,
    warnings: &mut Vec<String>,
) -> (u32, Option<String>) {
    let text = text.trim();
    match text.parse::<u32>() {
        Ok(v) if v >= min => (v, None),
        Ok(v) => {
            warnings.push(clamp_warning(label, &v.to_string(), &min.to_string()));
            (min, Some(min.to_string()))
        }
        Err(_) => {
            warnings.push(keep_prev_warning(label, text));
            (prev, None)
        }
    }
}

/// Parses the entry text as optional `f32`. Empty/whitespace-only text means
/// `None` (the bound is intentionally disabled); non-finite or unparseable text
/// keeps `prev` and is reported in `warnings`.
pub fn parse_optional_f32(
    entry:    &gtk::Entry,
    prev:     Option<f32>,
    label:    &str,
    warnings: &mut Vec<String>,
) -> Option<f32> {
    parse_optional_f32_text(entry.text().as_str(), prev, label, warnings)
}

/// Pure core of `parse_optional_f32`.
fn parse_optional_f32_text(
    text:     &str,
    prev:     Option<f32>,
    label:    &str,
    warnings: &mut Vec<String>,
) -> Option<f32> {
    let text = text.trim();
    if text.is_empty() {
        return None;
    }
    match text.parse::<f32>() {
        Ok(v) if f32::is_finite(v) => Some(v),
        _ => {
            warnings.push(match prev {
                Some(_) => keep_prev_warning(label, text),
                None => transl_and_replace(
                    "{label}: '{value}' is invalid. Value left empty.",
                    &[("{label}", label.to_string()), ("{value}", text.to_string())],
                ),
            });
            prev
        }
    }
}

/// Shows a modal warning dialog listing `warnings`; no-op when the list is empty.
pub fn show_warnings(window: &impl IsA<gtk::Window>, warnings: &[String]) {
    if warnings.is_empty() { return; }
    show_message(
        window,
        &gettext("Warning"),
        &warnings.join("\n"),
        gtk::MessageType::Warning,
    );
}

pub fn exec_and_show_error(
    window: &gtk::ApplicationWindow,
    fun:    impl FnOnce() -> anyhow::Result<()>
) {
    let exec_res = fun();
    if let Err(err) = exec_res {
        let message = if cfg!(debug_assertions) {
            format!("{}\n\nat\n\n{}", err, err.backtrace())
        } else {
            err.to_string()
        };
        show_error_message(window, "Error", &message);
    }
}

pub fn get_model_row_count(model: &gtk::TreeModel) -> usize {
    let Some(iter) = model.iter_first() else {
        return 0;
    };
    let mut result = 1;
    while model.iter_next(&iter) {
        result += 1;
    }
    result
}

pub fn get_list_view_selected_row(tree: &gtk::TreeView) -> Option<i32> {
    if let [before_selection] =
    tree.selection().selected_rows().0.as_slice() {
        if let &[row] = before_selection.indices().as_slice() {
            Some(row)
        } else {
            None
        }
    } else {
        None
    }
}

pub fn is_combobox_empty<T: IsA<gtk::ComboBox>>(cb: &T) -> bool {
    let Some(model) = cb.model() else { return true; };
    model.iter_first().is_none()
}

pub fn combobox_items_count<T: IsA<gtk::ComboBox>>(cb: &T) -> usize {
    let model = cb.model().unwrap();
    get_model_row_count(&model)
}

enum GtkHelperRoot {
    Builder(gtk::Builder),
    Container(gtk::Container),
}

pub struct UiHelper {
    root: GtkHelperRoot,
}

impl UiHelper {
    pub fn new_from_builder(bldr: &gtk::Builder) -> Self {
        Self {
            root: GtkHelperRoot::Builder(bldr.clone())
        }
    }

    pub fn object_by_id(&self, obj_bldr_id: &str) -> glib::Object {
        match &self.root {
            GtkHelperRoot::Builder(bldr) => bldr.object(obj_bldr_id).expect("Object not found"),
            GtkHelperRoot::Container(_) => todo!(),
        }
    }

    ///////////////////////////////////////////////////////////////////////////

    pub fn enable_widgets(&self, force_set: bool, names: &[(&str, bool)]) {
        for (widget_name, enable) in names {
            let object = self.object_by_id(widget_name);
            let widget = object
                .downcast::<gtk::Widget>()
                .expect("Is not gtk::Widget");
            if force_set || widget.is_sensitive() != *enable {
                widget.set_sensitive(*enable);
            }
        }
    }

    pub fn show_widgets(&self, names: &[(&str, bool)]) {
        for (widget_name, visible) in names {
            let object = self.object_by_id(widget_name);
            let widget = object
                .downcast::<gtk::Widget>()
                .expect("Is not gtk::Widget");
            if widget.is_visible() != *visible {
                widget.set_visible(*visible);
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////

    // bool

    pub fn set_prop_bool(&self, name_and_prop: &str, value: bool) {
        let (name, prop) = Self::extract_name_and_prop(name_and_prop);
        self.object_by_id(name)
            .set_property_from_value(prop, &value.into());
    }

    pub fn prop_bool(&self, name_and_prop: &str) -> bool {
        let (name, prop) = Self::extract_name_and_prop(name_and_prop);
        self.object_by_id(name)
            .property_value(prop)
            .get::<bool>()
            .expect("Wrong property type")
    }

    pub fn set_prop_bool_ex(&self, obj_bldr_id: &str, prop_name: &str, value: bool) {
        self.object_by_id(obj_bldr_id)
            .set_property_from_value(prop_name, &value.into());
    }

    pub fn prop_bool_ex(&self, obj_bldr_id: &str, prop_name: &str) -> bool {
        self.object_by_id(obj_bldr_id)
            .property_value(prop_name)
            .get::<bool>()
            .expect("Wrong property type")
    }

    // &str /  String

    pub fn set_prop_str(&self, name_and_prop: &str, value: Option<&str>) {
        let (name, prop) = Self::extract_name_and_prop(name_and_prop);
        self.object_by_id(name)
            .set_property_from_value(prop, &value.into());
    }

    pub fn prop_string(&self, name_and_prop: &str) -> Option<String> {
        let (name, prop) = Self::extract_name_and_prop(name_and_prop);
        self.object_by_id(name)
            .property_value(prop)
            .get::<Option<String>>()
            .expect("Wrong property type")
    }

    pub fn set_prop_str_ex(&self, obj_bldr_id: &str, prop_name: &str, value: Option<&str>) {
        self.object_by_id(obj_bldr_id)
            .set_property_from_value(prop_name, &value.into());
    }

    pub fn prop_string_ex(&self, obj_bldr_id: &str, prop_name: &str) -> Option<String> {
        self.object_by_id(obj_bldr_id)
            .property_value(prop_name)
            .get::<Option<String>>()
            .expect("Wrong property type")
    }

    // f64

    pub fn set_prop_f64(&self, name_and_prop: &str, value: f64) {
        let (name, prop) = Self::extract_name_and_prop(name_and_prop);
        self.object_by_id(name)
            .set_property_from_value(prop, &value.into());
    }

    pub fn prop_f64(&self, name_and_prop: &str) -> f64 {
        let (name, prop) = Self::extract_name_and_prop(name_and_prop);
        self.object_by_id(name)
            .property_value(prop)
            .get::<f64>()
            .expect("Wrong property type")
    }

    pub fn set_prop_f64_ex(&self, obj_bldr_id: &str, prop_name: &str, value: f64) {
        self.object_by_id(obj_bldr_id)
            .set_property_from_value(prop_name, &value.into());
    }

    pub fn prop_f64_ex(&self, obj_bldr_id: &str, prop_name: &str) -> f64 {
        self.object_by_id(obj_bldr_id)
            .property_value(prop_name)
            .get::<f64>()
            .expect("Wrong property type")
    }

    // i32

    pub fn set_prop_i32(&self, name_and_prop: &str, value: i32) {
        let (name, prop) = Self::extract_name_and_prop(name_and_prop);
        self.object_by_id(name)
            .set_property_from_value(prop, &value.into());
    }

    pub fn prop_i32(&self, name_and_prop: &str) -> i32 {
        let (name, prop) = Self::extract_name_and_prop(name_and_prop);
        self.object_by_id(name)
            .property_value(prop)
            .get::<i32>()
            .expect("Wrong property type")
    }

    pub fn set_prop_i32_ex(&self, obj_bldr_id: &str, prop_name: &str, value: i32) {
        self.object_by_id(obj_bldr_id)
            .set_property_from_value(prop_name, &value.into());
    }

    pub fn prop_i32_ex(&self, obj_bldr_id: &str, prop_name: &str) -> i32 {
        self.object_by_id(obj_bldr_id)
            .property_value(prop_name)
            .get::<i32>()
            .expect("Wrong property type")
    }


    ///////////////////////////////////////////////////////////////////////////

    pub fn set_fch_path(&self, obj_bldr_id: &str, path: Option<&Path>) {
        let widget = self.object_by_id(obj_bldr_id);
        let fch = widget
            .downcast::<gtk::FileChooserButton>()
            .expect("Widget is not gtk::FileChooserButton");
        let Some(path) = path else { return; };
        fch.set_filename(path);
    }

    pub fn fch_pathbuf(&self, obj_bldr_id: &str) -> Option<PathBuf> {
        let widget = self.object_by_id(obj_bldr_id);
        let fch = widget
            .downcast::<gtk::FileChooserButton>()
            .expect("Widget is not gtk::FileChooserButton");
        fch.filename()
    }

    pub fn set_range_value(&self, obj_bldr_id: &str, value: f64) {
        let widget = self.object_by_id(obj_bldr_id);
        let range = widget
            .downcast::<gtk::Range>()
            .expect("Widget is not gtk::Range");
        range.set_value(value);
    }

    pub fn range_value(&self, obj_bldr_id: &str) -> f64 {
        let widget = self.object_by_id(obj_bldr_id);
        let range = widget
            .downcast::<gtk::Range>()
            .expect("Widget is not gtk::Range");
        range.value()
    }

    pub fn is_combobox_empty(&self, widget_name: &str) -> bool {
        let widget = self.object_by_id(widget_name);
        let cb = widget
            .downcast::<gtk::ComboBox>()
            .expect("Widget is not gtk::Range");
        is_combobox_empty(&cb)
    }

    ///////////////////////////////////////////////////////////////////////////

    fn extract_name_and_prop(name_and_prop: &str) -> (&str, &str) {
        let split_pos = name_and_prop
            .bytes()
            .position(|v| v == b'.')
            .expect("`.` not found");
        let name = &name_and_prop[..split_pos];
        let prop = &name_and_prop[split_pos+1..];
        (name, prop)
    }
}

pub fn select_file_name_to_save(
    parent:        &impl IsA<gtk::Window>,
    title:         &str,
    filter_name:   &str,
    filter_ext:    &str,
    ext:           &str,
    def_file_name: &str,
) -> Option<PathBuf> {
    let ff = gtk::FileFilter::new();
    ff.set_name(Some(filter_name));
    ff.add_pattern(filter_ext);
    let fc = gtk::FileChooserDialog::builder()
        .action(gtk::FileChooserAction::Save)
        .title(title)
        .filter(&ff)
        .modal(true)
        .transient_for(parent)
        .build();
    fc.set_current_name(def_file_name);
    add_ok_and_cancel_buttons(
        fc.upcast_ref::<gtk::Dialog>(),
        "_Save",   gtk::ResponseType::Accept,
        "_Cancel", gtk::ResponseType::Cancel,
    );
    let resp = fc.run();
    fc.close();
    if resp != gtk::ResponseType::Accept {
        None
    } else {
        Some(fc.file()?
        .path()?
        .with_extension(ext))
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_clamped_f32_text, parse_clamped_u32_text, parse_optional_f32_text};

    fn parse(text: &str, prev: Option<f32>) -> (Option<f32>, Vec<String>) {
        let mut warnings = Vec::new();
        let value = parse_optional_f32_text(text, prev, "L", &mut warnings);
        (value, warnings)
    }

    #[test]
    fn empty_or_whitespace_text_means_none() {
        assert_eq!(parse("", Some(1.5)), (None, vec![]));
        assert_eq!(parse("   ", Some(1.5)), (None, vec![]));
    }

    #[test]
    fn parses_finite_values() {
        assert_eq!(parse("1.5", None), (Some(1.5), vec![]));
        assert_eq!(parse(" -2.25 ", Some(0.0)), (Some(-2.25), vec![]));
    }

    #[test]
    fn unparsable_text_keeps_prev_with_warning() {
        let (value, warnings) = parse("1,5", Some(1.5));
        assert_eq!(value, Some(1.5));
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("previous value"));

        let (value, warnings) = parse("1,5", None);
        assert_eq!(value, None);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("left empty"));
    }

    #[test]
    fn rejects_non_finite_values() {
        assert_eq!(parse("inf", Some(1.5)).0, Some(1.5));
        assert_eq!(parse("NaN", None).0, None);
        assert_eq!(parse("inf", Some(1.5)).1.len(), 1);
        assert_eq!(parse("NaN", None).1.len(), 1);
    }

    #[test]
    fn clamped_f32_valid_and_below_min() {
        let mut warnings = Vec::new();
        assert_eq!(parse_clamped_f32_text(" 1.8 ", 2.0, 1.0, "L", &mut warnings), (1.8, None));
        assert!(warnings.is_empty());

        let (value, rewrite) = parse_clamped_f32_text("0.5", 2.0, 1.0, "L", &mut warnings);
        assert_eq!((value, rewrite.as_deref()), (1.0, Some("1.0")));
        assert_eq!(warnings.len(), 1);
    }

    #[test]
    fn clamped_f32_unparsable_keeps_prev_with_warning() {
        let mut warnings = Vec::new();
        let (value, rewrite) = parse_clamped_f32_text("1,8", 2.0, 1.0, "L", &mut warnings);
        assert_eq!((value, rewrite), (2.0, None));
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("previous value"));
    }

    #[test]
    fn clamped_u32_valid_below_min_and_unparsable() {
        let mut warnings = Vec::new();
        assert_eq!(parse_clamped_u32_text("3", 1, 1, "L", &mut warnings), (3, None));
        assert!(warnings.is_empty());

        let (value, rewrite) = parse_clamped_u32_text("0", 5, 1, "L", &mut warnings);
        assert_eq!((value, rewrite.as_deref()), (1, Some("1")));

        let (value, rewrite) = parse_clamped_u32_text("2.5", 5, 1, "L", &mut warnings);
        assert_eq!((value, rewrite), (5, None));
        assert_eq!(warnings.len(), 2);
        assert!(warnings[1].contains("previous value"));
    }
}
