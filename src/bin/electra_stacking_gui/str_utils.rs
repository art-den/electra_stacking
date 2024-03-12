use std::{fmt::Debug, path::Path};
use gettextrs::*;

pub fn debug_to_str<T: Debug>(value: &T) -> String {
    format!("{:?}", value)
}

pub fn transl_and_replace(text: &str, items: &[(&str, String)]) -> String {
    let mut result = gettext(text);
    for (from, to) in items {
        result = result.replace(from, to);
    }
    result
}

pub fn path_to_string(path: &Path) -> String {
    path.to_str().unwrap_or_default().to_string()
}