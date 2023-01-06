use oters::export::export_oters;

use crate::color::Color;
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    pub static ref BACKGROUND_COLOR: Mutex<Color> = Mutex::new(Color {
        r: 0,
        g: 0,
        b: 0,
        a: 0,
    });
}

#[export_oters]
fn set_bg_color(color: Color) {
    let mut bg_color = BACKGROUND_COLOR.lock().unwrap();
    bg_color.r = color.r;
    bg_color.g = color.g;
    bg_color.b = color.b;
    bg_color.a = color.a;
}
