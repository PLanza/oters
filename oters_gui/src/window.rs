use oters_lang as oters;

use oters::export::export_oters;

use crate::color::Color;
use lazy_static::lazy_static;
use macroquad::prelude::*;
use std::sync::Mutex;

lazy_static! {
    pub static ref BACKGROUND_COLOR: Mutex<Color> = Mutex::new(Color {
        r: 255,
        g: 255,
        b: 255,
        a: 255,
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

#[export_oters]
fn window_dims() -> (i64, i64) {
    (screen_width() as i64, screen_height() as i64)
}

#[export_oters]
fn resize_window(dimensions: (i64, i64)) {
    request_new_screen_size(dimensions.0 as f32, dimensions.0 as f32)
}

#[export_oters]
fn set_fullscreen(b: bool) {
    unsafe {
        get_internal_gl().quad_context.set_fullscreen(b);
    };
}
