use oters_lang::export::export_oters;

use lazy_static::lazy_static;
use macroquad::text::Font;
use std::sync::Mutex;

use crate::color::Color;

lazy_static! {
    static ref FONTS: Mutex<Vec<Font>> = Mutex::new(Vec::new());
}

#[export_oters]
pub fn font_from_file(path: String) -> i64 {
    let font = futures::executor::block_on(macroquad::prelude::load_ttf_font(&path)).unwrap();

    let fonts = &mut FONTS.lock().unwrap();
    let id = fonts.len();
    fonts.push(font);

    id as i64
}

#[export_oters]
pub fn draw_text(text: String, pos: (i64, i64), font_size: i64, color: Color) {
    macroquad::text::draw_text(
        &text,
        pos.0 as f32,
        pos.1 as f32,
        font_size as f32,
        color.to_macroquad(),
    )
}

#[export_oters]
pub fn draw_text_with_font(text: String, font: i64, pos: (i64, i64), font_size: i64, color: Color) {
    let params = macroquad::text::TextParams {
        font_size: font_size as u16,
        font_scale: 1.0,
        color: color.to_macroquad(),
        font: FONTS.lock().unwrap()[font as usize].clone(),
        ..Default::default()
    };
    macroquad::text::draw_text_ex(&text, pos.0 as f32, pos.1 as f32, params)
}
