use oters_lang as oters;

use chrono::Timelike;
use macroquad::prelude::*;
use oters::export::export_oters;

#[export_oters]
pub fn fps() -> i64 {
    macroquad::time::get_fps() as i64
}

#[export_oters]
pub fn draw_fps_once() {
    draw_text(
        &format!("{}", macroquad::time::get_fps()),
        5.0,
        15.0,
        16.0,
        WHITE,
    )
}

#[export_oters]
pub fn time_since_start() -> f64 {
    get_time()
}

#[export_oters]
pub fn current_time() -> (i64, i64, i64, i64) {
    let now = chrono::Utc::now();
    (
        now.hour() as i64,
        now.minute() as i64,
        now.second() as i64,
        now.timestamp_subsec_millis() as i64,
    )
}

#[export_oters]
pub fn timestamp_millis() -> i64 {
    chrono::Utc::now().timestamp_millis()
}
