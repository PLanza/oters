use oters::export::{export_list, export_oters};

use macroquad::prelude::*;
use macroquad::ui::root_ui;

#[export_oters]
fn text(text: String, pos: (i64, i64), font_size: i64) {
    draw_text(&text, pos.0 as f32, pos.1 as f32, font_size as f32, BLACK)
}

#[export_oters]
fn button(pos: (i64, i64), label: String) -> bool {
    root_ui().button(
        Some(Vec2 {
            x: pos.0 as f32,
            y: pos.1 as f32,
        }),
        label,
    )
}

#[export_oters]
fn time_since_start() -> f64 {
    get_time()
}

#[export_oters]
fn floor(f: f64) -> i64 {
    f as i64
}

#[export_oters]
fn time_to_string(hours: i64, mins: i64, secs: i64) -> String {
    format!("{:02} : {:02} : {:02}", hours, mins, secs)
}

export_list!();

fn main() {
    let config = oters_gui::WindowConfig {
        title: "Stopwatch".to_string(),
        dimensions: (720, 480),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters_gui::run(
        vec!["./oters_gui/examples/stopwatch.otrs".to_string()],
        config,
        (
            EXPORT_FNS.clone(),
            EXPORT_STRUCTS.clone(),
            EXPORT_ENUMS.clone(),
        ),
    );
}
