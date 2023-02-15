use oters_lang::export::export_oters;

use macroquad::prelude::*;
use macroquad::ui::root_ui;

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
fn floor(f: f64) -> i64 {
    f as i64
}

#[export_oters]
fn time_to_string(hours: i64, mins: i64, secs: i64) -> String {
    format!("{:02} : {:02} : {:02}", hours, mins, secs)
}

fn main() {
    let config = oters_gui::WindowConfig {
        title: "Stopwatch".to_string(),
        dimensions: (720, 480),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters_lang::run!(
        vec!["./oters_gui/examples/stopwatch.otrs".to_string()],
        config,
    );
}
