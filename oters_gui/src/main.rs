use anyhow::Result;
use oters::export::{export_fn, export_list};
use oters::interpret::Interpreter;
use oters::parser;
use oters::types::check::ProgramChecker;

use macroquad::prelude::*;

#[export_fn]
fn print_int_pair(pair: (i64, i64)) {
    println!("({}, {})", pair.0, pair.1);
}

#[export_fn]
fn mouse_pos() -> (i64, i64) {
    let (x_f, y_f) = macroquad::input::mouse_position();
    (x_f as i64, y_f as i64)
}

export_list!();

#[macroquad::main("InputTest")]
async fn main() -> Result<()> {
    let program = parser::parse_file("oters_gui/examples/input.otrs".to_string())?;
    let mut checker = ProgramChecker::new(EXPORT_FNS.clone());

    let exprs = checker.type_check_program(&program)?;

    let mut interpreter = Interpreter::new(exprs, EXPORT_FNS.clone())?;

    use macroquad::ui;

    #[export_fn]
    fn label(pos: (i64, i64), label: String) {
        ui::root_ui().label(
            Some(Vec2 {
                x: pos.0 as f32,
                y: pos.1 as f32,
            }),
            &label,
        )
    }

    #[export_fn]
    fn button(pos: (i64, i64), label: String) -> bool {
        ui::root_ui().button(
            Some(Vec2 {
                x: pos.0 as f32,
                y: pos.1 as f32,
            }),
            label,
        )
    }

    #[export_fn]
    fn time_since_start() -> f64 {
        get_time()
    }

    export_list!();

    loop {
        clear_background(WHITE);

        interpreter.eval_step()?;
        next_frame().await
    }
}
