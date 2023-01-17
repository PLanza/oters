extern crate oters_gui;

use anyhow::Result;

use oters::export::export_list;
use oters::interpret::Interpreter;
use oters::parser;
use oters::types::check::ProgramChecker;

use macroquad::prelude::*;

export_list!();

#[macroquad::main("UI Test")]
async fn main() -> Result<()> {
    // Temporary solution
    let mut export_fns = EXPORT_FNS.clone();
    export_fns.extend(oters_gui::EXPORT_FNS.clone().into_iter());
    let mut export_structs = EXPORT_STRUCTS.clone();
    export_structs.extend(oters_gui::EXPORT_STRUCTS.clone().into_iter());
    let mut export_enums = EXPORT_ENUMS.clone();
    export_enums.extend(oters_gui::EXPORT_ENUMS.clone().into_iter());

    let program = parser::parse_file("oters_gui/examples/ui_test.otrs".to_string())?;
    let mut checker = ProgramChecker::new((
        export_fns.clone(),
        export_structs.clone(),
        export_enums.clone(),
    ));

    let exprs = checker.type_check_program(&program)?;

    let mut interpreter = Interpreter::new(exprs, export_fns.clone())?;

    loop {
        clear_background(
            oters_gui::window::BACKGROUND_COLOR
                .lock()
                .unwrap()
                .to_macroquad(),
        );

        interpreter.eval_step()?;
        next_frame().await
    }
}
