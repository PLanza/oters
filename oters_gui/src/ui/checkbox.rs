use macroquad::math::vec2;
use macroquad::ui;
use oters::export::export_oters;

use super::{UIInstance, UIType, FRAMES};

#[export_oters]
pub fn create_checkbox(frame_id: i64, size: (i64, i64)) -> i64 {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let id = frame.elems.len();
    frame.elems.push(UIInstance {
        ty: UIType::Checkbox(false),
        pos: (0, 0),
        size: (size.0 as u32, size.1 as u32),
        visible: false,
    });
    id as i64
}

#[export_oters]
pub fn draw_checkbox(frame_id: i64, cbx_id: i64) -> bool {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let cbx = &mut frame.elems[cbx_id as usize];
    match cbx {
        UIInstance {
            ty: UIType::Checkbox(b),
            pos,
            size,
            visible,
        } => {
            if *visible {
                ui::widgets::Checkbox::new(cbx_id as u64)
                    .pos(vec2(pos.0 as f32, pos.1 as f32))
                    .size((size.0 as f32, size.1 as f32).into())
                    .ui(&mut ui::root_ui(), b);
                *b
            } else {
                false
            }
        }
        _ => panic!("Expected a checkbox"),
    }
}
