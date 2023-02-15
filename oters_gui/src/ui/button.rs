use macroquad::math::vec2;
use macroquad::ui;
use oters_lang::export::export_oters;

use super::{UIInstance, UIType, FRAMES};

#[export_oters]
pub fn create_button(frame_id: i64, size: (i64, i64)) -> i64 {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let id = frame.elems.len();
    frame.elems.push(UIInstance {
        ty: UIType::Button,
        pos: (0, 0),
        size: (size.0 as u32, size.1 as u32),
        visible: false,
    });
    id as i64
}

#[export_oters]
pub fn draw_button(frame_id: i64, btn_id: i64, label: String) -> bool {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let btn = &frame.elems[btn_id as usize];
    match btn {
        UIInstance {
            ty: UIType::Button,
            pos,
            size,
            visible,
        } => {
            if *visible {
                ui::widgets::Button::new(label)
                    .position(vec2(pos.0 as f32, pos.1 as f32))
                    .size((size.0 as f32, size.1 as f32).into())
                    .ui(&mut ui::root_ui())
            } else {
                false
            }
        }
        _ => panic!("Expected a button"),
    }
}
