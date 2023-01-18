use macroquad::math::vec2;
use macroquad::ui;
use oters::export::export_oters;

use super::{UIInstance, UIType, FRAMES};

#[export_oters]
pub fn create_label(frame_id: i64, size: (i64, i64)) -> i64 {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let id = frame.elems.len();
    frame.elems.push(UIInstance {
        ty: UIType::Label,
        pos: (0, 0),
        size: (size.0 as u32, size.1 as u32),
        visible: false,
    });
    id as i64
}

#[export_oters]
pub fn draw_label(frame_id: i64, cbx_id: i64, content: String) {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let cbx = &mut frame.elems[cbx_id as usize];
    match cbx {
        UIInstance {
            ty: UIType::Label,
            pos,
            size,
            visible,
        } => {
            if *visible {
                ui::widgets::Label::new(content)
                    .position(vec2(pos.0 as f32, pos.1 as f32))
                    .size((size.0 as f32, size.1 as f32).into())
                    .ui(&mut ui::root_ui());
            }
        }
        _ => panic!("Expected a label"),
    }
}
