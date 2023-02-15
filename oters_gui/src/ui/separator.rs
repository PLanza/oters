use oters_lang as oters;

use oters::export::export_oters;

use super::{UIInstance, UIType, FRAMES};

#[export_oters]
pub fn create_separator(frame_id: i64) -> i64 {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let size = frame.size.clone();
    let id = frame.elems.len();
    frame.elems.push(UIInstance {
        ty: UIType::Separator,
        pos: (0, 0),
        size,
        visible: false,
    });
    id as i64
}

#[export_oters]
pub fn draw_separator(frame_id: i64, sep_id: i64) {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let sep = &mut frame.elems[sep_id as usize];
    match sep {
        UIInstance {
            ty: UIType::Separator,
            pos,
            size,
            visible,
        } => {
            if *visible {
                let (x1, y1) = (pos.0 as f32 + 2.0, pos.1 as f32 + 2.0);
                let (x2, y2) = (x1 + size.0 as f32 - 2.0, y1 + size.1 as f32 - 2.0);
                macroquad::prelude::draw_line(x1, y1, x2, y2, 2.0, macroquad::color::GRAY);
            }
        }
        _ => panic!("Expected a separator"),
    }
}
