use oters_lang::export::export_oters;

use super::{UIInstance, UIType, FRAMES};
use crate::image::Image;

#[export_oters]
pub fn create_guimage(frame_id: i64, size: (i64, i64)) -> i64 {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let id = frame.elems.len();
    frame.elems.push(UIInstance {
        ty: UIType::Image,
        pos: (0, 0),
        size: (size.0 as u32, size.1 as u32),
        visible: false,
    });
    id as i64
}

#[export_oters]
pub fn draw_guimage(frame_id: i64, img_id: i64, image: Image) {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let img = &mut frame.elems[img_id as usize];
    match img {
        UIInstance {
            ty: UIType::Image,
            pos,
            size,
            visible,
        } => {
            if *visible {
                image.draw(*pos, *size, (0, 0, image.width, image.height))
            }
        }
        _ => panic!("Expected a checkbox"),
    }
}
