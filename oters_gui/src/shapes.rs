use oters::export::export_oters;

use crate::color::Color;

use macroquad::shapes::*;

#[derive(Debug)]
#[export_oters]
pub struct Circle {
    pub pos: (i64, i64),
    pub r: i64,
    pub color: Color,
}

#[export_oters]
pub fn circle(circle: Circle) {
    draw_circle(
        circle.pos.0 as f32,
        circle.pos.1 as f32,
        circle.r as f32,
        circle.color.to_macroquad(),
    )
}
