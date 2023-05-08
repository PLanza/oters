use oters_lang as oters;

use oters::export::export_oters;

use crate::color::Color;

use macroquad::shapes::*;

#[derive(Debug)]
#[export_oters]
pub enum Shape {
    Circle((i64, i64), i64, Color),      // (Center Point, Radius, Color)
    Rect((i64, i64), (i64, i64), Color), // (Top-Left Point, Dimensions, Color)
    Line((i64, i64), (i64, i64), i64, Color), // (Point 1, Point 2, Thickness, Color)
    Triangle((i64, i64), (i64, i64), (i64, i64), Color), // (Point 1, Point 2, Point 3, Color)
}

#[export_oters]
pub fn draw_shape(shape: Shape) {
    match shape {
        Shape::Circle(pos, r, color) => {
            draw_circle(pos.0 as f32, pos.1 as f32, r as f32, color.to_macroquad())
        }

        Shape::Rect(pos, dims, color) => draw_rectangle(
            pos.0 as f32,
            pos.1 as f32,
            dims.0 as f32,
            dims.1 as f32,
            color.to_macroquad(),
        ),
        Shape::Line(p1, p2, thickness, color) => draw_line(
            p1.0 as f32,
            p1.1 as f32,
            p2.0 as f32,
            p2.1 as f32,
            thickness as f32,
            color.to_macroquad(),
        ),
        Shape::Triangle(p1, p2, p3, color) => draw_triangle(
            (p1.0 as f32, p1.1 as f32).into(),
            (p2.0 as f32, p2.1 as f32).into(),
            (p3.0 as f32, p3.1 as f32).into(),
            color.to_macroquad(),
        ),
    }
}

#[export_oters]
pub fn pos_squares(poses: Vec<(i64, i64)>, dims: (i64, i64), color: Color) -> Vec<Shape> {
    let mut squares = Vec::new();
    for pos in poses {
        squares.push(Shape::Rect(pos, dims, color));
    }
    squares
}

#[export_oters]
pub fn draw_batch(shapes: Vec<Shape>) {
    for shape in shapes {
        match shape {
            Shape::Circle(pos, r, color) => {
                draw_circle(pos.0 as f32, pos.1 as f32, r as f32, color.to_macroquad())
            }

            Shape::Rect(pos, dims, color) => draw_rectangle(
                pos.0 as f32,
                pos.1 as f32,
                dims.0 as f32,
                dims.1 as f32,
                color.to_macroquad(),
            ),
            Shape::Line(p1, p2, thickness, color) => draw_line(
                p1.0 as f32,
                p1.1 as f32,
                p2.0 as f32,
                p2.1 as f32,
                thickness as f32,
                color.to_macroquad(),
            ),
            Shape::Triangle(p1, p2, p3, color) => draw_triangle(
                (p1.0 as f32, p1.1 as f32).into(),
                (p2.0 as f32, p2.1 as f32).into(),
                (p3.0 as f32, p3.1 as f32).into(),
                color.to_macroquad(),
            ),
        }
    }
}
