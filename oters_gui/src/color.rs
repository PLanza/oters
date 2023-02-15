use oters_lang::export::export_oters;

#[derive(Debug)]
#[export_oters]
pub struct Color {
    pub r: i64,
    pub g: i64,
    pub b: i64,
    pub a: i64,
}

impl Color {
    pub fn to_macroquad(&self) -> macroquad::color::Color {
        macroquad::color::Color::from_rgba(self.r as u8, self.g as u8, self.b as u8, self.a as u8)
    }
}
