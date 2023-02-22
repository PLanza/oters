use std::sync::Mutex;

use oters_lang as oters;

use oters::export::export_oters;

use lazy_static::lazy_static;
use macroquad::{prelude::Rect, texture::Texture2D};

use crate::color::Color;

lazy_static! {
    static ref TEXTURE_MANAGER: Mutex<TextureManager> = Mutex::new(TextureManager::new());
}

#[derive(Debug, Clone, Copy)]
#[export_oters]
pub struct Image {
    pub texture_id: i64,
    pub width: i64,
    pub height: i64,
    pub rotation: f64, // In radians
    pub flip_x: bool,
    pub flip_y: bool,
}

impl Image {
    pub fn draw(&self, pos: (u32, u32), dest_size: (u32, u32), src_rect: (i64, i64, i64, i64)) {
        use macroquad::texture::{draw_texture_ex, DrawTextureParams};
        let params = DrawTextureParams {
            dest_size: Some((dest_size.0 as f32, dest_size.1 as f32).into()),
            source: Some(Rect {
                x: src_rect.0 as f32,
                y: src_rect.1 as f32,
                w: src_rect.2 as f32,
                h: src_rect.3 as f32,
            }),
            flip_x: self.flip_x,
            flip_y: self.flip_y,
            rotation: self.rotation as f32,
            pivot: None,
        };

        draw_texture_ex(
            TEXTURE_MANAGER.lock().unwrap().texture_map[self.texture_id as usize],
            pos.0 as f32,
            pos.1 as f32,
            macroquad::color::WHITE,
            params,
        )
    }
}

struct TextureManager {
    texture_map: Vec<Texture2D>,
    next_id: usize,
}

impl TextureManager {
    fn new() -> Self {
        TextureManager {
            texture_map: Vec::new(),
            next_id: 0,
        }
    }

    fn add_texture(&mut self, image: &macroquad::texture::Image) -> usize {
        let id = self.next_id;
        self.next_id += 1;

        self.texture_map.push(Texture2D::from_image(&image));

        id
    }
}

#[export_oters]
pub fn img_from_file(path: String) -> Image {
    let image = futures::executor::block_on(macroquad::texture::load_image(&path)).unwrap();
    let id = TEXTURE_MANAGER.lock().unwrap().add_texture(&image);
    Image {
        texture_id: id as i64,
        width: image.width as i64,
        height: image.height as i64,
        rotation: 0.0,
        flip_x: false,
        flip_y: false,
    }
}

#[export_oters]
pub fn draw_img(
    image: Image,
    pos: (i64, i64),
    dest_size: (i64, i64),
    src_rect: (i64, i64, i64, i64),
) {
    use macroquad::math::Rect;
    use macroquad::texture::{draw_texture_ex, DrawTextureParams};
    let params = DrawTextureParams {
        dest_size: Some((dest_size.0 as f32, dest_size.1 as f32).into()),
        source: Some(Rect {
            x: src_rect.0 as f32,
            y: src_rect.1 as f32,
            w: src_rect.2 as f32,
            h: src_rect.3 as f32,
        }),
        flip_x: image.flip_x,
        flip_y: image.flip_y,
        rotation: image.rotation as f32,
        pivot: None,
    };

    draw_texture_ex(
        TEXTURE_MANAGER.lock().unwrap().texture_map[image.texture_id as usize],
        pos.0 as f32,
        pos.1 as f32,
        macroquad::color::WHITE,
        params,
    )
}

#[export_oters]
pub fn rotate_img(image: Image, rotation: f64) -> Image {
    let mut image = image;
    image.rotation = rotation;
    image
}

#[export_oters]
pub fn flip_x_img(image: Image) -> Image {
    let mut image = image;
    image.flip_x = !image.flip_x;
    image
}

#[export_oters]
pub fn flip_y_img(image: Image) -> Image {
    let mut image = image;
    image.flip_y = !image.flip_y;
    image
}

#[export_oters]
pub fn new_sub_img(image: Image, rect: (i64, i64, i64, i64)) -> Image {
    let mut t_manager = TEXTURE_MANAGER.lock().unwrap();
    let texture = t_manager.texture_map[image.texture_id as usize];
    let image = texture.get_texture_data();
    let sub_image = image.sub_image(Rect {
        x: rect.0 as f32,
        y: rect.1 as f32,
        w: rect.2 as f32,
        h: rect.3 as f32,
    });

    let new_id = t_manager.add_texture(&sub_image);
    Image {
        texture_id: new_id as i64,
        width: sub_image.width as i64,
        height: sub_image.height as i64,
        rotation: 0.0,
        flip_x: false,
        flip_y: false,
    }
}

#[export_oters]
pub fn get_pixel(image: Image, pos: (i64, i64)) -> Color {
    let texture = TEXTURE_MANAGER.lock().unwrap().texture_map[image.texture_id as usize];
    let pixel = texture
        .get_texture_data()
        .get_pixel(pos.0 as u32, pos.1 as u32);
    Color {
        r: pixel.r as i64,
        g: pixel.g as i64,
        b: pixel.b as i64,
        a: pixel.a as i64,
    }
}

#[export_oters]
pub fn set_pixel(image: Image, pos: (i64, i64), color: Color) {
    let texture = TEXTURE_MANAGER.lock().unwrap().texture_map[image.texture_id as usize];
    let mut image = texture.get_texture_data();
    image.set_pixel(pos.0 as u32, pos.1 as u32, color.to_macroquad());
    texture.update(&image);
}

#[export_oters]
pub fn get_screen_image() -> Image {
    let screen_image = macroquad::texture::get_screen_data();
    let mut t_manager = TEXTURE_MANAGER.lock().unwrap();

    let new_id = t_manager.add_texture(&screen_image);
    Image {
        texture_id: new_id as i64,
        width: screen_image.width as i64,
        height: screen_image.height as i64,
        rotation: 0.0,
        flip_x: false,
        flip_y: false,
    }
}
