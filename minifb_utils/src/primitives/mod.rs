use crate::renderer::Renderer;
use crate::renderer::color::Color;
use crate::util;

pub fn dot (renderer: &mut Renderer, x: i32, y: i32) -> () {
  match util::coords_to_index(x, y, renderer.width, renderer.height) {
    Some(index) => {
      renderer.buffer[index] = renderer.stroke_color;
    },
    None => (),
  }
}

pub fn line (renderer: Renderer, x: i16, y: i16, x1: i16, y1: i16) -> i32 {
  0
}

pub fn background (renderer: &mut Renderer, color: Color) -> () {
  for pixel in renderer.buffer.iter_mut() {
    *pixel = color.hex;
  }

}
