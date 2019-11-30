use crate::renderer::Renderer;
use crate::renderer::color::Color;
use crate::util;

pub fn dot (renderer: &mut Renderer, x: i32, y: i32, color: Color) -> () {
  match util::coords_to_index(x, y, renderer.width, renderer.height) {
    Some(index) => {
      // drawing dot
      // println!("{:?}", index);
      renderer.push_layer(|layer| {
        layer[index] = color.hex;
        layer.to_vec()
      });
    },
    None => (),
  }
}

pub fn line (renderer: Renderer, x: i16, y: i16, x1: i16, y1: i16) -> i32 {
  0
}

pub fn background (renderer: &mut Renderer, color: Color) -> () {
  renderer.push_layer(|layer| {
    for pixel in layer.iter_mut() {
      *pixel = color.hex;
    }
    layer.to_vec()
  });
}
