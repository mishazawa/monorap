extern crate minifb;

mod primitives;
mod renderer;
mod util;

use crate::renderer::{ Renderer };
use crate::renderer::color::{ Color };

pub const WIDTH: usize = 640;
pub const HEIGHT: usize = 360;


fn main() {
  let mut processing = Renderer::new(WIDTH, HEIGHT);

  processing.setup(|p| {

  });

  processing.draw(|p| {
    // p.background(Color::from(0xff10ff80));
    for i in 100..200 {
      p.dot(i, i, Color::from(0xffffffff));
    }
  });
}
