extern crate minifb;

mod primitives;
mod renderer;
mod util;

use crate::renderer::{ Renderer, Processing };
use crate::renderer::color::{ Color };

pub const WIDTH: usize = 640;
pub const HEIGHT: usize = 360;


fn main() {
  let mut processing = Renderer::new(WIDTH, HEIGHT);

  processing.setup(|_p| {

  });

  processing.draw(|p| {
    p.background(Color::from(0xffffffff));
    for i in 100..200 {
      p.stroke(Color::from([i, i, i, i]));

      if i%2 == 0 {
        p.no_stroke(true);
      } else {
        p.no_stroke(false);
      }
      p.dot(i as i32 , i as i32);
    }
    p.dot(-1000, -121313);

    p.line(0, 0, WIDTH as i32, HEIGHT as i32);
    p.line(-10, -10, WIDTH as i32, HEIGHT as i32);

    p.stroke(Color::from([255, 0, 0, 255]));
    p.line(10, 10, 120, 120);
    p.line(10, 120, 120, 10);
    p.line(80 , 200 , 550, 150);

    p.stroke(Color::from([255, 0, 255, 255]));
    p.line(320, 0 , 320, 360);
  });
}
