use minifb::{Window, WindowOptions};
use color::{SimpleColor, Color};
use crate::{WIDTH, HEIGHT};

pub mod setup;
pub mod draw;
pub mod color;

#[derive(Debug)]
pub struct Renderer {
  pub window: Window,
  pub buffer: Vec<u32>,
}

impl Renderer {
  pub fn new () -> Self {
    let vec_len = WIDTH * HEIGHT;
    let window = Window::new(
      "Test - ESC to exit",
      WIDTH,
      HEIGHT,
      WindowOptions::default()
    ).unwrap_or_else(|e| {
      panic!("{}", e);
    });

    Self {
      buffer: vec![0; vec_len],
      window: window,
    }
  }

  pub fn apply_layers (layers: Vec<Vec<SimpleColor>>) -> Vec<SimpleColor> {
    let mask = vec![0x00; WIDTH * HEIGHT];
    return layers.iter().fold(mask, |mut acc, layer| {
      for (i, color) in acc.iter_mut().enumerate() {
        *color = Color::blend_color(Color::from(*color), Color::from(layer[i]));
      }
      acc
    });
  }
}
