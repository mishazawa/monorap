use minifb::{Window, WindowOptions};
pub mod setup;
pub mod draw;
pub mod color;

#[derive(Debug)]
pub struct Renderer {
  pub window: Window,
  pub buffer: Vec<u32>,
}

impl Renderer {
  pub fn new (w: usize, h: usize) -> Self {
    let vec_len = (w * h) as usize;
    let window = Window::new(
      "Test - ESC to exit",
      w,
      h,
      WindowOptions::default()
    ).unwrap_or_else(|e| {
      panic!("{}", e);
    });

    Self {
      buffer: vec![0; vec_len],
      window: window,
    }
  }

  pub fn apply_layers (layers: Vec<Vec<u32>>) -> Vec<u32> {
    layers[0].clone()
  }
}
