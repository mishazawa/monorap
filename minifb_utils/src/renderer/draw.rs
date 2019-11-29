use crate::renderer::Renderer;
use minifb::{ Key };

pub fn draw (renderer: &mut Renderer, f: impl Fn(&mut Renderer) -> Vec<Vec<u32>>) {
  while renderer.window.is_open() && !renderer.window.is_key_down(Key::Escape) {
    let pixels = Renderer::apply_layers(f(renderer));

    for (i, px) in renderer.buffer.iter_mut().enumerate() {
      *px = pixels[i];
    }
    renderer.window.update_with_buffer(&renderer.buffer).unwrap();
  }
}
