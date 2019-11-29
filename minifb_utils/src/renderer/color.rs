#[derive(Debug, Default)]
pub struct Color {
  r: u8,
  g: u8,
  b: u8,
  a: u8,
  pub hex: u32,
}


impl Color {
  pub fn new (red: u8, green: u8, blue: u8, alpha: u8) -> Self {
    Self {
      r: red, g: green, b: blue, a: alpha,
      // ARGB idk
      hex: blue as u32 | (green as u32) << 8 | (red as u32) << 16 | (alpha as u32) << 24
    }
  }

  pub fn blend (source: Color, destination: Color) -> Color {
    let s = (255 - source.a) as u32;
    let t = destination.a as u32;
    let blue = (((source.hex >> 0)  & 0xff) * s + ((destination.hex >> 0)  & 0xff) * destination.a as u32) >> 8;
    let green = ((((source.hex >> 8)  & 0xff) * s + ((destination.hex >> 8)  & 0xff) * t)     )  & !0xff;
    let red = ((((source.hex >> 16) & 0xff) * s + ((destination.hex >> 16) & 0xff) * t) << 8)  & !0xffff;
    let alpha = ((((source.hex >> 24) & 0xff) * s + ((destination.hex >> 24) & 0xff) * t) << 16) & !0xffffff;


    Color {
      r: red as u8,
      g: green as u8,
      b: blue as u8,
      a: alpha as u8,
      hex: blue | green | red | alpha
    }
  }
}

