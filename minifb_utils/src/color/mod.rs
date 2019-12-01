pub type SimpleColor = u32;

#[derive(Debug, Copy, Clone)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
    pub hex: SimpleColor,
}

#[derive(Debug)]
pub enum Colored {
    // RED = 0xffff0000,
    // GREEN = 0xff00ff00,
    // BLUE = 0xff0000ff,
    // WHITE = 0xffffffff,
    BLACK = 0xff000000,
    TRANSPARENT = 0x00000000,
}

impl Color {
    pub fn new(red: u8, green: u8, blue: u8, alpha: u8) -> Self {
        Self {
            r: red,
            g: green,
            b: blue,
            a: alpha,
            // ARGB idk
            hex: blue as u32 | (green as u32) << 8 | (red as u32) << 16 | (alpha as u32) << 24,
        }
    }

    // pub fn blend_color(background: Color, foreground: Color) -> SimpleColor {
    //     let alpha = 255u8.checked_sub(foreground.a).unwrap_or(0x00) as f32 / 100.;
    //     let falpha = foreground.a as f32 / 100.;

    //     let r = background.r as f32 * alpha + foreground.r as f32 * falpha;
    //     let g = background.g as f32 * alpha + foreground.g as f32 * falpha;
    //     let b = background.b as f32 * alpha + foreground.b as f32 * falpha;

    //     return Color::new(r as u8, g as u8, b as u8, 255).hex;
    // }
}

impl From<SimpleColor> for Color {
    fn from(color: SimpleColor) -> Self {
        let a: u8 = ((color >> 24) & 0xff) as u8;

        let r: u8 = ((color >> 16) & 0xff) as u8;
        let g: u8 = ((color >> 8) & 0xff) as u8;
        let b: u8 = (color & 0xff) as u8;

        Self {
            r,
            g,
            b,
            a,
            hex: color,
        }
    }
}

impl From<[u8; 4]> for Color {
    fn from(color: [u8; 4]) -> Self {
        Self::new(color[0], color[1], color[2], color[3])
    }
}

impl PartialEq for Color {
    fn eq(&self, other: &Self) -> bool {
        self.hex == other.hex
    }
}
