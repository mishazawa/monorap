pub fn coords_to_index (x: i32, y: i32, w: usize, h: usize) -> Option<usize> {
  let x_out = is_outside(x, 0, w);
  let y_out = is_outside(y, 0, h);

  return match x_out || y_out {
    true => None,
    false => {
      Some(x as usize + w * y as usize)
    }
  }
}

pub fn is_outside (val: i32, min: usize, max: usize) -> bool {
  return (val as usize) < min || (val as usize) > max;
}


pub fn limit (val: i32, min: i32, max:i32) -> i32 {
  return match val {
    v if v >= max => max,
    v if v < min => min,
    _ => val
  }
}
