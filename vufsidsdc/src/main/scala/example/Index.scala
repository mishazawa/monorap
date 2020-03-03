package example

import processing.core.{PApplet}

class Index extends PApplet {
  override def settings (): Unit = size(Index.WIDTH, Index.HEIGHT)

  override def draw() = {
    background(0)

    // ground
    stroke(255, 0, 0)
    line(0, Index.HEIGHT / 2, Index.WIDTH, Index.HEIGHT / 2)
  }
}

object Index {
  val WIDTH: Int = 200;
  val HEIGHT: Int = 200;

  def main(args: Array[String]) = PApplet.main("example.Index")
}

