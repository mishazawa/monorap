package example

import processing.core.{PApplet}

class Index extends PApplet {
      override def settings (): Unit = size(100, 100) 

      override def draw() = {
      
      }
}

object Index {
       def main(args: Array[String]) = PApplet.main("example.Index")
}

