package example

import processing.core.{PApplet, PVector}
import scala.collection.mutable.HashMap


class Index extends PApplet {
  var player: Player = _;
  var ground: Ground = _;
  var gameState: Index.GameState = new HashMap;

  override def settings (): Unit = {
    size(Index.WIDTH, Index.HEIGHT)
    ground = new Ground(this)
    player = new Player(this, new PVector(10, 10))
    gameState += ("ground" -> ground, "player" -> player)
    ground.setGameState(gameState);
    player.setGameState(gameState);
  }

  override def draw() = {
    background(0)
    ground.draw()
    player.draw();
  }
}

object Index {
  val WIDTH: Int = 200;
  val HEIGHT: Int = 200;
  val GRAVITY_FORCE: PVector = new PVector(0, 3.5f);
  type GameState = HashMap[String, GameObject];

  def main(args: Array[String]) = PApplet.main("example.Index")
}


trait GameObject {
  var gs: Index.GameState = new HashMap;

  def setGameState (state: Index.GameState): Unit = {
    gs = state;
  };
  // blank trait
}

trait DrawableObject extends GameObject {
  val p: PApplet;
  def draw (): Unit = {};
  def update (): Unit = {};
}

trait PhysicsObject extends GameObject {
  val collidable: Boolean = false;
  def isCollidedWith(coords: PVector): Boolean = {
    false
  }
}

trait StaticObject extends PhysicsObject {
  val position: PVector;
}

trait DynamicObject extends PhysicsObject {
  var position: PVector;
  def move(force: PVector): Unit = {};
  def fall(): Unit = {};
}

class Ground(rend: PApplet) extends StaticObject with DrawableObject {
  val p = rend;
  val position = new PVector(0, p.height / 2);

  override val collidable = true;

  override def draw (): Unit = {
    p.pushMatrix()
    p.pushStyle()
    p.translate(position.x, position.y)
    p.stroke(255, 0, 0)
    p.line(0, 0, p.width, 0)
    p.popStyle()
    p.popMatrix()
  }

}


class Player(rend: PApplet, coords: PVector = new PVector) extends DynamicObject with DrawableObject {
  val p = rend;
  var position = coords;

  // variables
  var selfSize = new PVector(10, 10);

  override def fall (): Unit = {
    gs.get("ground") match {
      case Some(ground) => {
        val gr = ground.asInstanceOf[StaticObject].position;
        if (!isOnGround(gr)) {
          position = position.add(Index.GRAVITY_FORCE)
        } else {
          position.y = gr.y - selfSize.y;
        }
      }
      case None => {}
    }

  }

  override def update (): Unit = {
    fall()
  }

  override def draw (): Unit = {
    update()

    p.pushMatrix()
    p.pushStyle()
    p.translate(position.x, position.y)
    p.noStroke()
    p.fill(255)
    p.rect(0, 0, selfSize.x, selfSize.y)
    p.popStyle()
    p.popMatrix()
  }

  def isOnGround (coords: PVector): Boolean = position.y + selfSize.y >= coords.y

}
