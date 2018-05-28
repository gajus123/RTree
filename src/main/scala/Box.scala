case class Box(left: Float, right: Float, down: Float, top: Float) {
  val area: Float = {
    val x_diff = right - left
    val y_diff = top - down
    x_diff * y_diff

  }

  def expand(box: Box): Box = {
    val min_x = Math.min(left, box.left)
    val min_y = Math.min(down, box.down)
    val max_x = Math.max(right, box.right)
    val max_y = Math.max(top, box.top)
    Box(min_x, max_x, min_y, max_y)
  }

  def calcExpand(box: Box): Float = {
    val new_box = expand(box)
    new_box.area - area
  }

  /*def distance(point : Point) : Float = {
    val x_diff = {
      if(point.x < left)
        left - point.x
      else if(point.x < right)
        0.0F
      else
        point.x - right
    }
    val y_diff = {
      if(point.y < down)
        down - point.y
      else if(point.y < top)
        0.0F
      else
        point.y - top
    }
    Math.sqrt(x_diff * x_diff + y_diff * y_diff).toFloat
  }*/

  def instersect(box : Box) : Boolean = {
    val x_intersect = {
      if(box.left < left) {
        if (box.right >= left)
          true
        else
          false
      }
      else if(box.left <= right)
        true
      else //box.left > right
        false
    }
    val y_intersect = {
      if(box.down < down) {
        if (box.top >= down)
          true
        else
          false
      }
      else if(box.down <= top)
        true
      else //box.down > top
        false
    }
    if(x_intersect && y_intersect)
      true
    else
      false
  }
}

//case class Point(x : Float, y : Float) extends Box(x, x, y, y)

object Box {
  def empty: Box = {
    val maxCoord = Math.sqrt(Float.MaxValue / 2.0F).toFloat / 2.0F
    val minCoord = -maxCoord
    Box(maxCoord, minCoord, maxCoord, minCoord)
  }
}