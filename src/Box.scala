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
}

object Box {
  def empty: Box = {
    val maxCoord = Math.sqrt(Float.MaxValue / 2.0F).toFloat / 2.0F
    val minCoord = -maxCoord
    Box(maxCoord, minCoord, maxCoord, minCoord)
  }
}