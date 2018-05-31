/** A simple representation for minimum bounding rectangle
  *
  * @constructor
  * @param left minimum x value
  * @param right max value
  * @param down minimum y value
  * @param top max y value
  */
case class Box(left: Float, right: Float, down: Float, top: Float) {
  /** Calculates and returns box's area
    */
  val area: Float = {
    val x_diff = right - left
    val y_diff = top - down
    x_diff * y_diff

  }

  /** Expands box's boundaries to contain given box
    *
    * @param box marks the area to expand by
    * @return a new box with expanded boundaries
    */
  def expand(box: Box): Box = {
    val min_x = Math.min(left, box.left)
    val min_y = Math.min(down, box.down)
    val max_x = Math.max(right, box.right)
    val max_y = Math.max(top, box.top)
    Box(min_x, max_x, min_y, max_y)
  }

  /** Calculates and returns the area growth after expanding current box by given one
    *
    * @param box marks the area to calculate expansion of
    */
  def calcExpand(box: Box): Float = {
    val new_box = expand(box)
    new_box.area - area
  }

  /** Checks if box intersects given box
    *
    * @param box marks the area to check intersection with
    * @return true if boxes intersects and false otherwise
    */
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

object Box {
  /** Creates empty box
    *
    * Empty, box is box, that boundaries are at minimal and maximal coordinates, so it cannot intersect any other box
    * @return a new created empty box
    */
  def empty: Box = {
    val maxCoord = Math.sqrt(Float.MaxValue / 2.0F).toFloat / 2.0F
    val minCoord = -maxCoord
    Box(maxCoord, minCoord, maxCoord, minCoord)
  }
}