import scala.annotation.tailrec
import scala.collection.immutable.Vector

/** Quadratic Split Node algorithm
  *
  */
object Splitter {
  /** Select two nodes from given which bounding box free area is the biggest
    *
    * @param vector nodes to select from
    * @tparam B type of node
    * @return tuple of vector remaining nodes and two selected nodes
    */
  def pickSeeds[B <: HasBox](vector: Vector[B]) : (Vector[B], B, B) = {
    val all_pairs = for {
      first <- vector
      second <- vector if first != second
    } yield (first, second)
    val pair = all_pairs.maxBy[Float]((parameter : (B, B)) => {
      val big_box : Box = parameter._1.box.expand(parameter._2.box)
      big_box.area - parameter._1.box.area - parameter._2.box.area
    })
    (vector.diff(Vector[B](pair._1, pair._2)), pair._1, pair._2)
  }

  /** Split given nodes into two vectors
    *
    * @param vector nodes to split
    * @tparam B type of node
    * @return tuple of two tuples of vector of nodes and its bounding box
    */
  def splitNode[B <: HasBox](vector: Vector[B]) : ((Vector[B], Box), (Vector[B], Box)) = {
    val vectors_begin = pickSeeds(vector)
    val new_vector = vectors_begin._1
    val first_node = vectors_begin._2
    val second_node = vectors_begin._3
    val vector1 : Vector[B] = Vector[B](first_node)
    val vector2 : Vector[B] = Vector[B](second_node)

    val result = distributeEntry((new_vector, (vector1, first_node.box), (vector2, second_node.box)))
    (result._2, result._3)
  }

  type DistributionType[B] = (Vector[B], (Vector[B], Box), (Vector[B], Box))

  /** Split given nodes into to vectors by minimum box expanding
    *
    * @param par tuple of vector of nodes to split, tuple of vector of nodes and bounding box, second tuple
    * @tparam B type of node
    * @return tuple of vector of nodes to split, tuple of vector of nodes and bounding box, second tuple
    */
  @tailrec
  final def distributeEntry[B <: HasBox](par : DistributionType[B]) : DistributionType[B] = {
    if(par._1.isEmpty)
      par
    else if(par._2._1.size >= 2 && par._1.size + par._3._1.size <= 2) {
      (Vector[B](), (par._2._1 ++ par._1, par._2._2), par._3)
    }
    else if(par._2._1.size >= 2 && par._1.size + par._3._1.size <= 2) {
      (Vector[B](), par._2, (par._3._1 ++ par._1, par._3._2))
    }
    else {
      val (toAssign : B, new_vector : Vector[B]) = pickNext(par._1, par._2._2, par._3._2)
      val expand1 = par._2._2.calcExpand(toAssign.box)
      val expand2 = par._3._2.calcExpand(toAssign.box)
      if(expand1 < expand2)
        distributeEntry((new_vector, (par._2._1 :+ toAssign, par._2._2.expand(toAssign.box)), par._3))
      else if(expand1 > expand2)
        distributeEntry((new_vector, par._2, (par._3._1 :+ toAssign, par._3._2.expand(toAssign.box))))
      else {
        val area1 = par._2._2.area
        val area2 = par._3._2.area
        if(area1 < area2)
          distributeEntry((new_vector, (par._2._1 :+ toAssign, par._2._2.expand(toAssign.box)), par._3))
        else if(area1 > area2)
          distributeEntry((new_vector, par._2, (par._3._1 :+ toAssign, par._3._2.expand(toAssign.box))))
        else {
          if(par._2._1.size < par._3._1.size)
            distributeEntry((new_vector, (par._2._1 :+ toAssign, par._2._2.expand(toAssign.box)), par._3))
          else
            distributeEntry((new_vector, par._2, (par._3._1 :+ toAssign, par._3._2.expand(toAssign.box))))
        }
      }
    }
  }

  /** Finds node which difference expanding given boxes is the largest
    *
    * @param vector nodes to select from
    * @param box1 bounding box of first set
    * @param box2 bounding box of second set
    * @tparam B type of node
    * @return tuple of selected node and vector of remaining nodes
    */
  def pickNext[B <: HasBox](vector: Vector[B], box1 : Box, box2 : Box) : (B, Vector[B]) = {
    val (best_node : B, index : Int) = vector.zipWithIndex.maxBy[Float]((variable : (B, Int)) => {
      val expand1 = box1.calcExpand(variable._1.box)
      val expand2 = box2.calcExpand(variable._1.box)
      Math.abs(expand1 - expand2)
    })
    (best_node, vector.take(index) ++ vector.drop(index+1))
  }
}