import scala.annotation.tailrec
import scala.collection.immutable.Vector

abstract class Node[A](val box : Box) extends HasBox {
  val maxNodeSize : Int = 4

  def insert(element: Element[A]): Either[Vector[Node[A]], Node[A]]

  def remove(element: Element[A]): Node[A] = {
    this
  }

  def search(search_box : Box) : List[Element[A]]

  //Helper
  def print : String = {
    def recur(node : Node[A], i : Int, sb : StringBuilder) : Unit = {
      val pad = " " * i
      val a = node.box.area
      node match {
        case LeafNode(childs, box) =>
          val pad2 = " " * (i + 1)
          sb.append(s"$pad leaf $a $box:\n")
          childs.foreach { case Element(p_box, value) =>
            sb.append(s"$pad2 element $p_box: $value\n")
          }
        case CompositeNode(childs, box) =>
          sb.append(s"$pad composite $a $box:\n")
          childs.foreach(c => recur(c, i+1, sb))
      }
    }
    val sb = new StringBuilder
    recur(this, 0, sb)
    sb.toString
  }

  def splitLeafNode(vector : Vector[Element[A]]) : Vector[LeafNode[A]] = {
    val ((v1, b1), (v2, b2)) = Splitter.splitNode(vector)
    Vector(LeafNode(v1, b1), LeafNode(v2, b2))
  }

  def splitCompositeNode(vector : Vector[Node[A]]) : Vector[CompositeNode[A]] = {
    val ((v1, b1), (v2, b2)) = Splitter.splitNode(vector)
    Vector(CompositeNode(v1, b1), CompositeNode(v2, b2))
  }
}

object Node {
  def empty[A] : Node[A] = LeafNode[A](Vector.empty[Element[A]], Box.empty)

  def getBestNodeAndIndex[A](collection : Seq[Node[A]], box : Box) : (Node[A], Int) = {
    collection.zipWithIndex.minBy[Float]((variable : (Node[A], Int)) => {
      variable._1.box.calcExpand(box)
    })
  }
}

case class LeafNode[A](childs : Vector[Element[A]], override val box: Box) extends Node[A](box) {
  def insert(element: Element[A]): Either[Vector[Node[A]], Node[A]] = {
    val new_childs : Vector[Element[A]] = childs :+ element
    if(new_childs.size <= maxNodeSize)
      Right(LeafNode(new_childs, box.expand(element.box)))
    else
      Left(splitLeafNode(new_childs))
  }

  def search(search_box : Box) : List[Element[A]] = {
    @tailrec
    def helper(vector : Vector[Element[A]], result : List[Element[A]]) : List[Element[A]] = {
      if(vector.isEmpty)
        result
      else {
        val current = vector.head
        if (current.box.instersect(search_box))
          helper(vector.tail, result :+ current)
        else
          helper(vector.tail, result)
      }
    }
    helper(childs, List.empty[Element[A]])
  }

  /*def nearest(point : Point) : Element[A] = {
    val best_element = childs.minBy[Float]((element : Element[A]) => {
      element.box.distance(point)
    })
    best_element
  }*/
}

case class CompositeNode[A](childs : Vector[Node[A]], override val box: Box) extends Node[A](box) {
  def insert(element: Element[A]): Either[Vector[Node[A]], Node[A]] = {
    val (best_node : Node[A], index : Int) = Node.getBestNodeAndIndex(childs, element.box)

    best_node.insert(element) match {
      case Left(nodes) =>
        val new_nodes = childs.take(index) ++ childs.drop(index+1) ++ nodes
        if(new_nodes.size <= maxNodeSize) {
          val new_box = nodes.foldLeft(box)((temp_box : Box, node: Node[A]) => temp_box.expand(node.box))
          Right(CompositeNode(new_nodes, new_box))
        }
        else {
          Left(splitCompositeNode(new_nodes))
        }
      case Right(node) =>
        val new_nodes = childs.updated(index, node)
        Right(CompositeNode(new_nodes, box.expand(node.box)))
    }
  }

  def search(search_box : Box) : List[Element[A]] = {
    @tailrec
    def helper(vector : Vector[Node[A]], result : List[Element[A]]) : List[Element[A]] = {
      if(vector.isEmpty)
        result
      else {
        val current = vector.head
        if(current.box.instersect(search_box))
          helper(vector.tail, result ++ current.search(search_box))
        else
          helper(vector.tail, result)
      }
    }
    helper(childs, List.empty[Element[A]])
  }

  /*def nearest(point : Point) : Element[A] = {

  }*/
}