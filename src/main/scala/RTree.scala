import scala.annotation.tailrec

case class RTree[A](root: Node[A]) {
  //Required
  def insert(element: Element[A]): RTree[A] = {
    val result = root.insert(element)
    val new_root = result match {
      case Left(nodes) => {
        val new_box = nodes.foldLeft(Box.empty)((first : Box, second : Node[A]) => {
          first.expand(second.box)
        })
        CompositeNode(nodes, new_box)
      }
      case Right(node) => node
    }
    RTree[A](new_root)
  }

  def remove(element: Element[A]): RTree[A] = {
    val result = root.remove(element)

    val newTree : RTree[A] = result._2 match {
      case None => RTree.empty[A]
      case Some(newNode) => RTree[A](newNode)
    }

    newTree.insert(result._1)

  };

  def search(box : Box) : List[Element[A]] = {
    root.search(box)
  }

  //Additional
  @tailrec
  final def insert(elements: Seq[Element[A]]): RTree[A] = {
    if (elements.isEmpty)
      this
    else {
      val temp : RTree[A] = insert(elements.head)
      temp.insert(elements.tail)
    }
  }

  //Helper
  def print : String = root.print
}

object RTree {
  def empty[A] : RTree[A] = new RTree[A](Node.empty[A])
}