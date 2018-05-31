import scala.annotation.tailrec

/** RTree implmentation
  *
  * @constructor creates new rtree with a root node
  * @param root A root node of the tree
  * @tparam A data type stored in the tree
  */
case class RTree[A](root: Node[A]) {
  /** Inserts an element into the tree by creating new one
    *
    * @param element to insert containing minimum bounding rectangle and stored value
    * @return a new tree with an element inserted
    */
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

  /** Removes an element from the tree
    *
    * @param element to remove containing minimum bounding rectangle and stored value
    * @return a new tree with given element removed
    */
  def remove(element: Element[A]): RTree[A] = {
    val result = root.remove(element)

    val newTree : RTree[A] = result._2 match {
      case None => RTree.empty[A]
      case Some(newNode) => RTree[A](newNode)
    }

    newTree.insert(result._1)

  }

  /** Returns list of elements, which minimum bounding rectangle is contained in given minimum bounding rectangle
    *
    * @param box minimum bounding rectangle to search elements in
    */
  def search(box : Box) : List[Element[A]] = {
    root.search(box)
  }

  /** Inserts list of elements into the tree
    *
    * @param elements sequence of elements to insert containing minimum bounding rectangle and stored value
    * @return a new tree with list of elements inserted
    */
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
  /** Creates an empty tree
    *
    * @tparam A stored data type
    */
  def empty[A] : RTree[A] = new RTree[A](Node.empty[A])
}