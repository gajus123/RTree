import scala.annotation.tailrec
import scala.collection.immutable.Vector

/** Abstract class for representing node
  *
  * @constructor
  * @param box minimum bounding rectangle of elements below that node
  * @tparam A data type stored in the node
  */
abstract class Node[A](val box : Box) extends HasBox {
  val maxNodeSize : Int = 4

  /** Inserts new element into the tree
    *
    * @param element to be inserted into tree
    * @return either node or vector of nodes after insertion
    */
  def insert(element: Element[A]): Either[Vector[Node[A]], Node[A]]

  /** Removes given element from the node
    *
    * @param element to be removed from tree
    * @return List of elements to readd to the tree and node recreate tree from
    */
  def remove(element: Element[A]): (List[Element[A]],Option[Node[A]])

  /** Returns list of elements, which minimum bounding rectangle is contained in given minimum bounding rectangle
    *
    * @param search_box minimum bounding rectangle to search elements in
    */
  def search(search_box : Box) : List[Element[A]]

  /** Returns list of elements, that are below this node
    *
    */
  def elements : List[Element[A]]

  /** Print node
    *
    * @param spaces number of spaces on line begin
    */
  def printNode(spaces : Int) : Unit

  /** Redistributes leaf node children into newly created nodes
    *
    * @param vector of leaf node's children (elements)
    * @return vector of newly created nodes with given elements split between them
    */
  def splitLeafNode(vector : Vector[Element[A]]) : Vector[LeafNode[A]] = {
    val ((v1, b1), (v2, b2)) = Splitter.splitNode(vector)
    Vector(LeafNode(v1, b1), LeafNode(v2, b2))
  }

  /** Redistributes composite node children into newly created nodes
    *
    * @param vector of composite node's children (nodes)
    * @return vector of newly created nodes with given nodes split between them
    */
  def splitCompositeNode(vector : Vector[Node[A]]) : Vector[CompositeNode[A]] = {
    val ((v1, b1), (v2, b2)) = Splitter.splitNode(vector)
    Vector(CompositeNode(v1, b1), CompositeNode(v2, b2))
  }
}

object Node {
  /** Creates an empty tree
    *
    * @tparam A stored data type
    */
  def empty[A] : Node[A] = LeafNode[A](Vector.empty[Element[A]], Box.empty)

  def getBestNodeAndIndex[A](collection : Seq[Node[A]], box : Box) : (Node[A], Int) = {
    collection.zipWithIndex.minBy[Float]((variable : (Node[A], Int)) => {
      variable._1.box.calcExpand(box)
    })
  }
}

/** Class representing leaf node of rtree
  *
  * @constructor
  * @param childs vector of elements contained in leaf
  * @param box minimum bounding rectangle of elements in this leaf node
  * @tparam A data type stored in the node
  */
case class LeafNode[A](childs : Vector[Element[A]], override val box: Box) extends Node[A](box) {
  def printNode(spaces : Int) : Unit = {
    for (i <- 1 to spaces) { print(" ") }
    println("leaf " + box)
    childs.foreach((child : Element[A]) => {
      for (i <- 1 to spaces + 2) { print(" ") }
      println(child.box + " " + child.value)
    })
  }
  /** Inserts new element into the tree
    *
    * The structure is immutable, so method returns new node to replace the current node.
    * It can either return a single node to replace when node contains less then maxNodeSize elements,
    * or if node already contains more then maxNodeSize elements it will split this node into two other
    * and redistribute elements between them using Quadratic Split Node algorithm and return vector of nodes.
    *
    * @param element to be inserted into tree
    * @return either node or vector of nodes after insertion
    */
  def insert(element: Element[A]): Either[Vector[Node[A]], Node[A]] = {
    val new_childs : Vector[Element[A]] = childs :+ element
    if(new_childs.size <= maxNodeSize)
      Right(LeafNode(new_childs, box.expand(element.box)))
    else
      Left(splitLeafNode(new_childs))
  }

  /** Returns list of elements, which minimum bounding rectangle is contained in given minimum bounding rectangle
    *
    * @param search_box minimum bounding rectangle to search elements in
    */
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

  /** Removes given element from the node
    *
    * Method returns list of element to insert into new tree,and optional a head node from which new tree is created.
    * If the element was not found it this node empty list and this node is returned,
    * and if the element was removed and after removal the node has less then maxNodeSize / 2 children it need to be removed
    * and its children readded ,so List of elements to readd to the tree and None is returned,
    * and if the element was removed and after removal the node has more then maxNodeSize / 2 children it needs to be replaced,
    * so minimal bounding are is expanded and empty List and new node to replace current with is returned.
    *
    * @param element to be removed from tree
    * @return List of elements to readd to the tree and node recreate tree from
    */
  def remove(element: Element[A]): (List[Element[A]],Option[Node[A]]) = {
    val index = childs.indexOf(element)
    if( index < 0 ) {
      (List.empty[Element[A]], Some(this))
    }
    else {
      val newChilds = childs.take(index) ++ childs.drop(index + 1)
      if( newChilds.size < 2 ) {
        (newChilds.toList, None)
      }
      else {
        val newBox = newChilds.foldLeft(Box.empty)(_ expand _.box)
        (List.empty[Element[A]], Some(LeafNode(newChilds, newBox)))
      }
    }
  }

  /** Returns list of elements, that are in this leaf node
    *
    */
  def elements : List[Element[A]] = {
    childs.toList
  }

}

/** Class representing composite node
  *
  * @constructor
  * @param childs vector of nodes to be contained in this composite node
  * @param box minimum bounding rectangle of elements below this composite node
  * @tparam A data type stored in the node
  */
case class CompositeNode[A](childs : Vector[Node[A]], override val box: Box) extends Node[A](box) {
  def printNode(spaces : Int) : Unit = {
    for (i <- 1 to spaces) { print(" ")}
    println("composite " + box)
    childs.foreach((child : Node[A]) => {
      child.printNode(spaces + 2)
    })
  }

  /** Inserts new element into the tree
    *
    * Method finds best fitting node among its children to insert and call insert method on that node.
    * If insert call returns a single node, then previously found node is replaced with one returned,
    * and if insert call returns vector of nodes, then previously found best node is replaced with that vector
    * and if node contains less then maxNodeSize elements current node minimum bounding rectangle is expanded,
    * else if after inserting vector node contains more then maxNodeSize elements it will split this node into two other
    * and redistribute elements between them using Quadratic Split Node algorithm.
    *
    * @param element to be inserted into tree
    * @return either node or vector of nodes after insertion
    */
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

  /** Search new element, which minimum bounding rectangle is contained in given minimum bounding rectangle.
    *
    * @param search_box minimum bounding rectangle to search elements in
    * @return list of elements below that node, which minimum bounding rectangle is contained in given minimum bounding rectangle
    */
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

  /** Removes given element from the node
    *
    * Method returns list of element to insert into new tree,and optional a head node from which new tree is created.
    * If the element was not found it this node empty list and this node is returned,
    * @param element to be removed from tree
    * @return List of elements to readd to the tree and node recreate tree from
    */
  def remove(element: Element[A]): (List[Element[A]],Option[Node[A]]) = {
    if( !box.instersect(element.box) ) {
      (List.empty[Element[A]], Some(this) )
    }
    else {
      @tailrec
      def helper(vector : Vector[Node[A]], newChilds : Vector[Node[A]], toAdd : List[Element[A]]) : (Vector[Node[A]], List[Element[A]]) = {
        if(vector.isEmpty)
          (newChilds, toAdd)
        else {
          val newNode = vector.head.remove(element)
          newNode._2 match {
            case None =>
              helper(vector.tail, newChilds, toAdd ++ newNode._1)
            case Some(node) =>
              helper(vector.tail, newChilds :+ node, toAdd ++ newNode._1)
          }
        }
      }
      val result = helper(childs, Vector.empty[Node[A]], List.empty[Element[A]])
      val newChilds = result._1
      val toAdd = result._2
      if(newChilds.size < 2) {
        def elements(childs : Vector[Node[A]]) : List[Element[A]] = {
          if(childs.isEmpty)
            List.empty[Element[A]]
          else {
            childs.foldLeft(List.empty[Element[A]])(_ ++ _.elements)
          }
        }
        (toAdd ++ elements(newChilds), None)
      }
      else {
        val newBox = newChilds.foldLeft(Box.empty)(_ expand _.box)
        (toAdd, Some(CompositeNode(newChilds, newBox)))
      }
    }
  }

  /** Returns list of elements, that are below this composite node
    *
    */
  def elements : List[Element[A]] = {
    if(childs.isEmpty)
      List.empty[Element[A]]
    else {
      childs.foldLeft(List.empty[Element[A]])(_ ++ _.elements)
    }
  }
}