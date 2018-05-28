import org.scalatest.FunSuite


class RTreeTest extends FunSuite {

  test("when tree is empty and element is inserted, tree contains it "){
    val element = Element(Box(0.1F, 0.3F, 0.5F, 0.7F), 3)
    var tree = RTree.empty[Int]

    tree = tree.insert(element)

    assert(tree.search(element.box).contains(element) === true )
  }

  test("when tree is empty and we search for element then, serach resturns empty list"){
    val element = Element(Box(0.1F, 0.3F, 0.5F, 0.7F), 3)
    var tree = RTree.empty[Int]


    assert(tree.search(element.box).isEmpty )
  }

  test("when list of elements is inserted, tree contains them "){
    val element1 = Element(Box(0.1F, 0.3F, 0.5F, 0.7F), 3)
    val element2 = Element(Box(0.9F, 0.99F, 0.1F, 0.2F), 6)
    val element3 = Element(Box(0.4F, 0.53F, 0.78F, 0.97F), 1)
    val elementList = List(element1, element2, element3 )
    var tree = RTree.empty[Int]

    tree = tree.insert(elementList)

    assert(tree.search(element1.box).contains(element1)  )
    assert(tree.search(element2.box).contains(element2)  )
    assert(tree.search(element3.box).contains(element3)  )
  }

  test("when some intersecting elements are inserted, tree.search contains all of them "){
    val element1 = Element(Box(0.1F, 0.5F, 0.3F, 0.7F), 3)
    val element2 = Element(Box(0.2F, 0.43F, 0.34F, 0.65F), 6)
    val element3 = Element(Box(0.365F, 0.462F, 0.68F, 0.77F), 1)
    val elementList = List(element1, element2, element3 )
    var tree = RTree.empty[Int]

    tree = tree.insert(element1)
    tree = tree.insert(element2)
    tree = tree.insert(element3)

    val searchResult = tree.search(element1.box)
    assert(searchResult.contains(element1))
    assert(searchResult.contains(element2))
    assert(searchResult.contains(element3))

  }

  test("when list of intersecting elements is inserted, tree.search contains all of them "){
    val element1 = Element(Box(0.1F, 0.5F, 0.3F, 0.7F), 3)
    val element2 = Element(Box(0.2F, 0.43F, 0.34F, 0.65F), 6)
    val element3 = Element(Box(0.365F, 0.462F, 0.68F, 0.77F), 1)
    val elementList = List(element1, element2, element3 )
    var tree = RTree.empty[Int]

    tree = tree.insert(elementList)

    val searchResult = tree.search(element1.box)
    assert(searchResult.contains(element1))
    assert(searchResult.contains(element2))
    assert(searchResult.contains(element3))

  }

  test("when element is removed, tree no longer contains it "){
    val element = Element(Box(0.1F, 0.3F, 0.5F, 0.7F), 3)
    var tree = RTree.empty[Int]

    tree = tree.insert(element)

    tree = tree.remove(element)

    assert(tree.search(element.box).contains(element) === false )
  }

  test("when element is removed, and we search for this element, search result is empty "){
    val element = Element(Box(0.1F, 0.3F, 0.5F, 0.7F), 3)
    var tree = RTree.empty[Int]

    tree = tree.insert(element)

    tree = tree.remove(element)

    assert(tree.search(element.box).isEmpty)
  }

}
