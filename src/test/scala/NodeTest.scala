import org.scalatest.FunSuite

class NodeTest extends  FunSuite {

  test("when leaf node has 2 elements and element is inserted, then box expands correctly and single node containing inserted elements is returned "){
    val box = Box(0.1F, 0.4F, 0.3F, 0.7F)
    val element1 = Element(Box(0.1F, 0.2F, 0.3F, 0.4F), 3)
    val element2 = Element(Box(0.3F, 0.4F, 0.4F, 0.7F), 35)
    val element3 = Element(Box(0.1F, 0.4F, 0.2F, 0.5F), 3356)

    val vec = Vector (element1, element2 )

    val leafNode: LeafNode[Int] = LeafNode(vec, box)

    val result = leafNode.insert(element3)
    assert( result.isRight )
    assert( result.right.get.box === Box(0.1F, 0.4F, 0.2F, 0.7F) )
  }

  test("when leaf node has 2 elements and element is inserted, then  single node containing inserted elements is returned "){
    val box = Box(0.1F, 0.4F, 0.3F, 0.7F)
    val element1 = Element(Box(0.1F, 0.2F, 0.3F, 0.4F), 3)
    val element2 = Element(Box(0.3F, 0.4F, 0.4F, 0.7F), 35)
    val element3 = Element(Box(0.1F, 0.4F, 0.2F, 0.5F), 3356)

    val vec = Vector (element1, element2 )

    val leafNode: LeafNode[Int] = LeafNode(vec, box)

    val result = leafNode.insert(element3)
    assert( result.isRight )
    assert( result.right.get.elements === List (element1, element2,element3 ) )
  }

  test("when leaf node has maxNodeSize elements and element is inserted, then vector of two nodes "){
    val element1 = Element(Box(0.1F, 0.2F, 0.3F, 0.4F), 3)
    val element2 = Element(Box(0.3F, 0.4F, 0.4F, 0.7F), 35)
    val element3 = Element(Box(0.1F, 0.4F, 0.2F, 0.5F), 3356)
    val element4 = Element(Box(0.1F, 0.14F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.32F, 0.4F, 0.21F, 0.7F), 556)

    val vec = Vector (element1, element2, element3, element4 )

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.1F, 0.4F, 0.2F, 0.7F))

    val result = leafNode.insert(element5)
    assert( result.isLeft )
    assert( result.left.get.size === 2 )
  }

  test("when leaf node has maxNodeSize elements and element is inserted, then vector of two nodes containing inserted elements is returned "){
    val element1 = Element(Box(0.1F, 0.2F, 0.3F, 0.4F), 3)
    val element2 = Element(Box(0.3F, 0.4F, 0.4F, 0.7F), 35)
    val element3 = Element(Box(0.1F, 0.4F, 0.2F, 0.5F), 3356)
    val element4 = Element(Box(0.1F, 0.14F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.32F, 0.4F, 0.21F, 0.7F), 556)

    val vec = Vector (element1, element2, element3, element4 )

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.1F, 0.4F, 0.2F, 0.7F))

    val result = leafNode.insert(element5)
    assert( result.isLeft )
    assert( result.left.get.head.elements === List(element2, element5 ) )
    assert( result.left.get.last.elements === List(element4, element1, element3))
  }

  test("when searching leaf node, then correct list of nodes contained in given box is returned "){
    val element1 = Element(Box(0.4F, 0.52F, 0.34F, 0.43F), 1)
    val element2 = Element(Box(0.5F, 0.63F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1F, 0.7F, 0.2F, 0.53F), 36)
    val element4 = Element(Box(0.1F, 0.14F, 0.234F, 0.36F), 23196)


    val vec = Vector (element1, element2, element3, element4 )

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.1F, 0.7F, 0.2F, 0.7F))

    val result = leafNode.search(Box(0.01F, 0.743F, 0.1F, 0.8677F))

    assert(result.size === 4)
    assert(result === List(element1, element2, element3, element4 ) )
  }

  test("when searching leaf node and searched box doesn't intersect any elements, then empty list of nodes is returned "){
    val element1 = Element(Box(0.4F, 0.52F, 0.34F, 0.43F), 1)
    val element2 = Element(Box(0.5F, 0.63F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1F, 0.7F, 0.2F, 0.53F), 36)
    val element4 = Element(Box(0.1F, 0.14F, 0.234F, 0.36F), 23196)


    val vec = Vector (element1, element2, element3, element4 )

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.1F, 0.7F, 0.2F, 0.7F))

    val result = leafNode.search(Box(0.781F, 0.843F, 0.897F, 0.9677F))

    assert(result.isEmpty )
  }

  test("when removing element from leaf node that isn't present in the node, then empty list and this node is returned "){
    val element1 = Element(Box(0.4F, 0.52F, 0.34F, 0.43F), 1)
    val element2 = Element(Box(0.5F, 0.63F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1F, 0.7F, 0.2F, 0.53F), 36)
    val element4 = Element(Box(0.1F, 0.14F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.1F, 0.11224F, 0.24334F, 0.98736F), 7)

    val vec = Vector (element1, element2, element3, element4 )

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.1F, 0.7F, 0.2F, 0.7F))

    val result = leafNode.remove(element5)

    assert(result._1 === List.empty)
    assert(result._2.get === leafNode)
  }

  test("when removing element from leaf node and after removing node has less then maxNodeSize/2 elements, then list of elements and None is returned "){
    val element1 = Element(Box(0.4F, 0.52F, 0.34F, 0.43F), 1)
    val element2 = Element(Box(0.5F, 0.63F, 0.42F, 0.7F), 5)


    val vec = Vector (element1, element2)

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.1F, 0.7F, 0.2F, 0.7F))

    val result = leafNode.remove(element1)

    assert(result._1 === List(element2))
    assert(result._2 === None)
  }

  test("when removing element from leaf node and after removing node has more then maxNodeSize/2 elements, then empty list and updated node is returned "){
    val element1 = Element(Box(0.4F, 0.52F, 0.34F, 0.43F), 1)
    val element2 = Element(Box(0.5F, 0.63F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1F, 0.7F, 0.2F, 0.53F), 36)
    val element4 = Element(Box(0.1F, 0.14F, 0.234F, 0.36F), 23196)

    val vec = Vector (element1, element2, element3, element4 )

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.1F, 0.7F, 0.2F, 0.7F))

    val result = leafNode.remove(element1)

    assert(result._1 === List.empty)
    assert(result._2.get === LeafNode(Vector( element2, element3, element4), Box(0.1F, 0.7F, 0.2F, 0.7F) ))
  }

  test("when removing element from leaf node and after removing node has more then maxNodeSize/2 elements, then updated node box in adjusted "){
    val element1 = Element(Box(0.4F, 0.52F, 0.34F, 0.43F), 1)
    val element2 = Element(Box(0.5F, 0.63F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1F, 0.7F, 0.2F, 0.53F), 36)
    val element4 = Element(Box(0.1F, 0.14F, 0.234F, 0.36F), 23196)

    val vec = Vector (element1, element2, element3, element4 )

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.1F, 0.7F, 0.2F, 0.7F))

    val result = leafNode.remove(element3)

    assert(result._1 === List.empty)
    assert(result._2.get.box ===  Box(0.1F, 0.63F, 0.234F, 0.7F)  )
  }

  test("when getting elements from leaf node, then all contained elements are returned "){
    val element1 = Element(Box(0.4F, 0.52F, 0.41323F, 0.43F), 1)
    val element2 = Element(Box(0.23F, 0.343F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1123F, 0.7F, 0.546F, 0.53F), 36)
    val element4 = Element(Box(0.678F, 0.9764F, 0.234F, 0.36F), 23196)

    val vec = Vector (element1, element2, element3, element4 )

    val leafNode: LeafNode[Int] = LeafNode(vec, Box(0.11231F, 0.9764F, 0.234F, 0.7F))

    val result = leafNode.elements

    assert(result.size === 4)
    assert(result === List(element1, element2, element3, element4 )  )
  }

  test("when getting elements from composite node, then all contained elements are returned "){
    val element1 = Element(Box(0.4F, 0.52F, 0.41323F, 0.43F), 1)
    val element2 = Element(Box(0.23F, 0.343F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1123F, 0.7F, 0.546F, 0.53F), 36)
    val element4 = Element(Box(0.678F, 0.9764F, 0.234F, 0.36F), 23196)

    val leafNode1: LeafNode[Int] = LeafNode(Vector (element1, element2 ), Box(0.23F, 0.52F, 0.41323F, 0.7F))

    val leafNode2: LeafNode[Int] = LeafNode(Vector (element3, element4 ), Box(0.11231F, 0.9764F, 0.234F, 0.53F))

    val compositeNode : CompositeNode[Int] = CompositeNode(Vector(leafNode1,leafNode2), Box(0.11231F, 0.9764F, 0.234F, 0.7F) )

    val result = compositeNode.elements

    assert(result.size === 4)
    assert(result === List(element1, element2, element3, element4 )  )
  }

  test("when inserting elements from composite node, then expanded node is returned returned "){
    val element1 = Element(Box(0.4F, 0.52F, 0.41323F, 0.43F), 1)
    val element2 = Element(Box(0.23F, 0.343F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1123F, 0.7F, 0.546F, 0.53F), 36)
    val element4 = Element(Box(0.678F, 0.9764F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.1F, 0.2F, 0.2F, 0.3F), 7)

    val leafNode1: LeafNode[Int] = LeafNode(Vector (element1, element2 ), Box(0.23F, 0.52F, 0.41323F, 0.7F))

    val leafNode2: LeafNode[Int] = LeafNode(Vector (element3, element4 ), Box(0.11231F, 0.9764F, 0.234F, 0.53F))

    val compositeNode : CompositeNode[Int] = CompositeNode(Vector(leafNode1,leafNode2), Box(0.11231F, 0.9764F, 0.234F, 0.7F) )

    val result = compositeNode.insert(element5)

    assert(result.isRight)
    assert(result.right.get.box === Box(0.1F, 0.9764F, 0.2F, 0.7F) )
  }

  test("when inserting elements from composite node and leaf node splits, then new nodes are added and box expands "){
    val element1 = Element(Box(0.4F, 0.52F, 0.41323F, 0.43F), 1)
    val element2 = Element(Box(0.23F, 0.343F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1123F, 0.7F, 0.546F, 0.53F), 36)
    val element4 = Element(Box(0.678F, 0.9764F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.8F, 0.88F, 0.44F, 0.5F), 42)
    val element6 = Element(Box(0.34F, 0.512F, 0.25F, 0.3F), 17)
    val element7 = Element(Box(0.1F, 0.2F, 0.2F, 0.3F), 7)

    val leafNode1: LeafNode[Int] = LeafNode(Vector (element1, element2 ), Box(0.23F, 0.52F, 0.41323F, 0.7F))

    val leafNode2: LeafNode[Int] = LeafNode(Vector (element3, element4, element5, element6 ), Box(0.11231F, 0.9764F, 0.234F, 0.53F))

    val compositeNode : CompositeNode[Int] = CompositeNode(Vector(leafNode1,leafNode2), Box(0.11231F, 0.9764F, 0.234F, 0.7F) )

    val result = compositeNode.insert(element7)

    assert(result.isRight)
    assert(result.right.get.box === Box(0.1F, 0.9764F, 0.2F, 0.7F) )
  }

  test("when inserting elements from composite node and composite node node splits, then maxNodeSize / 2 nodes are returned"){
    val element1 = Element(Box(0.4F, 0.52F, 0.41323F, 0.43F), 1)
    val element2 = Element(Box(0.23F, 0.343F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1123F, 0.7F, 0.546F, 0.53F), 36)
    val element4 = Element(Box(0.678F, 0.9764F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.8F, 0.88F, 0.44F, 0.5F), 42)
    val element6 = Element(Box(0.34F, 0.512F, 0.25F, 0.3F), 17)
    val element7 = Element(Box(0.81F, 0.9F, 0.4F, 0.5F), 7)
    val element8 = Element(Box(0.007F, 0.09F, 0.88F, 0.9F), 32)
    val element9 = Element(Box(0.01F, 0.1F, 0.801F, 0.88F), 654)

    val leafNode1: LeafNode[Int] = LeafNode(Vector (element1, element2 ), Box(0.23F, 0.52F, 0.41323F, 0.7F))

    val leafNode2: LeafNode[Int] = LeafNode(Vector (element3, element4, element5, element6 ), Box(0.11231F, 0.9764F, 0.234F, 0.53F))

    val leafNode3: LeafNode[Int] = LeafNode(Vector (element8, element9 ), Box(0.007F, 0.1F, 0.801F, 0.9F))

    val leafNode4: LeafNode[Int] = LeafNode(Vector ( ), Box(0.004F, 0.5F, 0.85F, 0.943F))

    val compositeNode : CompositeNode[Int] = CompositeNode(Vector(leafNode1,leafNode2, leafNode3, leafNode4), Box(0.007F, 0.9764F, 0.234F, 0.9F) )

    val result = compositeNode.insert(element7)

    assert(result.isLeft)
    assert(result.left.get.size === 2 )
  }

  test("when searching elements from composite node, then list nodes below is returned"){
    val element1 = Element(Box(0.4F, 0.52F, 0.41323F, 0.43F), 1)
    val element2 = Element(Box(0.23F, 0.343F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1123F, 0.7F, 0.546F, 0.53F), 36)
    val element4 = Element(Box(0.678F, 0.9764F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.8F, 0.88F, 0.44F, 0.5F), 42)
    val element6 = Element(Box(0.34F, 0.512F, 0.25F, 0.3F), 17)
    val element8 = Element(Box(0.007F, 0.09F, 0.88F, 0.9F), 32)
    val element9 = Element(Box(0.01F, 0.1F, 0.801F, 0.88F), 654)

    val leafNode1: LeafNode[Int] = LeafNode(Vector (element1, element2 ), Box(0.23F, 0.52F, 0.41323F, 0.7F))

    val leafNode2: LeafNode[Int] = LeafNode(Vector (element3, element4, element5, element6 ), Box(0.11231F, 0.9764F, 0.234F, 0.53F))

    val leafNode3: LeafNode[Int] = LeafNode(Vector (element8, element9 ), Box(0.007F, 0.1F, 0.801F, 0.9F))

    val leafNode4: LeafNode[Int] = LeafNode(Vector ( ), Box(0.004F, 0.5F, 0.85F, 0.943F))

    val compositeNode : CompositeNode[Int] = CompositeNode(Vector(leafNode1,leafNode2, leafNode3, leafNode4), Box(0.007F, 0.9764F, 0.234F, 0.9F) )

    val result = compositeNode.search( Box(0.1F, 0.6F, 0.2F, 0.7F) )

    assert(result.size === 4)

    assert(result === List( element1, element2, element3, element6))

  }

  test("when removing elements from composite node and element isn't present, then empty list and this composite node is returned"){
    val element1 = Element(Box(0.4F, 0.52F, 0.41323F, 0.43F), 1)
    val element2 = Element(Box(0.23F, 0.343F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1123F, 0.7F, 0.546F, 0.53F), 36)
    val element4 = Element(Box(0.678F, 0.9764F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.8F, 0.88F, 0.44F, 0.5F), 42)
    val element6 = Element(Box(0.34F, 0.512F, 0.25F, 0.3F), 17)
    val element7 = Element(Box(0.81F, 0.9F, 0.4F, 0.5F), 7)
    val element8 = Element(Box(0.007F, 0.09F, 0.88F, 0.9F), 32)
    val element9 = Element(Box(0.01F, 0.1F, 0.801F, 0.88F), 654)

    val leafNode1: LeafNode[Int] = LeafNode(Vector (element1, element2 ), Box(0.23F, 0.52F, 0.41323F, 0.7F))

    val leafNode2: LeafNode[Int] = LeafNode(Vector (element3, element4, element5, element6 ), Box(0.11231F, 0.9764F, 0.234F, 0.53F))

    val leafNode3: LeafNode[Int] = LeafNode(Vector (element8, element9 ), Box(0.007F, 0.1F, 0.801F, 0.9F))


    val compositeNode : CompositeNode[Int] = CompositeNode(Vector(leafNode1,leafNode2, leafNode3), Box(0.007F, 0.9764F, 0.234F, 0.9F) )

    val result = compositeNode.remove(element7)

    assert(result._1 === List.empty)
    assert(result._2.get === compositeNode)

  }

  test("when removing elements from composite node, then empty list and this composite node is returned"){
    val element1 = Element(Box(0.4F, 0.52F, 0.41323F, 0.43F), 1)
    val element2 = Element(Box(0.23F, 0.343F, 0.42F, 0.7F), 5)
    val element3 = Element(Box(0.1123F, 0.7F, 0.546F, 0.53F), 36)
    val element4 = Element(Box(0.678F, 0.9764F, 0.234F, 0.36F), 23196)
    val element5 = Element(Box(0.8F, 0.88F, 0.44F, 0.5F), 42)
    val element6 = Element(Box(0.34F, 0.512F, 0.25F, 0.3F), 17)
    val element7 = Element(Box(0.81F, 0.9F, 0.4F, 0.5F), 7)
    val element8 = Element(Box(0.007F, 0.09F, 0.88F, 0.9F), 32)
    val element9 = Element(Box(0.01F, 0.1F, 0.801F, 0.88F), 654)

    val leafNode1: LeafNode[Int] = LeafNode(Vector (element1, element2 ), Box(0.23F, 0.52F, 0.41323F, 0.7F))

    val leafNode2: LeafNode[Int] = LeafNode(Vector (element3, element4, element5, element6 ), Box(0.11231F, 0.9764F, 0.234F, 0.53F))

    val leafNode3: LeafNode[Int] = LeafNode(Vector (element8, element9 ), Box(0.007F, 0.1F, 0.801F, 0.9F))


    val compositeNode : CompositeNode[Int] = CompositeNode(Vector(leafNode1,leafNode2, leafNode3), Box(0.007F, 0.9764F, 0.234F, 0.9F) )

    val result = compositeNode.remove(element7)

    assert(result._1 === List.empty)
    assert(result._2.get === compositeNode)

  }
}
