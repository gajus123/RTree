import org.scalatest.FunSuite

class BoxTest extends  FunSuite {

  test("when box intersect other box, then Box.intersect returns true "){
    val box = Box(0.1F, 0.3F, 0.5F, 0.7F)
    val otherBox =  Box(0.2F, 0.3F, 0.4F, 0.7F)

    assert( box.instersect(otherBox) )
  }

  test("when box doesn't intersect other box, then Box.intersect returns flase " ){
    val box = Box(0.1F, 0.3F, 0.5F, 0.7F)
    val otherBox =  Box(0.5F, 0.6F, 0.1F, 0.2F)

    assert( !box.instersect(otherBox) )
  }

  test("Box.empty doesn't intersect any box" ){
    val box = Box(0.1F, 0.3F, 0.5F, 0.7F)
    val otherBox =  Box(0.5F, 0.6F, 0.1F, 0.2F)

    assert( !Box.empty.instersect(box) )
    assert( !Box.empty.instersect(otherBox) )
  }

  test("box expands correctly " ){
    val box = Box(0.1F, 0.3F, 0.5F, 0.7F)
    val otherBox =  Box(0.5F, 0.6F, 0.1F, 0.2F)

    assert( box.expand(otherBox) === Box(0.1F, 0.6F, 0.1F, 0.7F) )
  }

  test("box calculate expand correctly " ){
    val box = Box(0.0F, 0.5F, 0.0F, 0.5F)
    val otherBox =  Box(0.5F, 0.6F, 0.5F, 0.6F)

    assert( box.calcExpand(otherBox) ===  ( Box(0.0F, 0.6F, 0.0F, 0.6F).area - box.area ) )
  }

  test("box doesn't expand when it is bigger than the other box " ){
    val box = Box(0.0F, 0.5F, 0.0F, 0.5F)
    val otherBox =  Box(0.1F, 0.2F, 0.3F, 0.4F)

    assert( box.expand(otherBox) === box )
  }
  test("box expand is equal to 0 when box doesn't expand " ){
    val box = Box(0.0F, 0.5F, 0.0F, 0.5F)
    val otherBox =  Box(0.1F, 0.2F, 0.3F, 0.4F)

    assert( box.calcExpand(otherBox) ===  0.0F )
  }

  test("empty box expands correctly" ){
    val box = Box(0.0F, 0.5F, 0.0F, 0.5F)

    assert( Box.empty.expand(box) === box )
  }



}
