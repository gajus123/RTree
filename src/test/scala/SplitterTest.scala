import scala.collection.immutable.Vector
import org.scalatest.FunSuite

class SplitterTest extends FunSuite {

  test("pick seeds returns nodes with biggest area subtract"){
    val vector = Vector.empty[Element[Int]] :+ Element[Int](Box(0.0F, 0.1F, 0.0F, 0.1F), 1) :+ Element[Int](Box(1.0F, 1.1F, 1.0F, 1.1F), 2) :+ Element[Int](Box(0.5F, 0.6F, 0.5F, 0.6F), 3)

    val tuple = Splitter.pickSeeds(vector)
    assert(tuple._1.size === 1)
    assert(tuple._2.box === Box(0.0F, 0.1F, 0.0F, 0.1F))
    assert(tuple._3.box === Box(1.0F, 1.1F, 1.0F, 1.1F))
  }

  test("pick next returns node which expands box the most"){
    val vector = Vector.empty[Element[Int]] :+ Element[Int](Box(-0.2F, 0.2F, -0.2F, 0.2F), 1) :+ Element[Int](Box(0.4F, 0.6F, 1.0F, 3.0F), 2)
    val box1 = Box(0.0F, 1.0F, 0.0F, 1.0F)
    val box2 = Box(0.1F, 1.0F, 0.1F, 1.0F)

    val pair = Splitter.pickNext(vector, box1, box2)

    assert(pair._1.box === Box(0.4F, 0.6F, 1.0F, 3.0F))
    assert(pair._2.size === 1)
  }
}