object App {
  def main(args: Array[String]): Unit = {
    var drzewo = RTree.empty[Int]
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    drzewo = drzewo.insert(Element(Box(0.2F, 0.4F, 1.0F, 1.1F), 1))
    print(drzewo.print)
  }
}
