trait HasBox {
  def box : Box
}

case class Element[A](box : Box, value : A) extends HasBox