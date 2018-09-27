package solutions

object Solutions {
  def head[A](l: List[A]): Option[A] = l match {
    case Nil         => None
    case last :: Nil => Some(last)
    case _ :: xs     => head(xs)
  }

  def penultimate[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case x:: y:: Nil => Some(y)
    case _ :: xs => penultimate(xs)
  }
}
