package solutions

object Solutions {

  def head[A](l: List[A]): Option[A] = l match {
    case Nil         => None
    case last :: Nil => Some(last)
    case _ :: xs     => head(xs)
  }

}
