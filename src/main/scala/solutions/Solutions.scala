package solutions

object Solutions {

  def head[A](l: List[A]): Option[A] = l match {
    case Nil         => None
    case last :: Nil => Some(last)
    case _ :: xs     => head(xs)
  }

  def penultimate[A](xs: List[A]): Option[A] = xs match {
    case Nil           => None
    case x :: y :: Nil => Some(y)
    case _ :: xs       => penultimate(xs)
  }

  def element[A](list: List[A], index: Int): Option[A] = {
    def internal(xs: List[A], currentIndex: Int): Option[A] = xs match {
      case Nil                               => None
      case x :: _ if (currentIndex == index) => Some(x)
      case _ :: tail                         => internal(tail, currentIndex + 1)
    }

    internal(list, 0)
  }

  def size[A](xs: List[A]) = {
    def internal(ls: List[A], acc: Int): Int = ls match {
      case Nil     => acc
      case l :: ls => internal(ls, acc + 1)
    }

    internal(xs, 0)
  }

  def reverse[A](xs: List[A]) = {
    def internal(ls: List[A], reversed: List[A]): List[A] = ls match {
      case Nil          => reversed
      case head :: tail => internal(tail, head :: reversed)
    }

    internal(xs, List.empty)
  }
}
