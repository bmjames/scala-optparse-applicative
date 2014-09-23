package net.bmjames.opts

package object internal {

  def uncons[A](xs: List[A]): Option[(A, List[A])] =
    xs match {
      case Nil   => None
      case x::xs => Some((x, xs))
    }

  def min[A](a1: A, a2: A)(implicit A: Ordering[A]): A =
    A.min(a1, a2)
}
