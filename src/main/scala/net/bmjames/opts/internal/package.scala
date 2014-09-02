package net.bmjames.opts

package object internal {

  def uncons[A](xs: List[A]): Option[(A, List[A])] =
    xs match {
      case Nil   => None
      case x::xs => Some((x, xs))
    }

}
