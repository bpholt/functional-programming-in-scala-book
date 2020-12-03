package fpinscala.testing

object ** {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
}
