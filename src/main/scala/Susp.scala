package stroganoff

sealed trait Susp[A] {
  def map[B](f: A => B): Susp[B] =
    flatMap(x => Result(f(x)))
  def flatMap[B](f: A => Susp[B]): Susp[B]
}
  
case class Cont[A, E, V](
  value: E,
  cont: V => Susp[A]
) extends Susp[A] {
  def flatMap[B](f: A => Susp[B]): Susp[B] =
    Cont(value, cont(_: V).flatMap(f))
}
  
case class Result[A](value: A) extends Susp[A] {
  def flatMap[B](f: A => Susp[B]): Susp[B] =
    f(value)
}

object Susp {
  def suspend[A](value: A): Susp[A] = Cont(value, Result.apply)
}
