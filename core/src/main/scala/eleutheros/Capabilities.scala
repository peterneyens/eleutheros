package eleutheros

/**
 * Capabilities
 *
 * A Capability has type `T[E, Z[_], F[_], A]`
 *
 * E    = error
 * Z[_] = self type combined structure
 * F[_] = effect type
 * A    = value type
 */

final case class Pure[E, Z[_], F[_], A](a: A)

final case class Effect[E, Z[_], F[_], A](fa: F[A])

trait Sequence[E, Z[_], F[_], A] {
  type A0

  def a: Z[A0]
  def f: A0 => Z[A]

  override def toString: String =
    "Sequence(" + a.toString + ", " + f.toString + ")"
}

object Sequence {
  type Aux[E, Z[_], F[_], A, I] = Sequence[E, Z, F, A] { type A0 = I }

  def apply[E, Z[_], F[_], A, I](zi: Z[I], iza: I => Z[A]): Aux[E, Z, F, A, I] =
    new Sequence[E, Z, F, A] {
      type A0 = I
      val a = zi
      val f = iza
    }
}

trait Parallel[E, Z[_], F[_], A] {
  type B0
  type C0

  def left: Z[B0]
  def right: Z[C0]
  def join: (B0, C0) => A

  override def toString: String =
    "Parallel(" + left.toString + ", " + right.toString + ", " + join.toString + ")"
}

object Parallel {
  type Aux[E, Z[_], F[_], A, B, C] = Parallel[E, Z, F, A] { type B0 = B; type C0 = C }

  def apply[E, Z[_], F[_], A, B, C](l: Z[B], r: Z[C])(j: (B, C) => A): Aux[E, Z, F, A, B, C] =
    new Parallel[E, Z, F, A] {
      type B0 = B
      type C0 = C
      val left  = l
      val right = r
      val join  = j
    }
}

case class Failure[E, Z[_], F[_], A](error: E)

case class Recover[E, Z[_], F[_], A](value: Z[A], f: E => Z[A])

case class FirstSuccess[E, Z[_], F[_], A](first: Z[A], second: Z[A])
