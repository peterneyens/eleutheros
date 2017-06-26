package eleutheros

sealed trait EitherF[
  T1[_, _[_], _[_], _],
  T2[_, _[_], _[_], _],
     +E, Z[_], F[_], A
]

object EitherF {
  type H[
    T1[_, _[_], _[_], _],
    T2[_, _[_], _[_], _]
  ] = { 
    type Out[E0, Z0[_], F0[_], A0] = EitherF[T1, T2, E0, Z0, F0, A0]
  }
}

final case class LeftF[
  T1[_, _[_], _[_], _],
  T2[_, _[_], _[_], _],
     E, Z[_], F[_], A
](value: T1[E, Z, F, A]) extends EitherF[T1, T2, E, Z, F, A]

final case class RightF[
  T1[_, _[_], _[_], _],
  T2[_, _[_], _[_], _],
     E, Z[_], F[_], A
](value: T2[E, Z, F, A]) extends EitherF[T1, T2, E, Z, F, A]
