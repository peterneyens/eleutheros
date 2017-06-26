package eleutheros

/** 
 * Fix point type for a Capability 
 */
final case class Fixed[
  T[_, _[_], _[_], _],
    E,       F[_], A
](unfix: T[E, Fixed[T, E, F, ?], F, A])
