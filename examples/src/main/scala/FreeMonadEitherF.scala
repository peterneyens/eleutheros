package eleutheros

import cats.~>
import cats.Monad
import cats.syntax.flatMap._
import scala.annotation.tailrec

object FreeMonadEitherF {

  type PureSeqF[E0, Z0[_], F0[_], A0] = EitherF[Pure, Sequence, E0, Z0, F0, A0]
  type FreeMonadF[E0, Z0[_], F0[_], A0] = EitherF[Effect, PureSeqF, E0, Z0, F0, A0]

  type FreeMonad[F[_], A] = Fixed[FreeMonadF, Nothing, F, A]

  object FreeMonad {
    def pure[F[_], A](a: A): FreeMonad[F, A] =
      Fixed[FreeMonadF, Nothing, F, A](
        RightF[Effect, PureSeqF, Nothing, FreeMonad[F, ?], F, A](
          LeftF[Pure, Sequence, Nothing, FreeMonad[F, ?], F, A](
            Pure[Nothing, FreeMonad[F, ?], F, A](a))))

    def liftF[F[_], A](fa: F[A]): FreeMonad[F, A] =
      Fixed[FreeMonadF, Nothing, F, A](
        LeftF[Effect, PureSeqF, Nothing, FreeMonad[F, ?], F, A](
          Effect[Nothing, FreeMonad[F, ?], F, A](fa)))

    implicit def freeMonadMonad[F[_]]: Monad[FreeMonad[F, ?]] =
      new Monad[FreeMonad[F, ?]] {
        def pure[A](a: A): FreeMonad[F, A] = FreeMonad.pure(a)

        def flatMap[A, B](fa: FreeMonad[F, A])(ff: A => FreeMonad[F, B]) : FreeMonad[F, B] =
          Fixed[FreeMonadF, Nothing, F, B](
            RightF[Effect, PureSeqF, Nothing, FreeMonad[F, ?], F, B](
              RightF[Pure, Sequence, Nothing, FreeMonad[F, ?], F, B](
                new Sequence[Nothing, FreeMonad[F, ?], F, B] {
                  type A0 = A

                  val a: FreeMonad[F, A0] = fa
                  val f: A0 => FreeMonad[F, B] = ff
                })))
          
        def tailRecM[A, B](a: A)(f: A => FreeMonad[F, Either[A, B]]): FreeMonad[F, B] =
          flatMap(f(a)) {
            case Left(a1) => tailRecM(a1)(f)
            case Right(b) => pure(b)
          }
      }

    def foldMap[F[_], M[_], A]
      (fa: FreeMonad[F, A])
      (fk: F ~> M)
      (implicit M: Monad[M])
        : M[A] =
      M.tailRecM(fa)(step(_).unfix match {
        case LeftF(Effect(ef))      => M.map(fk(ef))(Right(_))
        case RightF(LeftF(Pure(a))) => M.pure(Right(a))
        case RightF(RightF(seq))    => M.map(foldMap(seq.a)(fk))(cc => Left(seq.f(cc)))
      })

    @tailrec
    final def step[F[_], A](fa: FreeMonad[F, A]): FreeMonad[F, A] = fa.unfix match {
      case RightF(RightF(seq: Sequence[Nothing, Fixed[FreeMonadF, Nothing, F, ?], F, _])) =>
        seq.a.unfix match {
          case RightF(RightF(seq2: Sequence[Nothing, Fixed[FreeMonadF, Nothing, F, ?], F, _])) =>
            step(seq2.a.flatMap(seq2.f(_).flatMap(seq.f)))
          case RightF(LeftF(Pure(a))) =>
            step(seq.f(a))
          case _ => fa
        }
      case _ => fa
    }

  }

}