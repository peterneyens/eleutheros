package eleutheros

import cats.{~>, Monad, Monoid}
import cats.data.Const
import cats.syntax.flatMap._
import iota._
import iota.QList.::
import scala.annotation.tailrec

object SeqParExample {
  
  type Capabilities = Pure :: Effect :: Parallel :: Sequence :: QNil

  type NoError = Nothing { type T = Unit }

  type SeqPar[F[_], A] = Fixed[CopQ.OnlyL[Capabilities]#Out, NoError, F, A]

  object SeqPar {
    
    val IPure:     CopQ.Inject[Pure,     CopQ.OnlyL[Capabilities]#Out] = scala.Predef.implicitly
    val IEffect:   CopQ.Inject[Effect,   CopQ.OnlyL[Capabilities]#Out] = scala.Predef.implicitly
    val IParallel: CopQ.Inject[Parallel, CopQ.OnlyL[Capabilities]#Out] = scala.Predef.implicitly
    val ISequence: CopQ.Inject[Sequence, CopQ.OnlyL[Capabilities]#Out] = scala.Predef.implicitly

    def pure[F[_], A](a: A): SeqPar[F, A] =
      Fixed(IPure.inj(Pure(a)))

    def liftF[F[_], A](fa: F[A]): SeqPar[F, A] =
      Fixed(IEffect.inj(Effect(fa)))

    implicit def monadInstance[F[_]]: Monad[SeqPar[F, ?]] =
      new Monad[SeqPar[F, ?]] {
        def pure[A](a: A): SeqPar[F, A] = SeqPar.pure(a)

        override def map2[A, B, C](fa: SeqPar[F, A], fb: SeqPar[F, B])(f: (A, B) => C): SeqPar[F, C] =
          Fixed(IParallel.inj(Parallel(fa, fb)(f)))

        override def ap[A, B](ff: SeqPar[F, A => B])(fa: SeqPar[F, A]): SeqPar[F, B] =
          map2(ff, fa)(_.apply(_))

        override def product[A, B](fa: SeqPar[F, A], fb: SeqPar[F, B]): SeqPar[F, (A, B)] =
          map2(fa, fb)(Tuple2.apply)

        override def tuple2[A, B](fa: SeqPar[F, A], fb: SeqPar[F, B]): SeqPar[F, (A, B)] =
          product(fa, fb)

        def flatMap[A, B](fa: SeqPar[F, A])(ff: A => SeqPar[F, B]) : SeqPar[F, B] =
          Fixed(ISequence.inj(Sequence(fa, ff)))

        def tailRecM[A, B](a: A)(f: A => SeqPar[F, Either[A, B]]): SeqPar[F, B] =
          flatMap(f(a)) {
            case Left(a1) => tailRecM(a1)(f)
            case Right(b) => pure(b)
          }
      }


      final def foldMap[F[_], G[_], A](spa: SeqPar[F, A])(f: F ~> G)(implicit G: Monad[G]): G[A] =
        step(spa).unfix match {
          case IPure(Pure(a))      => G.pure(a)
          case IEffect(Effect(fa)) => f(fa)

          // not stack safe 
          case IParallel(par) =>
            G.map2(foldMap(par.left)(f), foldMap(par.right)(f))(par.join)

          // in a Free monad seq.a should always be a Suspend / Effect
          // here this can also be a Parallel though
          case ISequence(seq) =>
            G.flatMap(foldMap(seq.a)(f))(c => foldMap(seq.f(c))(f))
        }

      @tailrec
      final def step[F[_], A](fa: SeqPar[F, A]): SeqPar[F, A] = fa.unfix match {
        case ISequence(seq) =>
          seq.a.unfix match {
            case ISequence(seq2) =>
              step(seq2.a.flatMap(seq2.f(_).flatMap(seq.f)))
            case IPure(Pure(a)) =>
              step(seq.f(a))
            case _ => fa
          }
        case _ => fa
      }

    final def analyze[F[_], M, A]
      (spa: SeqPar[F, A])
      (fk: F ~> λ[α => M])
      (implicit M: Monoid[M])
        : M =
      (spa.unfix match {
        case IPure(_)            => Const.empty[M, A]
        case IEffect(Effect(ef)) => Const(fk(ef))
        case IParallel(par)      => Const(M.combine(analyze(par.left)(fk), analyze(par.right)(fk)))
        case ISequence(seq)      => Const(analyze(seq.a)(fk))
      }).getConst

  }

}
