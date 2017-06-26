package eleutheros

import cats.{~>, Monad}
import cats.syntax.flatMap._
import iota._
import iota.QList.::
import scala.annotation.switch
import scala.annotation.tailrec

object FreeMonadIota {
  type MonadCapabilities = Pure :: Effect :: Sequence :: QNil

  // workaround around Nothing not being inferred 
  //  (also Nothing <: E, but class CopQ is invariant in type E)
  // https://issues.scala-lang.org/browse/SI-9453
  // https://stackoverflow.com/questions/32097291/bizzare-type-inference-limitation-multiple-type-params
  type NoError = Nothing { type T = Unit }

  type FreeMonad[F[_], A] = Fixed[CopQ.OnlyL[MonadCapabilities]#Out, NoError, F, A]

  object FreeMonad {
    
    val IPure:     CopQ.Inject[Pure,     CopQ.OnlyL[MonadCapabilities]#Out] = scala.Predef.implicitly
    val IEffect:   CopQ.Inject[Effect,   CopQ.OnlyL[MonadCapabilities]#Out] = scala.Predef.implicitly
    val ISequence: CopQ.Inject[Sequence, CopQ.OnlyL[MonadCapabilities]#Out] = scala.Predef.implicitly

    def pure[F[_], A](a: A): FreeMonad[F, A] =
      Fixed(IPure.inj(Pure(a)))

    def liftF[F[_], A](fa: F[A]): FreeMonad[F, A] =
      Fixed(IEffect.inj(Effect(fa)))

    implicit def monadInstance[F[_]]: Monad[FreeMonad[F, ?]] =
      new Monad[FreeMonad[F, ?]] {
        def pure[A](a: A): FreeMonad[F, A] = FreeMonad.pure(a)

        def flatMap[A, B](fa: FreeMonad[F, A])(ff: A => FreeMonad[F, B]) : FreeMonad[F, B] =
          Fixed(ISequence.inj(Sequence(fa, ff)))

        def tailRecM[A, B](a: A)(f: A => FreeMonad[F, Either[A, B]]): FreeMonad[F, B] =
          flatMap(f(a)) {
            case Left(a1) => tailRecM(a1)(f)
            case Right(b) => pure(b)
          }
      }

    // def foldMap[F[_], M[_], A]
    //   (fa: FreeMonad[F, A])
    //   (fk: F ~> M)
    //   (implicit M: Monad[M])
    //     : M[A] =
    //   M.tailRecM(fa)(step(_).unfix match {
    //     case IEffect(Effect(ef)) => M.map(fk(ef))(Right(_))
    //     case IPure(Pure(a))      => M.pure(Right(a))
    //     case ISequence(seq)      => M.map(foldMap(seq.a)(fk))(cc => Left(seq.f(cc)))
    //   })

    // @tailrec
    // final def step[F[_], A](fa: FreeMonad[F, A]): FreeMonad[F, A] = fa.unfix match {
    //   case ISequence(seq) =>
    //     seq.a.unfix match {
    //       case ISequence(seq2) =>
    //         step(seq2.a.flatMap(seq2.f(_).flatMap(seq.f)))
    //       case IPure(Pure(a)) =>
    //         step(seq.f(a))
    //       case _ => fa
    //     }
    //   case _ => fa
    // }

    // foldMap using @switch
    def foldMap[F[_], M[_], A]
      (fa: FreeMonad[F, A])
      (fk: F ~> M)
      (implicit M: Monad[M])
        : M[A] =
      M.tailRecM(fa){ a =>
        val copq = step(a).unfix
        (copq.index: @switch) match {
          case 0 => 
            val IPure(Pure(a)) = copq
            M.pure(Right(a))
          case 1 =>
            val IEffect(Effect(ef)) = copq
            M.map(fk(ef))(Right(_))
          case 2 => 
            val ISequence(seq) = copq
            M.map(foldMap(seq.a)(fk))(cc => Left(seq.f(cc)))
        }
      }

    // step using @switch
    @tailrec
    final def step[F[_], A](fa: FreeMonad[F, A]): FreeMonad[F, A] = {
      val copq = fa.unfix
      if (copq.index == 2) {
        val ISequence(seq) = copq
        val copq2 = seq.a.unfix
        (copq2.index: @switch) match {
          case 2 => 
            val ISequence(seq2) = copq2
            step(seq2.a.flatMap(seq2.f(_).flatMap(seq.f)))
          case 0 =>
            val IPure(Pure(a)) = copq2
            step(seq.f(a))
          case _ => fa
        }
      } else fa
    }


    // // foldMap using @switch and casting
    // def foldMap2[F[_], M[_], A]
    //   (fa: FreeMonad[F, A])
    //   (fk: F ~> M)
    //   (implicit M: Monad[M])
    //     : M[A] =
    //   M.tailRecM(fa){ a =>
    //     val copq = step2(a).unfix
    //     (copq.index: @switch) match {
    //       case 0 => 
    //         // val Pure(a) = copq.value.asInstanceOf[Pure[NoError, FreeMonad[F, ?], F, A]]
    //         val a = copq.value.asInstanceOf[Pure[NoError, FreeMonad[F, ?], F, A]].a
    //         M.pure(Right(a))
    //       case 1 =>
    //         // val Effect(ef) = copq.value.asInstanceOf[Effect[NoError, FreeMonad[F, ?], F, A]]
    //         val ef = copq.value.asInstanceOf[Effect[NoError, FreeMonad[F, ?], F, A]].fa
    //         M.map(fk(ef))(Right(_))
    //       case 2 => 
    //         val seq = copq.value.asInstanceOf[Sequence[NoError, FreeMonad[F, ?], F, A]]
    //         M.map(foldMap(seq.a)(fk))(cc => Left(seq.f(cc)))
    //     }
    //   }

    // // step using @switch and casting
    // @tailrec
    // final def step2[F[_], A](fa: FreeMonad[F, A]): FreeMonad[F, A] = {
    //   val copq = fa.unfix
    //   if (copq.index == 2) {
    //     val seq = copq.value.asInstanceOf[Sequence[NoError, FreeMonad[F, ?], F, A]]
    //     val copq2 = seq.a.unfix
    //     (copq2.index: @switch) match {
    //       case 2 => 
    //         val seq2 = copq2.value.asInstanceOf[Sequence[NoError, FreeMonad[F, ?], F, A]]
    //         step2(seq2.a.flatMap(seq2.f(_).flatMap(seq.f)))
    //       case 0 =>
    //         // val Pure(a) = copq2.value.asInstanceOf[Pure[NoError, FreeMonad[F, ?], F, Any]]
    //         val a = copq2.value.asInstanceOf[Pure[NoError, FreeMonad[F, ?], F, Any]].a
    //         val f: Any => FreeMonad[F, A] = seq.f.asInstanceOf[Any => FreeMonad[F, A]]
    //         step2(f(a))
    //       case _ => fa
    //     }
    //   } else fa
    // }

    // without @switch
    // [info] Benchmark                                   Mode  Cnt        Score        Error  Units
    // [info] FreeMonadBench.catsFree                    thrpt   10  5303847.922 ± 131820.906  ops/s
    // [info] FreeMonadBench.eleutherosEitherFFreeMonad  thrpt   10  2326453.816 ±  24621.420  ops/s
    // [info] FreeMonadBench.eleutherosIotaFreeMonad     thrpt   10  2205078.867 ±  43958.456  ops/s

    // with @switch
    // [info] Benchmark                                   Mode  Cnt        Score       Error  Units
    // [info] FreeMonadBench.catsFree                    thrpt   10  5202059.547 ± 48599.987  ops/s
    // [info] FreeMonadBench.eleutherosEitherFFreeMonad  thrpt   10  2366568.224 ± 17667.708  ops/s
    // [info] FreeMonadBench.eleutherosIotaFreeMonad     thrpt   10  2745629.222 ± 38607.882  ops/s

    // IotaFreeMonad -> 25% speed up

  }
}