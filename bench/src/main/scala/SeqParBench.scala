package eleutheros.bench

import cats.arrow.FunctionK
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class SeqParBench {

  @Benchmark
  def eleutherosIotaSeqPar: Option[Int] = { 
    import eleutheros.SeqParExample._
    import SeqPar._

    val one = SeqPar.pure[Option, Int](1)
    val two = SeqPar.liftF[Option, Int](Some(2))
    val f: Int => SeqPar[Option, Int] = i => SeqPar.pure(i + 1)

    val xyz: SeqPar[Option, Int] = for {
      x <- one  // 1
      y <- two  // 2
      z <- f(y).map2(two)(_ + _) // (2 + 1) + 2 = 5
    } yield x + y + z // 1 + 2 + 5 = 8

    val result: Option[Int] = SeqPar.foldMap(xyz)(FunctionK.id)

    result
  }

  @Benchmark
  def freestyleFreeS: Option[Int] = {
    import cats.~>
    import freestyle._

    val one = FreeS.pure[Option, Int](1)
    val two = FreeS.liftFA[Option, Int](Some(2))
    val f: Int => FreeS[Option, Int] = i => FreeS.pure(i + 1)

    val xyz = for {
      x <- one  // 1
      y <- two  // 2
      z <- f(y).map2(two)(_ + _) // (2 + 1) + 2 = 5
    } yield x + y + z // 1 + 2 + 5 = 8


    implicit val fk: Option ~> Option = FunctionK.id[Option]
    val result: Option[Int] = xyz.interpret[Option]

    result
  }

}