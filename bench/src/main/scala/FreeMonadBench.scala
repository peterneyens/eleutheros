package eleutheros.bench

import cats.arrow.FunctionK
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class FreeMonadBench {

  @Benchmark
  def eleutherosIotaFreeMonad: Option[Int] = { 
    import eleutheros.FreeMonadIota._
    import FreeMonad._

    val one = FreeMonad.pure[Option, Int](1)
    val two = FreeMonad.liftF[Option, Int](Some(2))
    val f: Int => FreeMonad[Option, Int] = i => pure(i + 1)

    val xyz = for {
      x <- one  // 1
      y <- two  // 2
      z <- f(y) // 2 + 1 = 3
    } yield x + y + z // 1 + 2 + 3 = 6

    FreeMonad.foldMap(xyz)(FunctionK.id)
  }

  // @Benchmark
  // def eleutherosIotaFreeMonad2: Option[Int] = { 
  //   import eleutheros.FreeMonadIota._
  //   import FreeMonad._

  //   val one = FreeMonad.pure[Option, Int](1)
  //   val two = FreeMonad.liftF[Option, Int](Some(2))
  //   val f: Int => FreeMonad[Option, Int] = i => pure(i + 1)

  //   val xyz = for {
  //     x <- one  // 1
  //     y <- two  // 2
  //     z <- f(y) // 2 + 1 = 3
  //   } yield x + y + z // 1 + 2 + 3 = 6

  //   FreeMonad.foldMap2(xyz)(FunctionK.id)
  // }

  @Benchmark
  def eleutherosEitherFFreeMonad: Option[Int] = { 
    import eleutheros.FreeMonadEitherF._
    import FreeMonad._

    val one = FreeMonad.pure[Option, Int](1)
    val two = FreeMonad.liftF[Option, Int](Some(2))
    val f: Int => FreeMonad[Option, Int] = i => pure(i + 1)

    val xyz = for {
      x <- one  // 1
      y <- two  // 2
      z <- f(y) // 2 + 1 = 3
    } yield x + y + z // 1 + 2 + 3 = 6

    FreeMonad.foldMap(xyz)(FunctionK.id)
  }

  @Benchmark
  def catsFree: Option[Int] = {
    import cats.free.Free

    val one = Free.pure[Option, Int](1)
    val two = Free.liftF[Option, Int](Some(2))
    val f: Int => Free[Option, Int] = i => Free.pure(i + 1)

    val xyz = for {
      x <- one  // 1
      y <- two  // 2
      z <- f(y) // 2 + 1 = 3
    } yield x + y + z // 1 + 2 + 3 = 6

    xyz.foldMap(FunctionK.id)
  }
}
