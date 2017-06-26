package eleutheros

import cats.arrow.FunctionK
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}

class FreeMonadSpec extends FlatSpec with Matchers {

  "eleutheros" should "be possible to create a Free monad with iota's CopQ" in {
    import FreeMonadIota._
    import FreeMonad._

    val one = FreeMonad.pure[Option, Int](1)
    val two = FreeMonad.liftF[Option, Int](Some(2))
    val f: Int => FreeMonad[Option, Int] = i => pure(i + 1)

    val xyz = for {
      x <- one  // 1
      y <- two  // 2
      z <- f(y) // 2 + 1 = 3
    } yield x + y + z // 1 + 2 + 3 = 6


    val result: Option[Int] = FreeMonad.foldMap(xyz)(FunctionK.id)

    result should be (Some(6))
  }

  it should "be possible to create a Free monad with EitherF" in {
    import FreeMonadEitherF._
    import FreeMonad._

    val one = FreeMonad.pure[Option, Int](1)
    val two = FreeMonad.liftF[Option, Int](Some(2))
    val f: Int => FreeMonad[Option, Int] = i => pure(i + 1)

    val xyz = for {
      x <- one  // 1
      y <- two  // 2
      z <- f(y) // 2 + 1 = 3
    } yield x + y + z // 1 + 2 + 3 = 6


    val result: Option[Int] = FreeMonad.foldMap(xyz)(FunctionK.id)

    result should be (Some(6))
  }

}



