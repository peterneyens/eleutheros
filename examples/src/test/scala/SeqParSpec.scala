package eleutheros

import cats.arrow.FunctionK
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}

class SeqParSpec extends FlatSpec with Matchers {
  
  "eleutheros" should "be possible to create a SeqPar with iota's CopQ" in {

    import SeqParExample._
    import SeqPar._

    val one = SeqPar.pure[Option, Int](1)
    val two = SeqPar.liftF[Option, Int](Some(2))
    val f: Int => SeqPar[Option, Int] = i => SeqPar.pure(i + 1)

    val xyz = for {
      x <- one
      y <- two
      z <- f(y).map2(two)(_ + _) // (2 + 1) + 2 = 5
    } yield x + y + z // 1 + 2 + 5 = 8

    val result: Option[Int] = SeqPar.foldMap(xyz)(FunctionK.id)

    result should be (Some(8))
  }

  it should "analyze" in {

    import cats.~>
    import cats.instances.list._
    import cats.syntax.cartesian._
    import SeqParExample._
    import SeqPar._

    sealed abstract class GetOp[A] extends Product with Serializable
    case class Get(id: Int) extends GetOp[String]

    val lift: Int => SeqPar[GetOp, String] =
      i => SeqPar.liftF[GetOp, String](Get(i))

    val a = (lift(1) |@| lift(2) |@| lift(3)).map(_ + _ + _)
    val b = a.flatMap(s => lift(s.length))
    val c = (lift(0) |@| b).map(_ + _)

    val ids: GetOp ~> λ[α => List[Int]] =
      λ[GetOp ~> λ[α => List[Int]]]{ case Get(x) => x :: Nil }

    SeqPar.analyze(a)(ids) should be (List(1, 2, 3))
    SeqPar.analyze(b)(ids) should be (List(1, 2, 3))
    SeqPar.analyze(c)(ids) should be (List(0, 1, 2, 3))
  }

}
