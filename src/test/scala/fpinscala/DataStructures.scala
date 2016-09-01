package fpinscala

import org.scalacheck._
import DataStructures._
import DataStructures.List._

object DataStructuresSpec extends Properties("DataStructuresSpec") {

  import org.scalacheck.Prop.forAll

  implicit val arbList: Arbitrary[List[Int]] = Arbitrary {

    val genNil = Gen.const(Nil)

    lazy val genCons = for {
      h <- Arbitrary.arbitrary[Int]
      t <- genList
    } yield Cons(h, t)

    def genList: Gen[List[Int]] = Gen.oneOf(genNil, genCons)

    genList
  }

  property("tail") = forAll { l: List[Int] =>
    l match {
      case Nil => true
      case Cons(h, t) => tail(l) == t
    }

  }

}
