import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbBool
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Gen._

object Language1Properties extends Properties("Function") {
	
	implicit val argumentGenerator: Arbitrary[NonZeroInt] = Arbitrary {
		for {
			c <- Gen.choose(-100, 100).suchThat(_ != 0)
		} yield new NonZeroInt(c)
	}

	class NonZeroInt(x: Int) {
        def get = x
    }
	
	implicit val constantGenerator: Arbitrary[Constant] = Arbitrary {
        for {
            c <- Gen.choose(1, 10)
        } yield Constant(c.toString)
    }
	
	implicit val functionGenerator: Arbitrary[Tree] = Arbitrary {
    	def containsArgumentAndConstant(conf: List[Boolean]) = conf.contains(true) && conf.contains(false)
    	for {
            operationOrder <- arbitrary[Boolean]
            consts <- containerOfN[List, Constant](3, arbitrary[Constant])
            operands <- containerOfN[List, Boolean](3, arbitrary[Boolean]).suchThat(containsArgumentAndConstant)
            ops = operands.zip(consts).map((x: (Boolean, Constant)) => if (x._1) Variable("x") else x._2)
        } yield {
            if (operationOrder) {
                Addition(Division(ops(0), ops(1)), ops(2))
            } else {
                Addition(ops(0), Division(ops(1), ops(2)))
            }
        }
    }
	
	property("genarateFunction") = forAll  { (func: Tree, param: NonZeroInt) =>
		val f = Function.genarateFunction("(x: Int) => " + func.toString).get
        f(param.get) == Some(func.calculate(param.get))
	}
	
}