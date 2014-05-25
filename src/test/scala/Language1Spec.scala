import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

class Language1Spec extends FlatSpec with Matchers with Checkers {

	var f1 = Function.genarateFunction("(gn: Int) => 10 + 9 / gn").get
	var f2 = Function.genarateFunction("(gn: Int) => 10 + gn / gn").get

	"Function $f1" should "be 9 for argument = -7" in {
		f1(-7) shouldBe (Some(9))
	}

	"Function $f2" should "not be defined for argument = 0" in {
		f2(0) shouldBe empty
	}

}