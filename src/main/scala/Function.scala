
import scala.Array.canBuildFrom

abstract class Tree {
	def calculate(x: Int): Int
	override def toString : String
}

case class Addition(leaf1: Tree, leaf2: Tree) extends Tree {
	def calculate(x: Int) = leaf1.calculate(x) + leaf2.calculate(x)
	override def toString : String = leaf1 + " + " + leaf2
}

case class Division(leaf1: Tree, leaf2: Tree) extends Tree {
	def calculate(x: Int) = leaf1.calculate(x) / leaf2.calculate(x)
	override def toString : String = leaf1 + " / " + leaf2
}

case class Variable(name: String) extends Tree {
	def calculate(x: Int) = x
	override def toString : String = name
}

case class Constant(name: String) extends Tree {
	def calculate(x: Int) = name.toInt
	override def toString : String = name
}

object Function {

	def genarateFunction(expression: String) = {
		val argumentAndFunctionBody = expression.split("=>").map(_.trim())
		if (argumentAndFunctionBody.length != 2) {
			None
		} else {
			getArgumentName(argumentAndFunctionBody(0)) match {
				case name: String => {
					generateFunctionBody(argumentAndFunctionBody(1), name) match {
						case Some(f) => addRestrictions(f)
						case _ => None
					}
				}
				case _ => None
			}
		}
	}

	private def generateFunctionBody(body: String, argumentName: String): Option[Tree] = {
		if (body.indexOf('+') != -1) {
			body.split("\\+").map(_.trim).map(generateFunctionBody(_, argumentName)) match {
				case Array(Some(leaf), Some(leaf2)) => Some(Addition(leaf, leaf2))
				case _ => None
			}
		} else if (body.indexOf('/') != -1) {
			body.split("/").map(_.trim).map(generateFunctionBody(_, argumentName)) match {
				case Array(Some(leaf), Some(leaf2)) => Some(Division(leaf, leaf2))
				case _ => None
			}
		} else if (body forall Character.isDigit) {
			Some(Constant(body))
		} else if (body equals argumentName) {
			Some(Variable(body))
		} else {
			None
		}
	}

	private def addRestrictions(f: Tree) = {
		Some((x: Int) => {
			if (-100 <= x && x <= 100) {
				try {
					Some(f.calculate(x))
				} catch {
					case e: Exception => None
				}
			} else {
				None
			}
		})
	}

	private def getArgumentName(header: String) = {
		header.split(":").map(_.trim()) match {
			case Array(name, "Int)") => name.slice(name.indexOf('(') + 1, name.length()).trim()
			case _ => None
		}
	}

}