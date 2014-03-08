package vu.mif.scala

object Main {

	def main(args: Array[String]) {

		val funnction = Function.genarateFunction("(gn: Int) => 10 + 9 / gn")
		funnction match {
			case Some(func: (Int => Option[Int])) => println(func(-7))
			case _ => print("Bad date")
		}
		
	}
}


//		def a = Some(Addition(Constant("5"), Variable("x")))
//		a.get.calculate(5)