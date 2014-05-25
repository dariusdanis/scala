import scala.util.control.Breaks._
class Solver(client: HttpClient, analyzer : Analyzer) {
	val maxNumberForLinearEquation = 10
	val maxNumberForHyperbolicEquation = 16
	val prefix = "(x: Int) => "
	
	def solve() {
		var map = initMap(client.get(0))
		var function = analyzer.analyze(map)
		if(analyzer.withTwoConstants) {
			if (!guess(function.toString())) {
				generateFunctions(map)
			}
		} else {
			guess(function.toString())
		}
	}
	
	def guess(function :String): Boolean = {
		var success = true
		if (!client.post(prefix + function).get.equals("OK")) {
			success = client.post(prefix + revert(function)).get.equals("OK")
		}
		success
	}
	
	def revert(function: String): String = {
		def array = function.split('+').map(_.trim())
		array(1) + " + " + array(0)
	}
	
	def initMap(firstValue :Option[Int]): Map[Int, Option[Int]] = {
		var map: Map[Int, Option[Int]] = Map()
		map += (0 -> firstValue)
		var index = maxNumberForLinearEquation
		if (map.get(0).get == None) {
			index = maxNumberForHyperbolicEquation
		}
		for (i <- 1 to index) {
			map += (i -> client.get(i))
		}
		map
	}
	
	def generateFunctions(map :Map[Int, Option[Int]]) {
		breakable {
			if (map.get(0).get.get != 0) {
				for (i <- 1 to 10) {
					if (check(1, i, i)) break
				}
			} else {
				for (i <- 1 to 10) {
					if (check(i+1, 10, i)) break
				}
			}
		}
	}
	
	def check(from :Int, index :Int, i :Int): Boolean = {
		var success = false
		for (j <- from to index) {
			if (guess(Addition(Variable("x"), Division(Constant(i.toString), Constant(j.toString))).toString)) {
				success = true
				break
			}
		}
		success
	}
	
}