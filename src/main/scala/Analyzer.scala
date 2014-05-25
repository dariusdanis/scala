import scala.util.control.Breaks._

class Analyzer {
	
	var withTwoConstants = false
	
	def analyze(map :Map[Int, Option[Int]]): Tree =  {
		if (map.get(0).get != None) {
			analizeForLinearEquation(map)
		} else {
			analizeForHyperbolicEquation(map)
		}
	}
	
	def analizeForLinearEquation(map :Map[Int, Option[Int]]): Tree = {
		if (map.get(0).get != Some(0)) {
			analizeForLEWithOneArgument(map)
		} else {
			analizeForLEWithTwoArguments(map);
		}
	}
	
	def analizeForHyperbolicEquation(map :Map[Int, Option[Int]]): Tree = {
		var index = checkIfContains(map).get
		if (index != -1) {
			Addition(Constant(map.get(index).get.get.toString), Division(Constant((index - 1).toString), Variable("x")))
		} else {
			analizeForHEForTwoArguments(map)
		}
	}
	
	def analizeForHEForTwoArguments(map :Map[Int, Option[Int]]): Tree = {
		var index = 0
		breakable {
			for (i <- 1 to map.size - 2) {
				if (map.get(i) == map.get(i + 1) && isSeq(map, i+1).get == -1) {
					index = i
					break
				}
			}
		}
		Addition(Variable("x"), Division(Constant(index.toString), Variable("x")))
	}
	
	def analizeForLEWithTwoArguments(map :Map[Int, Option[Int]]): Tree = {
		var constant = isSeq(map, 0).get
		if (constant == -1) {
			withTwoConstants = true
			Addition(Variable("x"), Division(Constant("1"), Constant("2")))
		} else {
			Addition(Variable("x"), Division(Variable("x"), Constant(constant.toString)))
		}
	}
	
	def checkIfContains(map :Map[Int, Option[Int]]): Some[Int] = {
		var index = Some(-1)
		breakable {
			var time = 0
			var number = map.get(1).get.get
			for (i <- 2 to map.size - 1) {
				if (map.get(i).get.get == number) {
					time += 1
					if (time == 5) {
						index = Some(i - 5)
						break
					}
				} else {
					time = 0
					number = map.get(i).get.get
				}
				
			}
		}
		index
	}
	
	def isSeq(map :Map[Int, Option[Int]], from :Int): Some[Int] = {
		var constant = Some(-1)
		breakable { 
			for (i <- from to map.size - 2) {
				if (map.get(i).get.get + 1 != map.get(i + 1).get.get) {
					constant = Some(i+1)
					break
				}
			}
		}
		constant
	}
	
	def analizeForLEWithOneArgument(map :Map[Int, Option[Int]]): Tree = {
		val constant = map.get(0).get.get
		var secondConstat = 0
		breakable { 
			for (i  <- 1 to map.size - 1) { 
				if (map.get(i).get.get - constant == 1) { 
					secondConstat = i 
					break
				}
			}
		}
		withTwoConstants = true
		Addition(Constant(constant.toString), Division(Variable("x"), Constant(secondConstat.toString)))
	}
	
}