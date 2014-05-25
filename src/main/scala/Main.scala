
object Main {

	def main(args: Array[String]) {
		new Solver(new HttpClient(1, 64, "Zoo8me3zuva5aeF0"), new Analyzer).solve
	}
	
}