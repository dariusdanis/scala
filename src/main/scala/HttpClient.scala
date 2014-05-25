import scala.concurrent.Await
import scala.concurrent.duration._
import dispatch._, Defaults._

class HttpClient(lang: Int, challengeId: Int, token: String) {

	val challengeHost = "guess.homedir.eu"
	val address = host(challengeHost) / "lang" / lang / "challenge" / challengeId

	def get(arg: Int): Option[Int] = {
		val request = address
		val myGet = request.GET <<? Map("arg" -> arg.toString, "token" -> token, "challenge" -> challengeId.toString)
		val future = Http(myGet OK as.String).option
		val result = Await.result(future, 10 seconds)
		result.map(_.toInt)
	}

	def post(solution: String) = {
		val request = address
		val post = request.POST <<? Map("token" -> token, "challenge" -> challengeId.toString)
		val postWithArgs = post << solution
		val future = Http(postWithArgs OK as.String).option
		Await.result(future, 10 seconds)
	}

}