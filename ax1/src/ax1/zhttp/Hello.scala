package ax1.zhttp

import zhttp.http._
import zhttp.service.Server
import zio._

object Hello extends App {
  val app: Http[Any, Nothing, Request, UResponse] = Http.collect[Request] {
    case Method.GET -> Root / "text" => Response.text("Hello World!")
    case Method.GET -> Root / "json" => Response.jsonString("""{"greetings": "Hello World!"}""")
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    println( "ZHttp server running on http://localhost:8090 with /text /json paths")
    Server.start(8090, app).exitCode
  }
}
