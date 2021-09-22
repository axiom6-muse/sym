package ax6.zhttp

import zhttp.http._
import zhttp.service.Server
import zio._

object Hello extends App {
  val app: Http[Any, Nothing, Request, UResponse] = Http.collect[Request] {
    case Method.GET -> Root / "text" => Response.text("Hello World!")
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8090, app).exitCode
}
