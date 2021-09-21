// build.sc
import mill._, scalalib._

object foo extends ScalaModule{
  def scalaVersion = "2.13.2" }

object ax6 extends ScalaModule{
  def scalaVersion = "2.13.2"
  def mainClass = Some("ax6.Ax6")
  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parser-combinators:2.0.0",
    ivy"io.d11::zhttp:1.0.0.0-RC17"
  )

}

/*
def moduleDeps = Seq("lib.Jama-1.0.3.jar")
//ivy"io.d11::zhttp-test:1.0.0.0-RC17"
ivy"com.lihaoyi::utest:0.6.0"
libraryDependencies ++= Seq(
  "io.d11" %% "zhttp"      % zhttpVersion,
  "io.d11" %% "zhttp-test" % zhttpVersion % "Test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"
)
 */