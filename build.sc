// build.sc
import mill._, scalalib._

object foo extends ScalaModule {
  def scalaVersion = "2.13.2" }

object ax6 extends ScalaModule {
  def scalaVersion = "2.12.1"
  override def mainClass = Some("ax6.Ax6")
  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parser-combinators:2.0.0",
    ivy"io.d11::zhttp:1.0.0.0-RC17",
    ivy"org.apache.spark::spark-mllib:2.4.4" )
  override def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_))) }
}

/*
def scalaVersion = "2.13.2"
$ivy.`org.apache.spark::spark-mllib:2.4.4
org.apache.spark » spark-mllib » 2.10
  def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_)))
  }
mill mill.scalalib.GenIdea/idea

def scalacOptions = Seq(
        "-deprecation",
        "-feature"
    )
    
def moduleDeps = Seq("lib.Jama-1.0.3.jar")
//ivy"io.d11::zhttp-test:1.0.0.0-RC17"
ivy"com.lihaoyi::utest:0.6.0"
libraryDependencies ++= Seq(
  "io.d11" %% "zhttp"      % zhttpVersion,
  "io.d11" %% "zhttp-test" % zhttpVersion % "Test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"
)
 */