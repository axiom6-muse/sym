// build.sc
import mill._
import mill.scalalib.{ScalaModule, _}

object ax1 extends ScalaModule {
  def scalaVersion = "3.0.2" }

object ax6 extends ScalaModule {
  def scalaVersion = "3.0.2"
  override def mainClass = Option("ax6.test.Suite")
  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parser-combinators:2.0.0",
    ivy"io.d11::zhttp:1.0.0.0-RC17" )
}

/*
ivy"org.apache.spark::spark-mllib:2.4.4"
  object test extends ScalaModule {
    def scalaVersion = "2.12.1"
    println( ("millSourcePath", millSourcePath ) )
    override def sources   = T.sources( millSourcePath )
    override def mainClass = Option("ax6.test.Suite")
  }
    override def sources   = T.sources( millSourcePath / "ax6.math")
    println( ("sources", sources ) )
//override def sources = T.sources( millSourcePath / "test" )
  println( "Sourece:" + sources )
  def srcs = T.source( "ax6.test" )
  def srcs = T.source(millSourcePath / "test")
  def srcs = T.source(millSourcePath / "src")
  object test extends Tests {
    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.5",
      ivy"com.novocode:junit-interface:0.11"
    )

    def testFrameworks = Seq("org.scalatest.tools.Framework",
         "com.novocode.junit.JUnitFramework")
  }


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