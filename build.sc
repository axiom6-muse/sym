
import mill._
import mill.define.Target
import mill.scalalib._

object ax6 extends ScalaModule {
  def scalaVersion = "3.1.0"
  override def mainClass:Target[Option[String]] = Option("ax6.test.Suite")
  override def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_))) }
  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parser-combinators:2.0.0" )
  object test extends Tests with TestModule.Utest {
    override def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.10")
  }
}

object ax2 extends ScalaModule {
  def scalaVersion = "3.1.0"
  override def mainClass:Target[Option[String]] = Option("ax2.zhttp.Hello")
  override def moduleDeps = Seq(ax6)
  override def ivyDeps = Agg(
    ivy"io.d11::zhttp:1.0.0.0-RC17",
    ivy"com.lihaoyi::upickle:1.4.0")
}
