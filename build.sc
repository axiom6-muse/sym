
import mill._
import mill.define.Target
import mill.scalalib.{ScalaModule, _}

object ax1 extends ScalaModule {
  def scalaVersion = "3.0.2"
  override def mainClass:Target[Option[String]] = Option("ax1.zhttp.Hello")
  override def ivyDeps = Agg(
    ivy"io.d11::zhttp:1.0.0.0-RC17" )
}

object ax6 extends ScalaModule {
  def scalaVersion = "3.0.2"
  override def mainClass:Target[Option[String]] = Option("ax6.test.Suite")
  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parser-combinators:2.0.0" )
}