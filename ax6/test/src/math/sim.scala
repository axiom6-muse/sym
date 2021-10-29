package ax6.test.sim

import utest._
import ax6.math.exp._
import ax6.math.num._
import ax6.math.parse.AsciiParse
import ax6.util.Text
import ax6.test.Test

// sim( "Sim.i", "(x*y*z)/(x*y*z*w)",    "1/w"
// sim( "Sim.j", "(w*x*y*z)/(z*x*w)",    "y" )

def sim( name:String, enter:String, expect:String ): Unit = {
  var exp:Exp = AsciiParse(enter)
  exp = exp.sim
  Test.init( name, enter, expect )      // exp.toLambda )
  Test.test( name, enter, exp.toAscii ) // exp.toLambda )
}

  object SimTest extends TestSuite{
  val tests = Tests{
    test("Sim.i"){ sim( "Sim.i", "(x*y*z)/(x*y*z*w)",    "1/w" ) }
    test("Sim.j"){ sim( "Sim.j", "(w*x*y*z)/(z*x*w)",    "y"   ) }
  }
}