
package ax6.test

import ax6.math.exp._
import ax6.math.num._
import ax6.math.parse.AsciiParse
import ax6.util.Text


object Suite
{
  def main( args: Array[String] ): Unit = {
    val suite = new Suite()
    runTests(suite) }

   def runTests(suite:Suite) : Unit =
   {
       suite.testDbg()
       suite.testDif()
       suite.testSim()
    // suite.testCal()
    // suite.testEee()
    // suite.testCex()
    // suite.testVex()
    // suite.testMex()
    // suite.testRun()
    // suite.testDif()
    // suite.testItg()
    // suite.testFun()
    // suite.testSum()
    // suite.testEqu()
    // suite.testSus
    // suite.testErr
    // suite.testEqs()
   } 
}
  
class Suite //extends Suite
{
  type CS     = Text.CS
  type Assign = String => Double

  def pars( name:String, seq:String* ) : Unit =
  {
    val iText:Text  = new Text(100)
    val tText:Text  = new Text(100)
    for( str <- seq )
      iText.all(str, ' ')
    Test.init( name, iText )
    for( str <- seq )
    {
      AsciiParse(str).ascii(tText)
      tText.app(' ')
    }
    Test.test( name, tText )
  }
  
  def par( name:String, str:String ): Unit = {
    Test.init( name, str )
    val exp:Exp = AsciiParse(str)
    Test.test( name, exp.toAscii )
  }

  def asc( name:String, enter:String, expect:String ): Unit = {
    val exp:Exp = AsciiParse(enter)
    Test.init( name, enter, expect )
    Test.test( name, enter, exp.toAscii )
  }

  def eqs( name:String, expect:String, result:String ): Unit = {
    val exp:Exp = AsciiParse(expect)
    val res:Exp = AsciiParse(result)
    val isEq    = if( exp == res ) "Pass" else "Fail"
    Test.init( name, expect, exp.toLambda, "Pass" )
    Test.test( name, result, res.toLambda,  isEq  )
  }

  def sim( name:String, enter:String, expect:String ): Unit = {
    var exp:Exp = AsciiParse(enter)
    exp = exp.sim
    Test.init( name, enter, expect,      exp.toLambda )
    Test.test( name, enter, exp.toAscii, exp.toLambda )
  }  

  def lam( name:String, enter:String, expect:String ): Unit = {
    val exp:Exp = AsciiParse(enter)
    Test.init( name, enter,       expect )
    Test.test( name, exp.toAscii, exp.toLambda )
  }  
  
  def itg( name:String, enter:String, expect:String ): Unit = {
    val exp:Exp = AsciiParse(enter)
    Test.init( name, enter, expect  )
    Test.test( name, enter, exp.itg.toAscii )
  }

  def dif( name:String, enter:String, expect:String ): Unit = {
    val exp:Exp = AsciiParse(enter)
    Test.init( name, enter, expect              )  // , exp.dif.toLambda 
    Test.test( name, enter, exp.dif.sim.toAscii )  // , exp.dif.toLambda
  }

  def dns( name:String, enter:String, expect:String ): Unit = {
    val exp:Exp = AsciiParse(enter)
    Test.init( name, enter, expect          )  // , exp.dif.toLambda
    Test.test( name, enter, exp.dif.toAscii )  // , exp.dif.toLambda
  }

  def testDbg(): Unit = {
    lam( "pow.a", "(x+y)^3",         "Pow(Add(Var(x),Var(y)),Num(3))" )
    lam( "add.a", "x+x+7+y",         "Add(Add(Add(Var(x),Var(x)),Num(7)),Var(y))"   )
    lam( "add.b", "x+x-7+y",         "Add(Add(Var(x),Sub(Var(x),Num(7))),Var(y))" )
    lam( "add.c", "(x+y+z)*(x+y+z)", "Mul(Add(Add(Var(x),Var(y)),Var(z)),Add(Add(Var(x),Var(y)),Var(z)))" )
    lam( "mul.b", "(x*y*z)*(x*y*z)", "Mul(Par(Mul(Mul(Var(x),Var(y)),Var(z))),Par(Mul(Mul(Var(x),Var(y)),Var(z))))" )
    lam( "sub.a", "x-x-7-z",         "Sub(Sub(Sub(Var(x),Var(x)),Num(7)),Var(z))"   )  // x-x-7-z
  }

  def testCal(): Unit = {
    val powc:Exp = Pow(Add(Var("x"),Var("y")),Num(3))
    val ppp:Assign = { case "x" => 2 case "y" => 1 }
    Test.init( "calc.b", "<x=2 y=1> ", powc.toAscii, " = ", 27.0 )
    Test.test( "calc.b", "<x=2 y=1> ", powc.toAscii," = ", powc.calc(ppp) )

    val env:Assign = { case "x" => 5 case "y" => 7 }
    Test.init( "calc.a", "<x=5 y=7> ", powc.toAscii, " = ", "24.0" )
    Test.test( "calc.a", "<x=5 y=7> ", powc.toAscii, " = ", powc.calc(env) )
   }
 
   def testCex(): Unit = {
     lam( "cex.a", "[a,b.i]",       "Cex(Var(a),Var(b))" )
     lam( "cex.b", "[a+b,(c+d).i]", "Cex(Adds(Var(a),Var(b)),Par(Adds(Var(c),Var(d))))" )
     lam( "cex.c", "i*a*i", "Mul(Mul(Var(i),Var(a)),Var(i))" )
   }
   
   def testVex(): Unit = {
     par( "vex.a", "[a,b]" )
     par( "vex.b", "[x^2,y^3,z^4]" )
   }   

   def testMex(): Unit = {
     par( "mex.parse.ma", "[[x,x^2,x^3][y,y^2,y^3][z,z^2,z^3]]" )
     val sb = "[[dx,2*x*dx,3*x^2*dx][dy,2*y*dy,3*y^2*dy][dz,2*z*dz,3*z^2*dz]]"
     Test.init( "mex.dif.mb", sb )
     val eb:Exp = AsciiParse( sb )
     val mb:Mex = Mex(eb)
     mb.dif
     Test.test( "mex.dif.mb", mb.toAscii )

     val rc:Assign = { case "x"=>1 case "y"=>2 case "z"=>3 }
     val ea:Exp    = AsciiParse("[[x,x^2,x^3][y,y^2,y^3][z,z^2,z^3]]")
     val nc        = "[[1,1,1][2,4,8][3,9,27]]"
     Test.init( "mex.eval.mc", ea.toAscii, nc )
     val ma:Mex = Mex(ea)
     val mc:Mat = ma.calcMex(rc)
     Test.test( "mex.eval.mc", ma.toAscii, mc.toStr )

     par( "mex.d.var.a", "[[a,b,c][d,e,f][g,h,i]]" )    
     par( "mex.d.var.b", "[[a,b^2][c,d^2]]" )

     val sf = "[[a,b][c,d]]"
     Test.init( "mex.f.inv2x2.a", "[[1/(a*d-b*c)*d,-1/(a*d-b*c)*b][-1/(a*d-b*c)*c,1/(a*d-b*c)*a]]" ) 
     val ef:Exp = AsciiParse( sf )
     val mf:Mex = Mex(ef)
     val fm:Exp = mf.inv2x2
     Test.test( "mex.f.inv2x2.a", fm.toAscii )

     val sg = "[[x,x^2,x^3][y,y^2][z]]"
     Test.init( "mex.g", "[[x,x^2,x^3][y,y^2,0][z,0,0]]" ) 
     val mg:Exp = AsciiParse( sg )
     Test.test( "mex.g", mg.toAscii )
   
     par(  "mex.h", "[[x^2,y^3,z^4][x^2,y^3,z^4][x^2,y^3,z^4]]" )
    
   }

  def testRun(): Unit = {

    // -------------- x ------------------" )
    pars( "x.y.a...", "5", "dx", "x^y", "x*y", "x/y", "x+y", "x-y", "(x+y)", "-x" )
    pars( "tran.fun", "exp^x", "ln(x)", "sqrt(x)", "log_10(x)", "log_2(x)" )
    pars( "trig.sin", "sin(x)", "cos(x)", "tan(x)", "cot(x)", "sec(x)", "csc(x)" )
 // pars( "trig.ltx", "\\sin(x)", "\\cos(x)", "\\tan(x)", "\\cot(x)", "\\sec(x)", "\\csc(x)" )
    pars( "trig.arc", "arcsin(x)", "arccos(x)", "arctan(x)", "arccot(x)", "arcsec(x)", "arccsc(x)" )
    // -------------- ops ------------------" )
    pars( "x,y,z.a", "x*y*z", "x*y/z", "x*y+z", "x*y-z"  )    
    pars( "x,y,z.b", "x*y*z", "x/y*x", "x+y*x", "x-y*x" )
    // -------------- par ------------------" )
    pars( "paren.a", "x*(y+z)", "x/(y+z)", "x+(y+z)", "x-(y+z)" )
    pars( "paren.b", "x*(y*z)", "x*(y/z)", "x*(y+z)", "x*(y-z)" )    
    pars( "paren.c", "(x*y)*z", "(x/y)*x", "(x+y)*x", "(x-y)*x" )
    // -------------- par op ------------------" )
    pars( "paren.d",  "w*(x+y)*z",   "x/(y+z)/x",   "x+(y*z)+x",   "x-(y*z)-x"  )
    pars( "paren.e", "(x*(y+z))*x", "(x/(y+z))/x", "(x+(y*z))+x", "(x-(y*z))-x" )
    pars( "paren.f", "x*((y+z)*x)", "x/((y+z)/x)", "x+((y*z)+x)", "x-((y*z)-x)" )
    // -------------- pow ------------------" )
    pars( "power.a", "(x+y)^3", "ln(x^2)", "ln(x^2)", "log_2(x^3)", "ln(x^2)", "sqrt(x^4)" )  
    // -------------- secondary ------------------" )
    pars( "exp.d.a", "exp^x", "-x", "dx", "exp^(x+y)", "(x+y)*dx", "-(x+y)"  )
    // -------------- vex mex ------------------" )
    pars( "vex...a",  "[x,x^2,x^3]" )
    pars( "a+mex.a", "a+[[x,x^2,x^3][y,y^2,y^3][z,z^2,z^3]]" ) 
    // -------------- Root ------------------" )
    pars( "root..a", "root_3(x)", "root_3(x+2)", "sqrt(x+2)" ) // "sqrt_3(x+2)" "sqrt[3](x+2)" 
    
    lam( "Lam.sin.a", "sin(x)", "Sin(Var(x))" )
    
  } 

  def testEee(): Unit = {
    lam(  "eee.a", "e^x", "Eee(Var(x))")
    lam(  "eee.b", "e^(x+2)", "Eee(Add(Var(x),Num(2)))")
    lam(  "eee.c", "e", "Var(e)")
    lam(  "eee.d", "ln(e)",   "Lnn(Var(e))")
    lam(  "eee.e", "ln(e^1)", "Lnn(Eee(Num(1)))")
    lam(  "eee.f", "e*x", "Mul(Var(e),Var(x))" )
    lam(  "eee.g", "a-e", "Sub(Var(a),Var(e))" )
    lam(  "eee.h", "e^e^x", "Eee(Eee(Var(x)))" )
    lam(  "eee.i", "d+e", "Add(Var(d),Var(e))")
   }
  
   def testDif(): Unit = {

     dif( "dif.a", "x*y*z",                         "y*z*dx+x*z*dy+x*y*dz" )
     dif( "dif.b", "x^2*y^3*z^4",                   "y^3*z^4*2*x*dx+x^2*z^4*3*y^2*dy+x^2*y^3*4*z^3*dz" )
     dif( "dif.c", "sin(x)+cos(x)+tan(x)",          "cos(x)*dx-sin(x)*dx-sec(x)^2*dx" )
     dif( "dif.d", "csc(x)+sec(x)+cot(x)",          "-csc(x)*cot(x)*dx+sec(x)*tan(x)*dx-csc(x)^2*dx" )
     dif( "dif.e", "arcsin(x)+arccos(x)+arctan(x)", "dx/(1+x^2)" )
     dns( "dif.f", "arccsc(x)+arcsec(x)+arccot(x)", "-dx/(x*sqrt(x^2-1))+dx/(x*sqrt(x^2-1))-dx/(1+x^2)" )

     lam( "dx.a",     "dx", "Dif(Var(x))" ) 
     lam( "Var(d).a", "d",  "Var(d)" ) 
     lam( "d(u).a",   "d(x+y^2)", "Dif(Add(Var(x),Pow(Var(y),Num(2))))" )    
     par( "dif.h",    "z^4*y^3*2*x*dx+z^4*x^2*3*y^2*dy+x^2*y^3*4*z^3*dz" )    
     lam( "dif.i",    "y*dx+x*dy", "Add(Mul(Var(y),Dif(Var(x))),Mul(Var(x),Dif(Var(y))))" ) 
 
   }
 
  def testItg(): Unit = {
    itg( "itg.a", "dx",     "x"       ) 
    itg( "itg.b", "x",      "x^2/2"   ) 
    itg( "itg.c", "x^2",    "x^3/3"   )
    itg( "itg.d", "sin(x)", "-cos(x)" )
    itg( "itg.e", "1/x",    "ln(x)"   ) 
  }
   
   def testFun(): Unit = {
     par(  "num.a", "(1+2)*3*(7-1)" ) 
     par(  "exp.a", "x^2+y^3*z^(a+4)" )  
     par(  "Lnn.a",  "ln(x)" )
     
     lam(  "Lnn.c",  "ln(x)", "Lnn(Var(x))" )     
     par(  "Fun.a",  "sin(x)" )
     pars( "Fun.b",  "sin(x)", "cos(x)", "tan(x)", "cot(x)", "sec(x)", "csc(x)")          
     lam(  "Log.a",  "log_10(x)", "Log(Var(x),Dbl(10.0))" )  // x*3-y^2
     lam(  "Root.a", "root_10(y)", "Roo(Var(y),Dbl(10.0))" )  // (y^5-z+1)       
     lam(  "Eee.a",  "e^x", "Eee(Var(x))" )      
     lam(  "Abs.a",  "|x-y|", "Abs(Sub(Var(x),Var(y)))" )      
     lam(  "Neg.a",  "-(x-y)", "Neg(Sub(Var(x),Var(y)))" )
     par(  "Big.a",  "[a^3,b*i]+sin(x^2)*sqrt(y)-20*ln(z)" )
   }
  
  def testSum(): Unit = {
     par(  "Int.a", "Int_0^x(x)" ) // "Int sub(0)sup(x)(x)" 
     par(  "sum.b", "sum_1^n(x^2)" )
     pars( "Int.c", "Int_1^n(x)", "Int_1^x(x)" )    
     pars( "sum.f", "sum_1^n(i)" )
     pars( "Int,g", "Int_1^x(x)" )     
     pars( "sum.h", "sum_{i=1}^n(x)", "sum_1^x(x)" )
  }
  
  def testEqu(): Unit = {
    lam( "Equ.a", "a+b=c+f", "Equ(Add(Var(a),Var(b)),Add(Var(c),Var(f)))" )
    par( "Equ.b", "(x^2)=3*x" )    
    lam( "Equ.c", "tan(x)=sin(x)/cos(x)", "Equ(Tan(Var(x)),Div(Sin(Var(x)),Cos(Var(x))))" )
    lam( "Equ.d", "cot(x)=cos(x)/sin(x)", "Equ(Cot(Var(x)),Div(Cos(Var(x)),Sin(Var(x))))" )       
    par( "Equ.e", "sin(x^2)=[a^3,b*i]" )
    par( "Equ.f", "sin(x^2)=[[a^3,b*i][x,y]]" )  
  }

  def testErr(): Unit = {
    val sa = "e&x$w~t@"
    Test.init( "Err.a", "error", ' ', sa )
    val ea:Exp = AsciiParse(sa)
    Test.test( "Err.a", ea.toAscii,    ' ', sa  )
   
    par( "Err.b", "sin(x" )
    par( "Err.c", "haha(x)" )
  }
  def testEqs(): Unit = {
    eqs( "Eqs.one", "1",              "1" )
    eqs( "Eqs.var", "x",              "x" )
    eqs( "Eqs.add", "(x+y)",          "(x+y)" )
    eqs( "Eqs.div", "(x+y)/(x+y)",    "(x+y)/(x+y)" )
    eqs( "Eqs.mul", "(x+y)*(x+y)",    "(x+y)*(x+y)" )
  }
  
  def testSim(): Unit = {
    sim( "Sim.q", "(3*5)/(2+3)",          "15/5" )
    sim( "Sim.a", "(x+y)/(x+y)",          "1" )
    sim( "Sim.b", "(x+y)^3/(x+y)^3",      "1" )
    sim( "Sim.c", "(x+y)*(a+b)/(x+y)",    "a+b" )
    sim( "Sim.d", "((x+y)*(a+b))/(x+y)",  "a+b" )
    sim( "Sim.e", "(x+y)/((x+y)*(a+b))",  "1/(a+b)" )   
    sim( "Sim.f", "(w*x*y*z)/(x*y*z)",    "w" )

    sim( "Sim.g", "(x*y*z)/(x*y*z*w)",    "1/w" ) 
    sim( "Sim.h", "(w*x*y*z)/(z*x*w)",    "y" ) 
    sim( "Sim.i", "(w*x*y*x)/(z*x*w)",    "y*x/z" )
    sim( "Sim.j", "((w-q)*x*y*x)/(z*x*w*(w-q))",     "y*x/(z*w)" )    
    sim( "Sim.k", "((w-q)^3*x*y*x)/(z*x*w*(w-q)^2)", "(w-q)*y*x/(z*w)" )
  }
  
  
// Right now subscript and superscript are a problem with stack overflows  
   def testSus(): Unit = {
     lam(  "sus.a", "x_1", "Sus(Var(x),Num(1))" )
     lam(  "sus.b", "x_y", "Sus(Var(x),Var(y))" )
   }
     
 
}
/*
Fail::Sim.g:(x*y*z)/(x*y*z*w) | 1/w | Div(Mul(Mul(Var(x),Var(y)),Var(z)),Mul(Mul(Mul(Var(x),Var(y)),Var(z)),Var(w)))
    ::Sim.g:(x*y*z)/(x*y*z*w) | (x*y*z)/(x*y*z*w) | Div(Mul(Mul(Var(x),Var(y)),Var(z)),Mul(Mul(Mul(Var(x),Var(y)),Var(z)),Var(w)))
Fail::Sim.h:(w*x*y*z)/(z*x*w) | y | Div(Mul(Mul(Mul(Var(w),Var(x)),Var(y)),Var(z)),Mul(Mul(Var(z),Var(x)),Var(w)))
    ::Sim.h:(w*x*y*z)/(z*x*w) | (w*x*y*z)/(z*x*w) | Div(Mul(Mul(Mul(Var(w),Var(x)),Var(y)),Var(z)),Mul(Mul(Var(z),Var(x)),Var(w)))
Fail::Sim.i:(w*x*y*x)/(z*x*w) | y*x/z | Div(Mul(Mul(Mul(Var(w),Var(x)),Var(y)),Var(x)),Mul(Mul(Var(z),Var(x)),Var(w)))
    ::Sim.i:(w*x*y*x)/(z*x*w) | (w*x*y*x)/(z*x*w) | Div(Mul(Mul(Mul(Var(w),Var(x)),Var(y)),Var(x)),Mul(Mul(Var(z),Var(x)),Var(w)))
Fail::Sim.j:((w-q)*x*y*x)/(z*x*w*(w-q)) | y*x/(z*w) | Div(Mul(Mul(Mul(Sub(Var(w),Var(q)),Var(x)),Var(y)),Var(x)),Mul(Mul(Mul(Var(z),Var(x)),Var(w)),Sub(Var(w),Var(q))))
    ::Sim.j:((w-q)*x*y*x)/(z*x*w*(w-q)) | ((w-q)*x*y*x)/(z*x*w*w-q) | Div(Mul(Mul(Mul(Sub(Var(w),Var(q)),Var(x)),Var(y)),Var(x)),Mul(Mul(Mul(Var(z),Var(x)),Var(w)),Sub(Var(w),Var(q))))
Fail::Sim.k:((w-q)^3*x*y*x)/(z*x*w*(w-q)^2) | (w-q)*y*x/(z*w) | Div(Mul(Mul(Mul(Pow(Sub(Var(w),Var(q)),Num(3)),Var(x)),Var(y)),Var(x)),Mul(Mul(Mul(Var(z),Var(x)),Var(w)),Pow(Sub(Var(w),Var(q)),Num(2))))
    ::Sim.k:((w-q)^3*x*y*x)/(z*x*w*(w-q)^2) | ((w-q)^3*x*y*x)/(z*x*w*(w-q)^2) | Div(Mul(Mul(Mul(Pow(Sub(Var(w),Var(q)),Num(3)),Var(x)),Var(y)),Var(x)),Mul(Mul(Mul(Var(z),Var(x)),Var(w)),Pow(Sub(Var(w),Var(q)),Num(2))))
 */