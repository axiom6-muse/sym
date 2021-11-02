package ax6.math.serve
// import  ax6.math.exp._
// import  ax6.math.num._
import  ax6.math.parse.AsciiParse

object Serve {

  def mathML(    ascii:String ) : String = AsciiParse(ascii).toMathML
  def ast(       ascii:String ) : String = AsciiParse(ascii).toAst
  def simplify(  ascii:String ) : String = AsciiParse(ascii).sim.toAscii
  def differ(    ascii:String ) : String = AsciiParse(ascii).dif.sim.toAscii
  def integrate( ascii:String ) : String = AsciiParse(ascii).itg.sim.toAscii
  def latex(     ascii:String ) : String = AsciiParse(ascii).toLatex

  def calculate( ascii:String, keyvals:String ) : String = AsciiParse(ascii).toCalc( keyvals ) // Needs testing
}
