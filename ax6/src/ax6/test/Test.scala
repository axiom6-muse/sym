
package ax6.test

import ax6.util.{ Log, Text }

object Test
{
  val  _expects:Text = new Text(1000)
  val  _results:Text = new Text(1000)

  def init( name:String, expects:Any* ): Unit = {
    _expects.clear()
    _expects.app(name+":")
    _expects.sed( expects )
  }

  def test( name:String, results:Any* ): Unit = {
    _results.clear()
    _results.app(name+":")
    _results.sed( results )
    if( Text.equ( _expects, _results ) ) {
      Log.msg("Pass::"); Log.log(_expects) }
    else {
      Log.msg("Fail::"); Log.log(_expects)
      Log.msg("    ::"); Log.log(_results) }
  }
}
