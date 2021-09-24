
package ax6.test

import ax6.util.{Log, Text}

import scala.collection.mutable.ArrayBuffer

object Test
{
   val  text:Text = new Text(1000)
   val  array     = new ArrayBuffer[Text](0)
   val  sep       = "::"
   type CS        = Text.CS

  def app( args:Any* ): Unit = {
     for( arg<-args )
       text.any( arg )
  }

  def tab( n:Int, args:Any* ): Unit = {
    text.tab( n )
     for( arg<-args )
       text.any( arg )
  }

  def cmp( node:Text ) : Boolean =
  {
    val t1 = text
    val t2 = node
    val n1 = t1.indexOf(sep)
    val n2 = t2.indexOf(sep)
    Text.equ(t1,0,n1,t2,0,n2)
  }

  def keep( name:CS, args:Any* ): Unit = {
    text.clear()
    text.app((name, sep))
    text.seq( args )
    array :+ text
    Console.out.print(("Test.keep", text, array ))
    text.clear()
  }

  def keep2( name:CS, args:Any* ): Unit = {
    text.clear()
    text.app((name, sep))
    Console.out.print(("Test.keep", array))
    val index = array.indexOf( text )
    if( index >= 0 )
    {
      text.clear()
      text.seq( args )
      // val node = app(text)
      app(text)
    }
    else
    {
      text.seq( args )
      //val text2 = new Text(text)
      array :+ text
    }
    text.clear()
  }

  def beg( name:CS ): Unit = { text.clear(); text.app((name, sep)) }

  def test( name:CS, args:Any* ): Unit = {
    beg( name )
    text.seq( args )
    end( name )
  }

  def end( name:CS ): Unit = {
    val n   = text.indexOf(sep)
    if( Text.equ( text.toCS, 0, n, name, 0, name.length ) )
    {
      val index = array.indexOf( text )
      Console.out.print(( "Test.end", index, text, array ))
      if( index >= 0 )
      {
        val keep = array(index)
        if( keep.equals(text) )
          { Log.msg("Pass::"); Log.msg(text.toCS);  Log.eol() }
        else
          { Log.msg("Fail::"); Log.msg(keep.toCS); Log.eol()
            Log.msg("    ::"); Log.msg(text.toCS); Log.eol() }
      }
      else
        {  Log.msg("Miss::"); Log.msg(text.toCS);  Log.eol() }
    }
    else
      Log.log( 2, "Need to call Log.beg(\"", name, "\")" )  // Log.trace
  }
}


