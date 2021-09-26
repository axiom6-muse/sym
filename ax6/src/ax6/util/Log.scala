
package ax6.util

object Log
{
  val  text:Text = new Text(1000)
  type CS        = Text.CS

  // ... log and put output
  // log is the most called method that clears the buffer, delimits args and
  //   outputs a newline
  // put is more generic in that does not delimit the args and does not
  //  output a newline so put can be called multiple times to log a line of text

  def log( args:Any* ): Unit = { text.clear(); arg(args); eol() }
  def put( args:Any* ): Unit = { text.clear(); seq(args); out() }

// .. Format args ...

  def app(   args:Any*     ): Unit = { text.seq(args)  }
  def seq(   args:Seq[Any] ): Unit = { text.seq(args)  }
  def delim( args:Seq[Any] ): Unit = { text.delim(" ",args)  }

// ... log error trace stack output ...

// Delimit with spaces and output args
   def arg( args:Seq[Any] ): Unit = { text.delim(" ",args); out() }

// ... error trace stack ...
  def error(        args:Any* ): Unit = { msg("Error:  "); arg(args); tracen(1); eol() }
  def error( n:Int, args:Any* ): Unit = { msg("Error:  "); arg(args); tracen(n); eol() }
  def trace( n:Int, args:Any* ): Unit = { msg("Trace:  "); arg(args); tracen(n); eol() }

  def except( e:Exception, args:Any* ): Unit = {
    msg("Except: "); arg(args)
    if( e != null )
      { app( " (", e.getMessage, ") " ); out(); tracen(4) }
    eol()
  }

  def stack( e:Exception, args:Any* ): Unit = {
    msg("Stack:  "); arg(args)
    if( e != null )
        e.printStackTrace()
    eol()
  }

  def fatal( args:Any* ): Unit = { msg("Fatal:  "); arg(args); tracen(6); eol() /*; Io.exit() */ }

// ... tracen ...

  private def tracen( n:Int ): Unit = { tracen( n, new Exception ) }

  private def tracen( n:Int, except:Exception ): Unit = {
    text.clear()
    val stack  = except.getStackTrace
    var name   = "Unknown"

    app(" [")
    for( i <- 2 until Math.min(n+2,stack.length) )
    {
      name = stack(i).getFileName
      name = name.substring( 0, name.indexOf('.') )
      app( name, '.', stack(i).getMethodName, ':', stack(i).getLineNumber, ' '  )

    }
    text.setCharAt( text.len-1, ']' )
    out()
  }

// ... tab array ...

  def array( tag:CS, n:Int, arr:Array[Char] ): Unit = {
     text.all(tag, ' ', n, " [ ")
     for( a <- arr )
       text.all(a, ' ')
     if( text.len > 1 )
         text.delTail()
     text.app( " ]" )
     out()
     eol()
  }

  def tab( n:Int, args:Any* ): Unit = {
     text.tab(n)
     text.seq(args)
     out()
     eol()
  }

// ... Console Output ...

  def out(): Unit = {
    for( i <- 0 until text.len )
      Console.out.print( text.charAt(i) )
    Console.out.flush()
    text.clear()
  }

// ... MessageConsole Output ...
   def msg( cs:CS ): Unit = {
     for( i <- 0 until cs.length )
       Console.out.print( cs.charAt(i) )
     Console.out.flush()
   }

  def eol(): Unit = { msg( Text.eol ) }
  def tab(): Unit = { msg( Text.tab ) }
  def space(): Unit = { msg( " "      ) }

}