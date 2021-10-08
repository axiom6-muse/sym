package ax6.util

//import scala.annotation.tailrec
import scala.collection.mutable.{StringBuilder => StringBuilderText}
//import scala.collection.IndexedSeq

object Text
{
  type CS = CharSequence

  // Constructors
  def apply( cap:Int ) : Text = new Text(cap)
//def apply( cs:CS   ) : Text = new Text(cs)  // Works for Text String etc
  def apply( sq:CS*  ) : Text =
  {
    var len = 0
    for( s <- sq )
      len += s.length
    val text = new Text(len)
    for( s <- sq )
      text.app(s)
    text
  }

  // Constants
  val tab   : String = "  "
  val eol   : String = "\n"
  val delim : String = ","

  // Empty and type checks
  val empty : Text = new Text(0)
  def is( text:Text  ) : Boolean = text!=null && text!=empty
  def isCS( cs:CS    ) : Boolean = cs!=null && cs.length > 0

  def toUpper( c:Char ) : Char = java.lang.Character.toUpperCase(c)
  def toLower( c:Char ) : Char = java.lang.Character.toLowerCase(c)

  def equ( a:Text, ab:Int, al:Int, b:Text, bb:Int, bl:Int ) : Boolean = { Text.equ( a.sb, ab, al,  b.sb, bb, bl) }
  def equ( a:Text, b:Text )               : Boolean = { Text.equ( a.sb, b.sb ) }
  def equ( a:CS,   b:CS   )               : Boolean = { Text.equ( a,  0, a.length, b, 0, b.length ) }
  def equ( a:CS,   ab:Int, al:Int, b:CS ) : Boolean = { Text.equ( a, ab, al,       b, 0, b.length ) }

  def equ( a:CS,   ab:Int, al:Int, b:CS,   bb:Int, bl:Int ) : Boolean =
  {
    if( a != null && b != null && al == bl )
    {
      var i   = 0
      while( i < al )
      {
        if( a.charAt(ab+i) != b.charAt(bb+i) )
          return false
        i += 1
      }
      return true
    }
    false
  }

}

class Text( _cap:Int )
{
  val sb = new StringBuilderText( _cap )

  type CS    = Text.CS

  def this() = this( 100 )
  def this( cs:CharSequence ) = { this(); app(cs)   }
  def this( text:Text )       = { this(); app(text) }

  override def toString  : String  = { sb.toString() }

  def charAt(i:Int)           : Char   = { sb.charAt(i) }
  def setCharAt(i:Int,c:Char) : Unit   = { sb.setCharAt(i,c) }
  def len                     : Int    = { sb.size }
  def cap                     : Int    = { sb.capacity }
  def indexOf( c:Char )       : Int    = { sb.indexOf(c) }
  def indexOf( s:String )     : Int    = { sb.indexOf(s) }
  def toCS                    : CS     = { sb.asInstanceOf[CS] }
  def toStr                   : String = { sb.toString() }
  def noop( str:String )      : Unit   = { if( len < 0 ) app(str) } // A dumb noop()
  def toText( a:Array[Text] ) : Text = {
    val tx:Text = new Text()
    for( e <- a ) {
      tx.app( e ) }
    tx
  }

  def cap( _cap:Int )    : Unit    = { sb.ensureCapacity(_cap) }
  def grow( inc:Int )    : Unit    = { sb.ensureCapacity( cap + inc ) }
  def clear()            : Unit    = { sb.clear() }

  def in( b:Int, e:Int ) : Boolean = in(b)&& in(e-1) && b <= e
  def in( i:Int )        : Boolean = 0 <= i && i < len
  def head()             : Char    = sb.charAt(0)
  def tail()             : Char    = if(sb.nonEmpty ) sb.charAt(sb.size-1) else '\u0000'
  def has(     c:Char )  : Boolean = sb.contains( c )
  def hasTail( c:Char )  : Boolean = tail()==c
  def delTail()          : Unit = { if( len > 0 ) sb.setLength(sb.size-1) }
  def delTail(c:Char)    : Unit = { if( hasTail(c)) delTail() }
  def delHead()          : Unit = {
    for (i <- sb.indices) {
      if( i < sb.size - 1 )
        sb(i) = sb(i + 1)
    }
    sb.setLength(sb.size - 1)
  }

  // ... appends ....
  def app( str:String    ) : Unit = { sb.append( str ) }
  def app( cs:CS         ) : Unit = { sb.append( cs  ) }
  def app( c:Char        ) : Unit = { sb.append( c   ) }
  def app( b:Byte        ) : Unit = { sb.append( b   ) }
  def app( s:Short       ) : Unit = { sb.append( s   ) }
  def app( i:Int         ) : Unit = { sb.append( i   ) }
  def app( n:Long        ) : Unit = { sb.append( n   ) }
  def app( f:Float       ) : Unit = { sb.append( f   ) } // app( f, dec(f) ) }
  def app( d:Double      ) : Unit = { sb.append( d   ) } // app( s, dec(s) ) }
  def app( b:Boolean     ) : Unit = { if(b) sb.append("true") else sb.append("false") }
  def app( a:Array[Char] ) : Unit = { sb.appendAll( a ) }
  def app( a:Array[Text] ) : Unit = { app( toText(a) ) }
  def app( t:Text        ) : Unit = { if( t!=this ) sb.append( t ) }
  def app( a:Any         ) : Unit = { sb.append(a) }
  def app( a:Seq[Any]    ) : Unit = { seq(a) }

  def all( args:Any*     ) : Unit = { for( arg<-args ) app(arg) }
  def seq( args:Seq[Any] ) : Unit = { for( arg<-args ) app(arg) }
  def sed( args:Seq[Any] ) : Unit = {
    for(  i <- args.indices ) {
      if( i <  args.length-1 ) { app( args(i) ); app(" | ") }
      else                     { app( args(i) ) }
    }
  }
  
  def replace( a:Char, b:Char ) : Unit =
    { for( i <- sb.indices ) if( a == sb.charAt(i) ) sb.setCharAt(i,b) }

  def delim( mid:CS, args:Seq[Any] ) : Unit = { delim( "", mid, "", args ) }

  def delim( beg:CS, mid:CS, end:CS, args:Seq[Any] ): Unit = {
    app( beg )
    for( i <- args.indices )
    {
      if(  i < args.length-1 ) {
        app( args(i) )
        app( mid     ) }
      else
        app( args(i) )
    }
    app( end )
  }

  def text( args:Any* ) : Text = { seq(args); this }
  def att( name:CS, value:Any ) : Unit = { all( " ", name, "=\"",  value, "\""   ); replace('_','-') }
  def css( name:CS, value:Any ) : Unit = { all( " ", name, ": ",   value, "; "   ); replace('_','-') }

  def css( name:CS, value:Any, uom:String ) : Unit =
    { all(" ", name, ": ", value, uom, "; "); replace('_','-') }

  def space( n:Int ) : Unit =  { for(_ <-0 until n) app(' '     ); }
  def tab(   n:Int ) : Unit =  { for(_ <-0 until n) app(Text.tab); }

  def tab( n:Int, args:Any* ) : Unit = { tab(n); seq(args) }

  def rem( dbl:Double )             : Double = dbl - Math.floor(dbl)
  def dig( rem:Double, mul:Int    ) : Int    = (rem * mul).toInt % 10
  
} // End of class Text