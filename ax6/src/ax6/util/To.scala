
package ax6.util

import  java.util.Date
//import  desk.app.Enums.{ unEnum, Uom }
//import  draw.csss.Cse.{ uoms }

object To
{   
  type c    = Int // Coordinate representation can only be Int, Float or Double
  type CS   = CharSequence
  type ID   = Int
  // type Enum = java.lang.Enum

  var  idv : ID    = 0
  val  ctype:c     = 0
  val  FTOL:Float  = 0.000001f
  val  DTOL:Double = 0.000000001

  def id : ID = { idv +=1; idv }

  //  var  text:Text   = new Text(200)
  //     text.sync(true)

  def ses : String = "ses"

// ... is as to ...

  //  def is[T]( any:Any ) : Boolean = any.isInstanceOf[T]
  def as[T]( any:Any ) : T       = any.asInstanceOf[T]
  def to[T]( any:Any ) : T       = any.asInstanceOf[T]
/*
  def to[T]( any:Any ) : T       = 
  {
    if( any.isInstanceOf[T] ) 
        any.asInstanceOf[T] 
    else
      Log.error( 3, "Cast error" )
    return as[T](null)     
  }
*/
// ... eq ne ...

  def ne( a:CS, b:CS ) : Boolean = !eq(a,b)
  def eq( a:CS, b:CS ) : Boolean =
  {
      if( a != null && b != null && a.length == b.length )
      {
          val len = a.length
          var i   = 0
          while( i < len )
          {
              if( a.charAt(i) != b.charAt(i) )
                  return false
              i = i + 1
          }
          return true
      }
      false
  }

  def eq( a:CS, ab:Int, al:Int, b:CS ) : Boolean = 
      eq( a,    ab,     al,     b, 0, b.length )

  def eq( a:CS, ab:Int, al:Int, 
          b:CS, bb:Int, bl:Int ) : Boolean =
  {
      if( a != null && b != null && al == bl )
      {
          var i   = 0
          while( i < al )
          {
              if( a.charAt(ab+i) != b.charAt(bb+i) )
                  return false
              i = i + 1
          }
          return true
      }
      false
  }

  def eqci( a:CS, b:CS ) : Boolean =
  {
      if( a != null && b != null && a.length == b.length )
      {
          val len = a.length
          var i   = 0
          while( i < len )
          {
              if( notci( a.charAt(i), b.charAt(i) )  )
                  return false
              i = i + 1
          }
          return true
      }
      false
  }

   def notci( a:Char, b:Char ) : Boolean =
    Character.toLowerCase(a) != Character.toLowerCase(b)

  // def eq( a:Enum, b:Enum ) : Boolean =  a.equals(b)
  // def ne( a:Enum, b:Enum ) : Boolean = !a.equals(b)

  def eq( a:Int,    b:Int    ) : Boolean = a==b     // Called when type c converts to Int
  def eq( a:Float,  b:Float  ) : Boolean = b-FTOL <= a && a <= b+FTOL
  def eq( a:Double, b:Double ) : Boolean = b-DTOL <= a && a <= b+DTOL

  def ne( a:Int,    b:Int    ) : Boolean = a!=b     // Called when type c converts to Int
  def ne( a:Float,  b:Float  ) : Boolean = !eq(a,b)
  def ne( a:Double, b:Double ) : Boolean = !eq(a,b)


  def ne( a:Array[Float],  b:Array[Float]  ) : Boolean = !eq(a,b)
  def eq( a:Array[Float],  b:Array[Float]  ) : Boolean = 
  {
    if( a.length==b.length )
    {
      for( i <- a.indices)
        { if( ne(a(i),b(i)) ) return false }
      true
    }
    else
      false
  }

  def ne( a:Array[Double], b:Array[Double] ) : Boolean = !eq(a,b)
  def eq( a:Array[Double], b:Array[Double] ) : Boolean = 
  {
    if( a.length==b.length )
    {
      for( i <- a.indices)
        { if( ne(a(i),b(i)) ) return false }
      true
    }
    else
      false
  }

/*
  def eq( a:CS, any:Any ) : Boolean =
  {
      var q:Boolean = false
      if( is[CS](any) )
         q = eq( a, as[CS](any) )
      else
         q = eq( a, any.toString )
      return q
  }
*/

  def in( ch:Char, seq:CS ) : Boolean =
  { 
      val n = seq.length
      var i = 0
      while( i < n )
      {
          if( ch == seq.charAt(i) )  
              return true
          i = i + 1                    
      }
      false
  }

// ... digit hex ...

   val digits = "0123456789"
   def digit( i:Int  ) : Char = digits.charAt(i%10)
   def digit( i:Long ) : Char = digits.charAt((i%10).toInt)
   val hexs   = "0123456789ABCDEF"
   def hex(  i:Int )  : Char = hexs.charAt(i%16)

// isDig isInt, isHex, isDbl

   def isInt( s:CS ) : Boolean =
   {
    val len = s.length
    val i   = 0
    var c   = ' '
    while( i < len )
    {
      c = s.charAt(i)
      if( !( Character.isDigit(c) || c == '-' ) )
         return false
    }
    true
   }

  def isHex( c:Char ) : Boolean =
  {
  val i   = 0
  while( i < 16 )
  {
    if( c == hexs.charAt(i) )
      return true
  }
  false
  }

   def isHex( s:CS ) : Boolean =
   {
    val i = 0
    while( i < s.length )
    {
      if( !isHex(s.charAt(i)) )
         return false
    }
    true
   }

  def isDbl( s:CS ) : Boolean =
  {
    val len = s.length
    val i   = 0
    var c   = ' '
    while( i < len )
    {
      c = s.charAt(i)
      if( ! ( Character.isDigit(c) || c =='.'  || c =='-' || c =='E'  || c =='e' ) )
         return false
    }
    true
  }

// ... Math Functions ...

  def ln(    x:Double ) : Double = Math.log(x)
  val ln2  : Double = ln(2.0)
  val ln10 : Double = ln(10.0)
  val ln16 : Double = ln(16.0)
  def log2(  x:Double ) : Double = ln(x) / ln2
  def log10( x:Double ) : Double = ln(x) / ln10
  def log16( x:Double ) : Double = ln(x) / ln16
  def log(   x:Double, n:Double ) : Double = ln(x) / ln(n)

  def rnd( a:Any ) : Double = a match
  {
    case i:Int    => i.toDouble
    case f:Float  => Math.round(f).toFloat
    case d:Double => Math.round(d).toDouble
  }

  def rnf( a:Any ) : Float = a match
  {
    case i:Int    => i.toFloat
    case f:Float  => Math.round(f).toFloat
    case d:Double => Math.round(d).toFloat
  }

  def rni( a:Any ) : Int = a match
  {
    case i:Int    => i
    case f:Float  => Math.round(f)
    case d:Double => Math.round(d).toInt
  }

  def rnc( a:Any ) : Int = a match
  {
    case i:Int    => convert(i)
    case f:Float  => convert(Math.round(f))
    case d:Double => convert(Math.round(d).toInt)
  }

  def up(  x:Double ) : Int = Math.floor(x).toInt
  def dn(  x:Double ) : Int = Math.ceil(x).toInt

// ... min max rnd ...

  def min( s1:Int,    s2:Int    ) : Int    = { if(s1<s2) s1 else s2 }    
  def min( s1:Float,  s2:Float  ) : Float  = { if(s1<s2) s1 else s2 }
  def min( s1:Double, s2:Double ) : Double = { if(s1<s2) s1 else s2 }
  
  def max( s1:Int,    s2:Int    ) : Int    = { if(s1>s2) s1 else s2 }
  def max( s1:Float,  s2:Float  ) : Float  = { if(s1>s2) s1 else s2 }
  def max( s1:Double, s2:Double ) : Double = { if(s1>s2) s1 else s2 }  

  def dis( s1:Int,    s2:Int    ) : Int    = { abs(s2-s1) }
  def dis( s1:Float,  s2:Float  ) : Float  = { abs(s2-s1) }
  def dis( s1:Double, s2:Double ) : Double = { abs(s2-s1) }

  def abs( s1:Int               ) : Int    = { if(s1>=0  ) s1 else -s1 } 
  def abs( s1:Float             ) : Float  = { if(s1>=0  ) s1 else -s1 } 
  def abs( s1:Double            ) : Double = { if(s1>=0.0) s1 else -s1 } 

  def min( s1:Int,    s2:Int,    s3:Int    ) : Int    = min(min(s1,s2),s3)
  def min( s1:Float,  s2:Float,  s3:Float  ) : Float  = min(min(s1,s2),s3)
  def min( s1:Double, s2:Double, s3:Double ) : Double = min(min(s1,s2),s3) 

  def max( s1:Int,    s2:Int,    s3:Int    ) : Int    = max(max(s1,s2),s3)
  def max( s1:Float,  s2:Float,  s3:Float  ) : Float  = max(max(s1,s2),s3)
  def max( s1:Double, s2:Double, s3:Double ) : Double = max(max(s1,s2),s3) 

  def min( s1:Int,    s2:Int,    s3:Int,    s4:Int    ) : Int    = min(min(s1,s2),min(s3,s4))
  def min( s1:Float,  s2:Float,  s3:Float,  s4:Float  ) : Float  = min(min(s1,s2),min(s3,s4))
  def min( s1:Double, s2:Double, s3:Double, s4:Double ) : Double = min(min(s1,s2),min(s3,s4))

  def max( s1:Int,    s2:Int,    s3:Int,    s4:Int    ) : Int    = max(max(s1,s2),max(s3,s4))
  def max( s1:Float,  s2:Float,  s3:Float,  s4:Float  ) : Float  = max(max(s1,s2),max(s3,s4))
  def max( s1:Double, s2:Double, s3:Double, s4:Double ) : Double = max(max(s1,s2),max(s3,s4))

  def diam( x1:c, y1:c, x2:c, y2:c ) : c = To.max( To.dis(x1,x2), To.dis(y1,y2) )

  def div( s1:Int, s2:Int ) : Double =    
  { 
      var d:Double  = s1.toDouble
      if( s2==0 )
          Log.error( "divide by zero", To.str(s1), To.str(s2) )
      else 
          d = s1.toDouble/s2.toDouble
      d
  }

  def div( s:c, d:Double  ) : c =
  {
  var q : c = s
  if( eq(d,0.0) )
    Log.error( "divide by zero", To.str(s), To.str(d) )
  else
    q = rnc(s/d) 
  q
  }

  def div( s:Double, d:Double  ) : Double =
  {
  var q : Double = s
  if( eq(d,0.0) )
    Log.error( "divide by zero", To.str(s), To.str(d) )
  else
    q = s / d
  q
  }    

// ... un defined ...

//val unEnu:Enum           = unEnum.none
  val unChr:Char           = '\u0000'
  val unByt:Byte           = -99 // Math.MIN_BYTE
  val unShr:Short          = -99 // Math.MIN_SHORT
  val unInt:Int            = -99 // Math.MIN_INT
//val unUom:Uom            = Uom.none
  val unRgb:Int            = unInt
  val unLng:Long           = -99 // Math.MIN_LONG
  val unFlt:Float          = -99.0F // Math.NaN_FLOAT
  val unDbl:Double         = -99.0  // Math.NaN_DOUBLE
  val unStr:String         = ""
  val unSta:Array[String]  = new Array[String](0)
  val unArc:Array[c]       = new Array[c](0)
  val unObj:Object         = new Object()
  val unBol:Boolean        = false
//val unTok:Tok            = Tok.empty
  val unTxt:Text           = Text.empty
  val unTxa:Array[Text]    = new Array[Text](0)    
//val unUri:Uri            = Uri.empty
  val unDat:Date           = new Date( 0L )
  val unTim:Date           = unDat

 // ....................... Hav checks for set values ..........................
  
//def hav( v:Enum      ) : Boolean  = v != unEnu
  def hav( v:Char      ) : Boolean  = v != unChr
  def hav( v:Byte      ) : Boolean  = v != unByt
  def hav( v:Short     ) : Boolean  = v != unShr
  def hav( v:Int       ) : Boolean  = v != unInt
//def hav( v:Uom       ) : Boolean  = v != unUom
  def hav( v:Long      ) : Boolean  = v != unLng
  def hav( v:Double    ) : Boolean  = v != unDbl
  def hav( v:Float     ) : Boolean  = v != unFlt
  
//def hav( v:Tok           ) : Boolean  = v != null && v.length > 0 && v != unTok
  def hav( v:Text          ) : Boolean  = v != null && v.len > 0 && v != unTxt
//def hav( v:Uri           ) : Boolean  = v != null && v != unUri
  def hav( v:String        ) : Boolean  = v != null && v.nonEmpty && v != unStr
  def hav( v:Array[String] ) : Boolean  = v != null && v.length > 0 && !(v sameElements unSta)
  def hav( v:CS            ) : Boolean  = v != null && v.length > 0
  def hav( v:Date          ) : Boolean  = v != null && v != unDat     
  def hav( v:Array[Float]  ) : Boolean  = v != null && v.length > 0

  // ... str ... 

  val NaN = "NaN"

  def str( i:Int     ) : String = { if(hav(i)) String.valueOf(i) else NaN }  
  def str( n:Long    ) : String = { if(hav(n)) String.valueOf(n) else NaN }   
  def str( d:Double  ) : String = { if(hav(d)) String.valueOf(d) else NaN }   
  def str( f:Float   ) : String = { if(hav(f)) String.valueOf(f) else NaN } 
  def str( b:Boolean ) : String = { if(b)  "true" else "false"; } 
  def str( s:CS      ) : String = 
  { 
  if( hav(s) ) 
    { if( !s.isInstanceOf[String] ) s.toString else To.as[String](s) }
  else
    unStr
  }
   
  def str( f:Array[Float] ) : String =
  { 
    var s:String = unStr
    if( hav(f) )
    {
      s = new String()
      for( i <- f.indices)
      {
        if(i==f.length-1) s = s + str(f(i)) + ", "
        else              s = s + str(f(i))
      }
    }
    s
  }
  /*
  def str( f:Float, u:Uom ) : String =
  {
  var s = str(f)
  if( u!=Uom.none && u!=Uom.px )
    s = s + u.name
  s
  }
  */
// ... conversions from Int Float Double ...

  def convert( i:Int    ) : Int = i
  def convert( f:Float  ) : Int = f.toInt
  def convert( d:Double ) : Int = d.toInt

  def coord( a:Any ) : Int = a match
  {
    case i:Int    => convert(i)
    case f:Float  => Math.round(f)
    case d:Double => Math.round(d).toInt
    case s:CS     => convert(num(s))
  }

  def num( a:Any ) : Int = a match
  {
    case d:Double => d.toInt
    case f:Float  => f.toInt
    case i:Int    => i
    case _        => Log.trace( 4, a.toString ); 0
  }

  def flt( a:Any ) : Float = a match
  {
  case i:Int    => i.toFloat
  case f:Float  => f
  case d:Double => d.toFloat
  }

  def dbl( a:Any ) : Double = a match
  {
  case i:Int    => i.toDouble
  case f:Float  => f.toDouble
  case d:Double => d
  }

// ... conversions from CS ...

  def bol( seq:CS ) : Boolean = eq( seq, "true" )    
  def byt( seq:CS ) : Byte    = lng(seq).toByte  
  def num( seq:CS ) : Int     = lng(seq).toInt
  def shr( seq:CS ) : Short   = lng(seq).toShort
  def flt( seq:CS ) : Float   = dbl(seq).toFloat
  def chr( seq:CS ) : Char    = if(hav(seq)) seq.charAt(0) else unChr    
  def txt( seq:CS ) : Text    = if(hav(seq)) new Text(seq) else unTxt   
//def rgb( seq:CS ) : Int     = Color.toRGB(seq)

  def len( seq:CS ) : Int =
  {
  var ch : Char = unChr
  val ln : Int  = if(hav(seq)) seq.length else 0
  var i  : Int  = ln - 1
  while( i >= 0 )
  {
    ch = seq.charAt(i)
    if( Character.isDigit(ch) || ch == '.' )
      return i+1
    i = i - 1
  }
  ln
  }
  
  def lng( seq:CS ) : Long =
  {
  var pow : Int  = 1
  var lng : Long = 0L
  var i   : Int  = len(seq) - 1
  while( i >= 0 && Character.isDigit(seq.charAt(i)) )
  {
    lng = lng + Character.digit( seq.charAt(i), 10 ) * pow
    pow = pow * 10
    i = i - 1
  }
  if( lng == unLng )
    Log.error( "lng", seq )
  lng
 }
   
  def hex( seq:CS ) : Int =
  {
  var pow : Int  = 1
  var hx  : Int  = 0
  var ch  : Char = unChr
  val i   : Int  = len(seq) - 1
  while( i >= 0 )
  {
    ch = seq.charAt(i)
    if( ch=='#' || ch=='X' || ch== 'x' )
      return hx
    hx = hx + Character.digit( ch, 16 ) * pow
    pow = pow * 16
  }
  hx
  } 
  
  def dbl( seq:CS ) : Double =
  {
  val ln  : Int    = len(seq)
  var ch  : Char   = To.unChr
  val dc  : Int    = dec(seq,ln)
  var pow : Double = if(dc==0) 1.0 else Math.pow( 10.0, -dc )
  var db  : Double = 0.0
  var i   : Int    = ln - 1

  while( i >=0 )
  {
    ch = seq.charAt(i)
    if( ch != '.' )
       { db = db + Character.digit( ch, 10 ) * pow; pow = pow * 10; }
    i = i - 1 
  }
  // Log.log( "dbl", String.valueOf(dbl) );
  db
  }  

  def dec( seq:CS, len:Int ) : Int =
  {
  var i : Int = len - 1
  while( i >= 0 )
  {
    if( seq.charAt(i) == '.' )
       return len-i-1
    i = i - 1
  }
  0
  }

// .. uom ...
  /*
  def uom( seq:CS ) : Uom =
  {
  val b : Int = len(seq)
  val e : Int = seq.length
  if( b==e )
    return Uom.none

  var i = 0
  while( i < uoms.length )
  {
    if( eq( seq, b, e, uoms(i) ) )
      return Uom.values()(i)
    i = i + 1
  }
  Uom.none
  }
  */
// ... tok parser ....
/*
def coord( tag:CS, toks:Array[Tok], ntok:Int, defv:c ) : c =
{
  var v : c   = defv
  var i : Int = 0
  while( i < ntok )
  {
      if( eq(tag,toks(i)) )
        return coord(toks(i+1))
      i += 1
  }
  v
}

def num( tag:CS, toks:Array[Tok], ntok:Int, defv:Int ) : Int =
{
  var v : Int = defv
  var i : Int = 0
  while( i < ntok )
  {
      if( eq(tag,toks(i)) )
        return num(toks(i+1))
      i += 1
  }
  v
}

def dbl( tag:CS, toks:Array[Tok], ntok:Int, defv:Double ) : Double =
{
  var v : Double = defv
  var i : Int    = 0
  while( i < ntok )
  {
      if( eq(tag,toks(i)) )
        return dbl(toks(i+1))
      i += 1
  }
  return v
}


def str( tag:CS, toks:Array[Tok], ntok:Int, defv:String ) : String =
{
  var v : String = defv
  var i : Int    = 0
  while( i < ntok )
  {
      if( eq(tag,toks(i)) )
        return str(toks(i+1))
      i += 1
  }
  v
}

def text( tag:CS, toks:Array[Tok], ntok:Int, defv:Text ) : Text =
{
  var v : Text = defv
  var i : Int    = 0
  while( i < ntok )
  {
      if( eq(tag,toks(i)) )
        return new Text(toks(i+1))
      i += 1
  }
  v
}
*/

}
