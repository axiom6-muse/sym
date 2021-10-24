
package ax6.util

import scala.reflect.ClassTag

// ------------------------------- Tode[T] -------------------------------------

class Tode[T](  _elem:T )
{
  var elem             : T       = _elem
  var level            : Int     = 0
  def term             : Tode[T] = Tree.term[T]
  @transient var prevn : Tode[T] = term
  @transient var nextn : Tode[T] = term
  @transient var paren : Tode[T] = term
  @transient var child : Tode[T] = term
  @transient var lastn : Tode[T] = term

  def next() : Tode[T] = { if( child != term ) child else nextn }
  def prev() : Tode[T] = { if( prevn != term ) prevn else paren }
  def hasNext: Boolean = next() != term
  def hasPrev: Boolean = prev() != term
}

// ------------------------------- Tree[T] -------------------------------------

class Tree[T]()
{
  var size : Int   = 0

  @transient var root:Tode[T] = term
  
// ... bounds ...
  def head                 : Tode[T] = root.child
  def head( node:Tode[T] ) : Tode[T] = node.paren.child
  def tail                 : Tode[T] = root.lastn
  def tail( node:Tode[T] ) : Tode[T] = node.paren.lastn
  def term                 : Tode[T] = Tree.term[T]
  def in( node:Tode[T] )   : Boolean = node!=term && node!=null

  def isRoot(   node:Tode[T] ) : Boolean = in(node)      &&  node == root
  def isHead(   node:Tode[T] ) : Boolean = in(node)      &&  node == head(node)
  def isTail(   node:Tode[T] ) : Boolean = in(node)      &&  node == tail(node)
  def isBranch( node:Tode[T] ) : Boolean = !isRoot(node) &&  in(node.child)
  def isLeaf(   node:Tode[T] ) : Boolean = !isRoot(node) && !in(node.child)

// ... params ...

  def isEmpty : Boolean = size == 0

  def level( node:Tode[T] ) : Int =
  {
    var lev   = 0
    var paren = node.paren
    while( in(paren) )
     { lev = lev + 1; paren = paren.paren }
    node.level = lev
    lev
  }

  def numChild( node:Tode[T] ) : Int =
  {
    var num   = 0
    var child = node.child
    while( in(child) )
      { num = num + 1; child = child.nextn }
    num
  }

// ... inc dec ...

  private def inc( node:Tode[T] ) : Unit =
  {
    size += 1
    node.level = node.paren.level + 1
  }

  private def dec() : Unit =   // dec( node:Tode[T] )
  {
    size -= 1
  }

// ... add ins del ...

  def add( elem:T ) : Tode[T] = {
    if( in(root) ) add( root, elem ) else { root = new Tode[T](elem); root } }

  def add( parent:Tode[T], elem:T ) : Tode[T] =
  {
    if( !in(parent) )
      return term

    val child = new Tode[T](elem)
    child.paren = parent
    if( !in(parent.child) )
    {
      parent.child = child
      parent.lastn = child
    }
    else if( parent.child == parent.lastn )
    {
      child.prevn        = parent.child
      parent.lastn       = child
      parent.child.nextn = parent.lastn
      parent.lastn.prevn = parent.child
    }
    else     // Add after tail child
    {
      child.prevn       = parent.lastn
      parent.lastn.nextn = child
      parent.lastn      = child
    }
    inc(child)
    child
  }

  def ins( elem:T ) : Tode[T] = {
    if( in(root) ) ins( root, elem ) else { root = new Tode[T](elem) ; root } }

  def ins( parent:Tode[T], elem:T ) : Tode[T] =
  {
    if( !in(parent) )
      return term

    val child = new Tode[T](elem)
    child.paren = parent
    if( !in( parent.child ) )
    {
      parent.child = child
      parent.lastn = child
    }
    else     // Insert before head child
    {
      child.nextn        = parent.child
      parent.child.prevn = child
      parent.child       = child
    }
    inc(child)
    child
 }

  def del( elem:T ) : Tode[T] = del( find(elem) )

  def del( node:Tode[T] ) : Tode[T] =
  {
    if( !in(node) )
      return term

    // Reset paren references
    if( node.paren.child == node )    // Parent child
      { node.paren.child =  node.nextn }
    if( node.paren.lastn  == node )    // Parent lastn
      { node.paren.lastn  =  node.prevn }

    // Reset sibling references
    if( in(node.prevn) )                     // Sibling prevn
      { node.prevn.nextn = node.nextn }
    if( in(node.nextn) )                     // Sibling nextn
      { node.nextn.prevn = node.prevn }

    dec()
    node
  }

  def clear() : Unit =
  {
    var node = root
    while( in(node) )
    {
      val next = node.next()
      del( node )
      node = next
    }
  }

  def find( elem:T ) : Tode[T] =
  {
    if( root.elem == elem ) return root
    var next = root.child
    while( in(next) ) {
      if( next.elem == elem )  return next
      next = next.next() }
    term
  }

  // ... for comprehensions ...

  def foreach[U]( func:T => U ) : Unit =
  {
    recurse( root )( func )
  }

  def recurse[U]( node:Tode[T] )( func:T => U ) : Unit =
  {
    func(node.elem)
    var child = node.child
    while( in(child) )
    {
      recurse(  child )( func )  // Depth first recursion
      child = child.nextn        // then traverse sibling
    }
  }

  def map[U]( func : T => U ) : Tree[U] =
  {
    val tree   = Tree[U](func(root.elem))
    var parent = tree.root
    var fode   = Tree.term[U]
    var node   = root.child
    while( in(node) )
    {
      fode = tree.add( parent, func(node.elem) )
      parent = fode.paren
      node   = node.next()
    }
    tree
  }

  // Needs testing
  def filter( isIn:T => Boolean ): Tree[T] =
  {
    val tree   = Tree[T](root.elem) // For now we do not filter the root elem
    var parent = tree.root
    var fode   = term
    var node   = root.child
    while( in(node) )
    {
      if( isIn(node.elem) ) {
        fode = tree.add( parent, node.elem ) }
      parent = if( in(fode) ) fode.paren else parent
      node   = node.next()
    }
    tree
  }

  def flatMap[U]( func:T => U )        : Tree[U] =
    map( func )

  def withFfilter( pred:T => Boolean ) : Tree[T] =
    filter( pred )

  def toArray[ U >: T : ClassTag ] : Array[U] =
  {
    val array = new Array[U](size)
    var node  = root
    var i     = 0
    while( in(node) ) {
      array(i) = node.elem
      node     = node.next()
      i = i + 1 }
    array
  }

  def toList[U >: T : ClassTag] : List[U] = toArray[U].toList

  // ... breath traversals ...

  def cousin( node:Tode[T] ) : Tode[T] =
  {
    if(   in(node.nextn) )
      return node.nextn

    var ancest = node.paren
    val nlevel = level(node)
    var couson = term

    while( in(ancest) )  // Search for the nextn cousin
    {
      if( !in( ancest.nextn ) )
      {
        ancest = ancest.paren
      }
      else
      {
        ancest = ancest.nextn
        couson = ancest
        var i  = level(ancest)
        while( i < nlevel && in(couson) )
        { couson = couson.child; i = i + 1 }
        if( in(couson) && level(couson) == nlevel )
          return couson
      }
    }
    term
  }

  def cousins( first:Tode[T], visit: Tode[T] => Unit ) : Tode[T] =
  {
    var grandChild : Tode[T] = term
    var child = first
    while( in(child) )
    {
      visit(child)
      if( !in(grandChild) && in(child.child) )
        grandChild = child.child
      child  = cousin(child)
    }
    grandChild
  }

  def breath( top:Tode[T], visit: Tode[T] => Unit ) : Unit =
  {
    visit(top)
    var first = top.child
    while( in(first ) )
      first = cousins( first, visit )
  }

  // ... depth traversals ...

  def prepost( node:Tode[T] )( pre: Tode[T] => Unit )( post: Tode[T] => Unit ) : Unit =
  {
    var child = node.child
    while( in(child) )
    {
      pre( child )
      prepost( child )( pre )( post )
      post( child )
      child = child.nextn
    }
  }


}

object Tree
{
  def term[D]:Tode[D] = new Tode[D]( null.asInstanceOf[Nothing] )
  val maxLevel : Int = 12

  def apply[T](        ) : Tree[T] = new Tree[T]()
  def apply[T]( elem:T ) : Tree[T] = { val tree = Tree[T](); tree.root.elem = elem; tree }

  def test() : Unit =
  {
    val tree = new Tree[String]()
    tree.root.elem = "0"
    var node = tree.term
    node = tree.add("1")
    tree.add( node, "1.1" )
    tree.add( node, "1.2" )
    tree.add( node, "1.3" )
    node = tree.add("2")
    tree.add( node, "2.1" )
    tree.add( node, "2.2" )
    tree.add( node, "2.3" )
    node = tree.add("3")
    tree.add( node, "3.1" )
    tree.add( node, "3.2" )
    tree.add( node, "3.3" )
   // tree.gen


  }
}


