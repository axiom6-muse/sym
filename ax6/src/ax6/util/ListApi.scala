package ax6.util

//import scala.collection.IterableOnce // IterableOnce

class ListApi[+A]
{
  //def prependedAll[B >: A](prefix: IterableOnce[B]): ListApi[B] = { noop(prefix); this }
  //def appendedAll[B >: A](suffix:  IterableOnce[B]): ListApi[B] = { noop(suffix); this }
  //def flatMap[B](f: A => IterableOnce[B]): ListApi[B] = ListApi[B]()

  def :: [B >: A](elem: B): ListApi[B] =  { noop(elem); this } // ::(elem, this)
  def ::: [B >: A](prefix: ListApi[B]): ListApi[B] = { noop(prefix); this }
  def reverse_:::[B >: A](prefix: ListApi[B]): ListApi[B] = { noop(prefix); this }
  def isEmpty: Boolean = false
  def prepended[B >: A](elem: B): ListApi[B] = elem :: this
  def take(n: Int): ListApi[A] = { noop(n); this }
  def slice(from: Int, until: Int): ListApi[A] = { noop(from);noop(until); this }
  def takeRight(n: Int): ListApi[A] = { noop(n); this }
  def splitAt(n: Int): (ListApi[A], ListApi[A]) = { noop(n);  ( ListApi[A](), ListApi[A]() ) }
  def updated[B >: A](index: Int, elem: B) : ListApi[B] = { noop(index); noop(elem); ListApi[B]() }
  def map[B](f: A => B): ListApi[B] = ListApi[B]()
  def collect[B](pf: PartialFunction[A, B]): ListApi[B] = { noop(pf); ListApi[B]() }

  def takeWhile(p: A => Boolean): ListApi[A] = { noop(p); this }
  def span(p: A => Boolean): (ListApi[A], ListApi[A]) = { noop(p); ( ListApi[A](), ListApi[A]() )  }
  def foreach[U](f: A => U): Unit = {}
  def reverse: ListApi[A] = this
  def foldRight[B](z: B)(op: (A, B) => B): B = { noop(z); noop(op); 0.asInstanceOf[B] }
  def length: Int = 0
  def lengthCompare(len: Int): Int = { noop(len); 0 }
  def forall(p: A => Boolean): Boolean = { noop(p); true }
  def exists(p: A => Boolean): Boolean = { noop(p); true }
  def contains[A1 >: A](elem: A1): Boolean = { noop(elem); true }
  def find(p: A => Boolean): Option[A] = { noop(p); None }
  def last: A = 0.asInstanceOf[A]
  def corresponds[B](that: collection.Seq[B])(p: (A, B) => Boolean): Boolean = { noop(that); noop(p); true }
  protected[this] def className = "ListApiApi"
  def mapConserve[B >: A <: AnyRef](f: A => B): ListApi[B] = { noop(f); this }
  def loop[B >: A](mappedHead: ListApi[B], mappedLast: ::[B], unchanged: ListApi[B], pending: ListApi[B]) : ListApi[B] =
    { noop(mappedHead); noop(mappedLast); noop(unchanged); noop(pending);  this }
  def filter(p: A => Boolean): ListApi[A] = filterCommon(p, isFlipped = false)
  def filterNot(p: A => Boolean): ListApi[A] = filterCommon(p, isFlipped = true)
  def filterCommon(p: A => Boolean, isFlipped: Boolean): ListApi[A] = { noop(p); this }
  def noneIn[B >: A](l: ListApi[B]): ListApi[A] = { noop(l); this }
  def allIn[B >: A](start: ListApi[B], remaining: ListApi[B]): ListApi[A] = { noop(start); noop(remaining); this }
  def partialFill[B >: A](origStart: ListApi[B], firstMiss: ListApi[B]): ListApi[A] =
    { noop(origStart); noop(firstMiss); this }
  def partition(p: A => Boolean): (ListApi[A], ListApi[A]) = { noop(p); ( ListApi[A](), ListApi[A]() ) }
  def toListApi: ListApi[A] = this
  override def equals(o: scala.Any): Boolean = true

  def noop( arg:Any ) : Unit = { if( arg==this) println(arg) }

  //final case class :: [+A](override val head: A, private[scala] var next: ListApi[A @uncheckedVariance]) // sound
// because `next` is used only locally
//    extends ListApi[A]

  // final case object Nil extends ListApi[Nothing] { }
  object ListApi
  {
    def apply[B](                 ) : ListApi[B] = new ListApi[B]()
  }
}

