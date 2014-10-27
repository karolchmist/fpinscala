package fpinscala.laziness

trait Stream[+A] {
  def toList :List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    if(n > 0) {
      this match {
        case Empty => Empty
        case Cons(h,_) if n == 0 => Stream.cons(h(), Empty)
        case Cons(h,t) => Stream.cons(h(), t().take(n-1))
      }
    } else {
      Empty
    }


  def drop(n: Int): Stream[A] =
    if (n <= 0) {
      this
    } else {
      this match {
        case Empty => Empty
        case Cons(h,t) => t().drop(n-1)
      }
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty:Stream[A])((a,b) => if (p(a)) Stream.cons(a, b) else Empty)

  def headOption : Option[A] =
    foldRight(Option.empty:Option[A])((a:A,b) => Option(a) )

  def map[B](f: A => B) : Stream[B] = foldRight(Empty:Stream[B])((a,b) => Stream.cons(f(a), b) )

  def filter(p: A => Boolean) : Stream[A] = foldRight(Empty:Stream[A])((a,b) => if(p(a)) Stream.cons(a, b) else b )

  def append[B>:A](s:Stream[B]): Stream[B] =
    foldRight(s)((a,b) => Stream.cons(a,b))

  def flatMap[B](f: A => Stream[B]) : Stream[B] =
      foldRight(Empty:Stream[B])((a,b) => f(a).append(b) )

  def startsWith[A](s:Stream[A]) : Boolean = Stream.unfold((this,s)) {
    case (Empty, Empty) => None
    case (Empty, _) => Some(false, (Empty, Empty))
    case (_, Empty) => None
    case (Cons(h1,t1), Cons(h2,t2)) if h1() == h2() => Some(true, (t1(), t2()))
    case _ => Some(false, (Empty, Empty))
  }.forAll(r => r)

  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this){
    case Empty => None
    case Cons(h, t) => Some((f(h()),t()))
  }

  def takeUnfold(n:Int) : Stream[A] = Stream.unfold((n, this)) {
    case (_, Empty) => None
    case (m, _) if m <= 0 => None
    case (m, Cons(h,t)) => Some(h(),(m-1,t()))
  }

  def takeWhileUnfold(p: A => Boolean) : Stream[A] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithUnfold[B](sb: Stream[B]) : Stream[(A,B)] = Stream.unfold((this,sb)) {
    case (Empty,_) => None
    case (_,Empty) => None
    case (Cons(h1,t1),Cons(h2,t2)) => Some((h1(),h2()), (t1(),t2()))
  }
  def zipAllUnfold[B](sb: Stream[B]) : Stream[(Option[A], Option[B])] = Stream.unfold((this,sb)){
    case (Empty,Empty) => None
    case (Empty,Cons(h,t)) => {
      // without implicit types Scala does not compile
      val a: (Option[A], Option[B]) = (None, Some(h()))
      Some(a, (Empty,t()))
    }
    case (Cons(h,t),Empty) => {
      val a: (Option[A], Option[B]) = (Some(h()), None)
      val s: (Stream[A], Stream[B]) = (t(), Empty)
      Some(a, s)
    }
    case (Cons(h1,t1),Cons(h2,t2)) => Some(
      (Some(h1()),Some(h2())),
      (t1(),t2())
    )
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))


  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant(n:Int): Stream[Int] = Stream.cons(n, constant(n))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  val fibo : Stream[Int] = {
    def go(n0:Int, n1:Int) : Stream[Int] =
      cons(n0, go(n1, n0+n1))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(n:S) : Stream[A] = {
      f(n) match {
        case Some((a,s)) => cons(a, go(s))
        case None => empty
      }
    }
    go(z)
  }

  def fiboUnfold = unfold((0,1)){ case (n0,n1) => Some(n0,(n1,n0+n1)) }

  def fromUnfold(n:Int) = unfold(n){ s => Some(s, s+1) }
  def constantUnfold(n:Int) = unfold(n){ s => Some(s, s) }
  val onesUnfold = constantUnfold(1)
}