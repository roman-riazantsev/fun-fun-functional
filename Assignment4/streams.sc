import Stream.empty

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail:() => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](args: A*): Stream[A] =
    if(args.isEmpty) empty
    else cons(args.head, apply(args.tail: _*))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))
}

sealed trait Stream[+A] {
  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }



  def toList: List[A] = {
    def move(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => move(t(), h() :: acc)
      case _ => acc
    }

    move(this, List()).reverse
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => Stream.cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 1 => t()
    case _ => this
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A,=>B)=>B):B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def anotherExist(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) =>
      if (p(a)) Stream.cons(a,b)
      else b
    )
}


val stream = Stream(1,2,3,4,5)

stream.take(3).toList

stream.drop(3).toList

stream.filter(_!=2).toList


val more = Stream.from(100)

def fibs: Stream[Int] = {
  def next(f0: Int, f1: Int): Stream[Int] =
    Stream.cons(f0, next(f1, f0 + f1))
  next(0, 1)
}

def primes: Stream[Int] = {
  def check(k: Int): Boolean = Stream.from(2).take(k-2).forAll(k % _ !=0)

  def getNext(current: Int): Stream[Int] =
    if (check(current)) {
      Stream.cons(current, getNext(current+1))
    } else {
      getNext(current+1)
    }
  getNext(2)

}


more.take(5).toList
primes.take(10).toList

Stream.from(2).take(6-2).toList

Stream.from(2).take(1).forAll(3 % _ !=0)




