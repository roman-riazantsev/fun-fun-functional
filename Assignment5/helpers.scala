package finita


object helpers {
  trait Monoid[A] {
    def op(x: A, y: A): A
    def zero: A
  }


  import java.util.concurrent._

  // The fork/join framework uses a work-stealing algorithm
  // http://supertech.csail.mit.edu/papers/steal.pdf
  val forkJoinPool = new ForkJoinPool

  def task[T](computation: => T): RecursiveTask[T] = {
    val t = new RecursiveTask[T] {
      def compute = computation
    }

    Thread.currentThread match {
      case wt: ForkJoinWorkerThread =>
        t.fork() // schedule for execution
      case _ =>
        forkJoinPool.execute(t)
    }

    t
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA

    (left, right.join())
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task {
      taskA
    }
    val tb = task {
      taskB
    }
    val tc = task {
      taskC
    }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

  def foldMapPar[A, B](xs: IndexedSeq[A], from: Int, to: Int, m: Monoid[B])
                      (f: A => B)
                      (implicit theresholdSize: Int): B =
    if (to - from <= theresholdSize)
      foldMapSegment(xs, from, to, m)(f)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldMapPar(xs, from, middle, m)(f)
        (theresholdSize),
        foldMapPar(xs, middle, to, m)(f)
        (theresholdSize))

      m.op(l, r)
    }

  def foldMapSegment[A, B](xs: IndexedSeq[A], from: Int, to: Int, m: Monoid[B])
                          (f: A => B): B = {
    var res = f(xs(from))
    var index = from + 1
    while (index < to) {
      res = m.op(res, f(xs(index)))
      index = index + 1
    }
    res
  }
}
