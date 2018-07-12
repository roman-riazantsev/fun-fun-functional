// Roman Riazantsev; Parallel Functional Programming > Workshop 2 > Assignment 2
object Laba_2 {
  // 1 Загальнi мiркування.
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean =
    s(elem)

  // 2 Основнi операцiї на множинах.
  def singletonSet(elem: Int): Set =
    (x: Int) => x == elem

  def union(s: Set, t: Set): Set =
    (x: Int) => contains(s, x) || contains(t, x)

  def intersect(s: Set, t: Set): Set =
    (x: Int) => contains(s, x) && contains(t, x)

  def diff(s: Set, t: Set): Set =
    (x: Int) => contains(s, x) && !contains(t, x)

  def filter(s: Set, p: Int => Boolean): Set =
    (x: Int) => contains(s, x) && p(x)

  // 3 Запити i трансформацiї множин.
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }

    iter(-1000)
  }

  def exists(s: Set, p: Int => Boolean): Boolean =
    !forall(s, (x: Int) => !p(x))

  def map(s: Set, f: Int => Int): Set =
    (y: Int) => exists(s, (x: Int) => f(x) == y)
}
