// Roman Riazantsev; Parallel Functional Programming > Workshop 1 > Assignment 1
object Laba_1 {
  // 1 Трикутник Паскаля.
  def pascal(c: Int, r: Int): Int =
    if (c > r) -1
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  // 2 Балансування дужок.
  def balance(chars: List[Char]): Boolean = {
    def balance_f(chars: List[Char], stack: List[Char]): Boolean = {
      if (chars.isEmpty) stack.isEmpty
      else if (chars.head == '(') balance_f(chars.tail, stack :+ chars.head)
      else if (chars.head == ')') stack.nonEmpty && balance_f(chars.tail, stack.tail)
      else balance_f(chars.tail, stack)
    }
    balance_f(chars,List())
  }

  // 3 Розмiнюємо грошi.
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}