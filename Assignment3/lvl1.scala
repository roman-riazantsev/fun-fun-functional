final class Machine {
  def reduce(expr: Expr, env: Map[String, Any]): Expr = {
    println(show(expr))

    if (expr.isReducible)
      reduce(reductionStep(expr, env), env)
    else
      expr
  }

  def reductionStep(expr: Expr, env: Map[String, Any]): Expr = {
    expr match {
      case Prod(e1, e2) =>
        if (e1.isReducible)
          Prod(reductionStep(e1, env), e2)
        else if (e2.isReducible)
          Prod(e1, reductionStep(e2, env))
        else
          Number(Prod(e1, e2).eval)

      case Sum(e1, e2) =>
        if (e1.isReducible)
          Sum(reductionStep(e1, env), e2)
        else if (e2.isReducible)
          Sum(e1, reductionStep(e2, env))
        else
          Number(Sum(e1, e2).eval)

      case If(condition, e1, e2) =>
        if (condition.isReducible)
          If(reductionStep(condition, env), e1, e2)
        else if (condition.eval != 0) e1 else e2

      case Logic(e1, e2) =>
        if (e1.isReducible)
          Logic(reductionStep(e1, env), e2)
        else if (e2.isReducible)
          Logic(e1, reductionStep(e2, env))
        else
          Number(Logic(e1, e2).eval)

      case Bool(x) =>
        x match {
          case e1: Expr if e1.isReducible =>
            Bool(reductionStep(e1, env))
          case n: Number => n
          case b: Boolean => Number(Bool(b).eval)
        }

      case Var(n) =>
        if (env.contains(n))
          env(n) match {
            case b: Boolean => Number(Bool(b).eval)
            case i: Int => Number(i)
          }
        else{
          Var(n)
          throw new Exception("No such Variable")
        }
    }
  }

  def show(expr: Expr): String = {
    def paren(expr: Expr) = {
      expr match {
        case Sum(_, _) => "(" + show(expr) + ")"
        case _ => show(expr)
      }
    }

    expr match {
      case Number(n) => n.toString
      case Var(s) => s.toString
      case Sum(e1, e2) => show(e1) + " + " + show(e2)
      case Prod(e1, e2) => paren(e1) + " * " + paren(e2)
      case Bool(s) => s.toString
      case Logic(e1, e2) => show(e1) + " < " + show(e2)
      case If(condition, e1, e2) => "if(" + show(condition) + "){" + show(e1) + "}else{" + show(e2) + "}"
    }
  }
}

trait Expr {
  def eval: Int = {
    this.checkArguments()
    this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
      case Prod(e1, e2) => e1.eval * e2.eval
      case Logic(e1, e2) => if (e1.eval < e2.eval) 1 else 0
      case Bool(e1) =>
        e1 match {
          case b: Boolean => if (b) 1 else 0
          case n: Number => if (n.eval == 1) 1 else 0
        }
      case _ => 0
    }
  }

  def isReducible: Boolean = {
    this match {
      case Sum(_, _) | Prod(_, _) | Var(_) | If(_, _, _) | Logic(_, _) | Bool(_) => true
      case _ => false
    }
  }

  def checkArguments(): Unit = {
    this match {
      case Number(_) | Bool(_) =>
      case Sum(e1, e2) if e1.isInstanceOf[Number] && e2.isInstanceOf[Number] =>
      case Prod(e1, e2) if e1.isInstanceOf[Number] && e2.isInstanceOf[Number] =>
      case Logic(e1, e2) if e1.isInstanceOf[Number] && e2.isInstanceOf[Number] =>
      case If(condition, _, _) if condition.isInstanceOf[Logic] || condition.isInstanceOf[Bool] || condition.isInstanceOf[Number] =>
      case _ => throw new IllegalArgumentException("IllegalArgumentException")
    }
  }
}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(x: String) extends Expr

case class Bool(x: Any) extends Expr

case class Logic(e1: Expr, e2: Expr) extends Expr

case class If(x: Expr, e1: Expr, e2: Expr) extends Expr

