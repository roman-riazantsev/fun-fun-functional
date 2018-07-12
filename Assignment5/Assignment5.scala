package finita

import helpers._

import scala.io.Source

object Assignment5 {
  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile("file.txt")
    val charSeq = (for (char <- bufferedSource) yield char).toIndexedSeq
    bufferedSource.close()

    val foldedText = foldMapPar(charSeq, 0, charSeq.length, monoidCounter)(charToCounter)(1000)
    print(foldedText._2)
  }

  type counter = (String, Int, String)

  val monoidCounter = new Monoid[counter] {
    def op(x: counter, y: counter): counter = {
      (x._3, y._1) match {
        case ("char","symbol") => (x._1, x._2 + y._2 + 1, y._3)
        case (_,"zero") => x
        case ("zero",_) => y
        case  _ => (x._1, x._2 + y._2, y._3)
      }
    }

    def zero: counter = ("zero", 0, "zero")
  }

  def charToCounter(char: Char): counter = {
    if (char.isLetter) {
      ("char", 0, "char")
    } else {
      ("symbol", 0, "symbol")
    }
  }
}
