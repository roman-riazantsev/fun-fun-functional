package assignment

import nine.parallel.parallel
import org.scalameter.{Key, Warmer, config}

import scala.util.Random

object MonteCarloIntegration {
  val totalNumberOfPoints = 100000
  val startPoint = -1
  val endPoint = 0.5

  def polynomialFunction(x: Double): Double =
    -Math.pow(x, 4) + Math.pow(x, 2) + 1

  def monteCarloIntegration(totalNumberOfPoints: Int, startPoint: Double, endPoint: Double): Double = {
    val rndX = new Random

    def calculateSumOfAreas(sumOfAreas: Double, pointsGenerated: Int): Double =
      if (pointsGenerated >= totalNumberOfPoints)
        sumOfAreas
      else {
        val x = startPoint + (endPoint - startPoint) * rndX.nextDouble
        val y = polynomialFunction(x)
        val interval = endPoint - startPoint
        val area = y * interval
        calculateSumOfAreas(sumOfAreas + area, pointsGenerated + 1)
      }

    calculateSumOfAreas(0, 0) / totalNumberOfPoints
  }

  def twoThreadsMonteCarloIntegration(totalNumberOfPoints: Int, startPoint: Double, endPoint: Double): Double = {
    val splitPoint = (startPoint + endPoint) / 2
    val (integral1, integral2) = parallel(monteCarloIntegration(totalNumberOfPoints / 2, startPoint, splitPoint),
                                          monteCarloIntegration(totalNumberOfPoints / 2, splitPoint, endPoint))
    integral1 + integral2
  }

  def fourThreadsMonteCarloIntegration(totalNumberOfPoints: Int, startPoint: Double, endPoint: Double): Double = {
    val splitPoint = (startPoint + endPoint) / 2
    val (integral1, integral2) = parallel(twoThreadsMonteCarloIntegration(totalNumberOfPoints / 2, startPoint, splitPoint),
      twoThreadsMonteCarloIntegration(totalNumberOfPoints / 2, splitPoint, endPoint))
    integral1 + integral2
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer new Warmer.Default

    val seqtime = standardConfig.measure {
      monteCarloIntegration(totalNumberOfPoints, startPoint, endPoint)
    }

    val partime = standardConfig.measure {
      fourThreadsMonteCarloIntegration(totalNumberOfPoints, startPoint, endPoint)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel tine $partime ms")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}
