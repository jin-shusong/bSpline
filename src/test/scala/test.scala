import breeze.linalg.{DenseVector, sum}
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.distribution.UniformRealDistribution
import breeze.plot._

/**
  * Created by jinss on 7/25/17.
  */
object test extends App {

  val nf = new NormalDistribution(0.0, 0.2)
  val uf = new UniformRealDistribution(-1.0, 1.0)
  val epsilon = for (i <- 0 until 400) yield nf.sample()
  val x1 = for (i <- 0 until 400) yield uf.sample()
  val x2 = for (i <- 0 until 400) yield uf.sample()
  val y = for (i <- 0 until 400) yield x1(i) * x2(i) + epsilon(i)


  val x1V = x1.toArray
  val x2V = x2.toArray
  val x1and2 = x1V ++ x2V
  val xMatrix = new breeze.linalg.DenseMatrix[Double](400, 2, x1and2)

  val yArray = y.toArray
  val ymean = yArray.sum / yArray.length.toDouble
  val yCentered = yArray.map(x => x - ymean)
  val yV = new breeze.linalg.DenseVector[Double](yCentered)
  val wTmp = for (i <- 0 to 1) yield uf.sample() // Step 2
  val w = new breeze.linalg.DenseVector[Double](wTmp.toArray)
  var tse1 = sum(yV *:* yV)
  var tse0 = 0.0
  var j = 0
  while (math.abs(tse1 - tse0) > 1e-6 && j < 400) {
    val v = xMatrix * w
    // Step 3
    val vMin = breeze.linalg.min(v)
    val vMax = breeze.linalg.max(v)
    val vBreaks = bSpline.findKnotsWithData(10, v, 3)
    val vSplineMatrix = bSpline.findBSplineMatrix(v, 3, vBreaks)
    val tmpMatrix1 = vSplineMatrix.t * vSplineMatrix
    val tmpVector = vSplineMatrix.t * yV
    val betaHatForG = tmpMatrix1 \ tmpVector
    val yhat = vSplineMatrix * betaHatForG
    val zeta = yV - yhat
    tse0 = tse1
    tse1 = sum(zeta *:* zeta)
    // step 4
    val theta = v

    val xi = theta.map { x =>
      val res2 = bSpline.bSplineDerivEval(x, 3, vBreaks)
      res2(::, 1).t * betaHatForG
    }
    val newy = (0 until 400).map { i =>
      theta(i) + zeta(i) / xi(i)
    }
    val newyV = new breeze.linalg.DenseVector[Double](newy.toArray)
    val xi2 = xi.map(x => x * x)
    val xiV2 = new breeze.linalg.DenseVector[Double](xi2.toArray)
    val wMatrix = breeze.linalg.diag(xiV2)
    val tmp2 = xMatrix.t * wMatrix
    val tmp3 = tmp2 * xMatrix
    val tmp4 = tmp2 * newyV
    val wVector = tmp3 \ tmp4
    w(0) = wVector(0)
    w(1) = wVector(1)
    j += 1
  }

  val k0 = sum(w *:* w)
  val w0 = w / math.sqrt(k0)
  println(w0)
  val v0 = xMatrix * w0
  val vMin = breeze.linalg.min(v0)
  val vMax = breeze.linalg.max(v0)
  val v0Breaks = bSpline.findKnotsWithData(10, v0, 3)
  val v0SplineMatrix = bSpline.findBSplineMatrix(v0, 3, v0Breaks)
  val tmp0Matrix1 = v0SplineMatrix.t * v0SplineMatrix
  val tmp0Vector = v0SplineMatrix.t * yV
  val beta0HatForG = tmp0Matrix1 \ tmp0Vector
  val y0hat = v0SplineMatrix * beta0HatForG
  val newy1 = yV - y0hat

  val w1Tmp = for (i <- 0 to 1) yield uf.sample() // Step 2
  val w1 = new breeze.linalg.DenseVector[Double](wTmp.toArray)
  tse1 = sum(newy1 *:* newy1)
  tse0 = 0.0
  j = 0
  while (math.abs(tse1 - tse0) > 1e-6 && j < 400) {
    val v = xMatrix * w1
    // Step 3
    val vMin = breeze.linalg.min(v)
    val vMax = breeze.linalg.max(v)
    val vBreaks = bSpline.findKnotsWithData(10, v, 3)
    val vSplineMatrix = bSpline.findBSplineMatrix(v, 3, vBreaks)
    val tmpMatrix1 = vSplineMatrix.t * vSplineMatrix
    val tmpVector = vSplineMatrix.t * newy1
    val betaHatForG = tmpMatrix1 \ tmpVector
    val yhat = vSplineMatrix * betaHatForG
    val zeta = newy1 - yhat
    tse0 = tse1
    tse1 = sum(zeta *:* zeta)
    // step 4
    val theta = v

    val xi = theta.map { x =>
      val res2 = bSpline.bSplineDerivEval(x, 3, vBreaks)
      res2(::, 1).t * betaHatForG
    }
    val newy = (0 until 400).map { i =>
      theta(i) + zeta(i) / xi(i)
    }
    val newyV = new breeze.linalg.DenseVector[Double](newy.toArray)
    val xi2 = xi.map(x => x * x)
    val xiV2 = new breeze.linalg.DenseVector[Double](xi2.toArray)
    val wMatrix = breeze.linalg.diag(xiV2)
    val tmp2 = xMatrix.t * wMatrix
    val tmp3 = tmp2 * xMatrix
    val tmp4 = tmp2 * newyV
    val wVector = tmp3 \ tmp4
    w1(0) = wVector(0)
    w1(1) = wVector(1)
    j += 1
  }
  val k1 = sum(w1 *:* w1)
  val w1True = w1 / math.sqrt(k1)
  println(w1True)
  val v1 = xMatrix * w1True
  val v1Min = breeze.linalg.min(v1)
  val v1Max = breeze.linalg.max(v1)
  val v1Breaks = bSpline.findKnotsWithData(10, v1, 3)
  val v1SplineMatrix = bSpline.findBSplineMatrix(v1, 3, v1Breaks)
  val tmp1Matrix1 = v1SplineMatrix.t * v1SplineMatrix
  val tmp1Vector = v1SplineMatrix.t * newy1
  val beta1HatForG = tmp1Matrix1 \ tmp1Vector
  val y1hat = v1SplineMatrix * beta1HatForG


  val f = Figure()
  val p = f.subplot(2, 1, 0)
  p += plot(v0, y0hat, '.')
  val p1 = f.subplot(2, 1, 1)
  p1 += plot(v1, y1hat, '.')
}