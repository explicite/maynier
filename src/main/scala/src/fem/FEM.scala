package src.fem

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.abs

/**
 * Finite element method model
 *
 * @param minimalRadius minimal radius of cylinder [m]
 * @param maximalRadius maximal radius of cylinder [m]
 * @param αAir air convection heat transfer coefficient [W/m2*K]
 * @param t0 begin temperature [°C]
 * @param stops temperature stops(stop temperature[°C], stop time[s])
 * @param c heat factor [J/kg*K]
 * @param ρ0 material density [kg/m3]
 * @param β coefficient of linear expansion [1/K]
 * @param λ thermal conductivity [W/m*K]
 * @param numberOfNodes number of nodes
 *
 * @author Jan Paw
 *         Date: 2/2/14
 */
case class FEM(minimalRadius: Double, maximalRadius: Double, αAir: Double, t0: Double, stops: Seq[(Double, Double)], c: Double, ρ0: Double, β: Double, λ: Double, numberOfNodes: Int) {

  //integration points in the local coordinate system
  val E: Seq[Double] = Seq(-0.5773502692, 0.5773502692)

  //importance of integration
  val W: Seq[Double] = Seq(1.0, 1.0)

  //first shape function
  val N1: Seq[Double] = Seq(0.5 * (1.0 - E(0)), 0.5 * (1.0 - E(0)))

  //second shape function
  val N2: Seq[Double] = Seq(0.5 * (1.0 + E(0)), 0.5 * (1.0 + E(0)))

  val endTime: Double = stops.foldLeft(0.0)((acc, c) => acc + c._2)

  val numberOfElements: Int = numberOfNodes - 1

  val σRadius: Double = (maximalRadius - minimalRadius) / numberOfElements

  val numberOfIterations: Int = ((endTime / ((σRadius * σRadius) / (0.5 * (λ / (c * ρ0))))) + 1).toInt

  val σTime: Double = endTime / numberOfIterations

  val coordinates: Seq[Double] = (0 until numberOfNodes).map(i => i * σRadius)

  var nodeTemperature: ArrayBuffer[Double] = ArrayBuffer.fill(numberOfNodes)(t0)

  var temperatureStep: (Double, Double) = stops(0)

  var airTemp: Double = temperatureStep._1

  //lower diagonal stiffness matrix
  var aC: ArrayBuffer[Double] = ArrayBuffer.fill(numberOfNodes)(0.0)

  //diagonal stiffness matrix
  var aD: ArrayBuffer[Double] = ArrayBuffer.fill(numberOfNodes)(0.0)

  //upper diagonal stiffness matrix
  var aE: ArrayBuffer[Double] = ArrayBuffer.fill(numberOfNodes)(0.0)

  //load vector
  var aB: ArrayBuffer[Double] = ArrayBuffer.fill(numberOfNodes)(0.0)

  /**
   * Resolve simulation
   *
   * @param ω iteration factor [1,2] When ω, then become Gauss-Seidel method
   * @param ε accuracy rate
   *
   * @return vector with temperature in nodes
   */
  def apply(ω: Double, ε: Double): mutable.Buffer[Seq[Double]] = {

    var σTemperature: Double = 0.0
    var σMaxTemperature: Double = 0.0
    var time: Double = 0.0

    var data: mutable.Buffer[Seq[Double]] = mutable.Buffer.empty[Seq[Double]]

    for (iteration <- 0 until numberOfIterations) {
      aC = ArrayBuffer.fill(numberOfNodes)(0.0)
      aD = ArrayBuffer.fill(numberOfNodes)(0.0)
      aE = ArrayBuffer.fill(numberOfNodes)(0.0)
      aB = ArrayBuffer.fill(numberOfNodes)(0.0)

      if (temperatureStep._2 <= time)
        temperatureStep = stops.find(_._2 >= time).getOrElse(stops.last)

      airTemp = temperatureStep._1

      for (element <- 0 until numberOfElements) {
        val ρ: Double = ρ0 + ρ0 * (3 * β * (t0 - nodeTemperature(element)))

        val r: Seq[Double] = Seq(coordinates(element), coordinates(element + 1))
        val temp: Seq[Double] = Seq(nodeTemperature(element), nodeTemperature(element + 1))

        val σR: Double = r(1) - r(0)
        var α: Double = 0.0

        if (element == numberOfElements - 1) α = αAir

        val H: ArrayBuffer[Double] = ArrayBuffer.fill(4)(0.0)

        val P: ArrayBuffer[Double] = ArrayBuffer.fill(2)(0.0)

        //First point
        var τRadius: Double = N1(0) * r(0) + N2(0) * r(1)
        var τTemperature: Double = N1(0) * temp(0) + N2(0) * temp(1)

        H(0) += λ * τRadius * W(0) / σR + c * ρ * σR * τRadius * W(0) * N1(0) * N1(0) / σTime
        H(1) += -λ * τRadius * W(0) / σR + c * ρ * σR * τRadius * W(0) * N1(0) * N2(0) / σTime
        H(2) = H(1)
        H(3) += λ * τRadius * W(0) / σR + c * ρ * σR * τRadius * W(0) * N2(0) * N2(0) / σTime + 2 * α * maximalRadius

        P(0) += c * ρ * σR * τTemperature * τRadius * W(0) * N1(0) / σTime
        P(1) += c * ρ * σR * τTemperature * τRadius * W(0) * N2(0) / σTime + 2 * α * maximalRadius * airTemp

        //Second point
        τRadius = N1(1) * r(0) + N2(1) * r(1)
        τTemperature = N1(1) * temp(0) + N2(1) * temp(1)

        H(0) += λ * τRadius * W(1) / σR + c * ρ * σR * τRadius * W(1) * N1(1) * N1(1) / σTime
        H(1) += -λ * τRadius * W(1) / σR + c * ρ * σR * τRadius * W(1) * N1(1) * N2(1) / σTime
        H(2) = H(1)
        H(3) += λ * τRadius * W(1) / σR + c * ρ * σR * τRadius * W(1) * N2(1) * N2(1) / σTime + 2 * α * maximalRadius

        P(0) += c * ρ * σR * τTemperature * τRadius * W(1) * N1(1) / σTime
        P(1) += c * ρ * σR * τTemperature * τRadius * W(1) * N2(1) / σTime + 2 * α * maximalRadius * airTemp

        aD(element) += H(0)
        aD(element + 1) += H(3)
        aE(element) += H(1)
        aC(element + 1) += H(2)
        aB(element) += P(0)
        aB(element + 1) += P(1)
      }

      val stiffnessMatrix: ArrayBuffer[Double] = ArrayBuffer.fill(numberOfNodes * numberOfNodes)(0.0)
      val loadsVector: ArrayBuffer[Double] = ArrayBuffer.fill(numberOfNodes)(0.0)

      for (i <- 0 until numberOfNodes) {
        stiffnessMatrix(i + i * numberOfNodes) = aD(i)
        loadsVector(i) = aB(i)
      }

      for (i <- 0 until (numberOfNodes - 1)) {
        stiffnessMatrix(i + 1 + (i * numberOfNodes)) = aC(i + 1)
        stiffnessMatrix(i + ((i + 1) * numberOfNodes)) = aE(i)
      }

      nodeTemperature = SOR(stiffnessMatrix, loadsVector)(ω, ε)

      σTemperature = abs(nodeTemperature(0) - nodeTemperature.last)
      if (σTemperature > σMaxTemperature)
        σMaxTemperature = σTemperature

      time += σTime

      data ++= nodeTemperature :: Nil
    }

    data
  }
}
