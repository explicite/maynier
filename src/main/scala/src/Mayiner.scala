import scala.math.{log, sqrt, exp}

/**
 * Calculation of the structure parameters for low alloy steel.
 * Hardening and tempering according to the cooling rate.
 *
 * @param C coal rate
 * @param Mn manganese rate
 * @param Ni nickel rate
 * @param Cr chromium rate
 * @param Mo molybdenum rate
 * @param S sulfur rate
 * @param V vanadium rate
 * @param T temperature [K]
 * @param t time [s]
 * @param v speed [K/s]
 *
 * @author Jan Paw
 *         Date: 1/22/14
 */
case class Mayiner(C: Double, Mn: Double, Ni: Double, Cr: Double, Mo: Double, S: Double, V: Double, T: Double, t: Double, v: Double) {

  //Activation energy for low alloy steel [J/mol]
  val H: Double = 418000

  //Gas constant [J/mol*K]
  val R: Double = 8.3144621

  def Pa: Double = 1 / ((1 / T) - ((R / H) * log(t)))

  def m100: Double = T / exp(9.81 - ((4.62 * C) + (1.05 * Mn) + (0.54 * Ni) + (0.5 * Cr) + (0.66 * Mo) + (0.00183 * Pa)))

  def m90: Double = T / exp(8.76 - ((4.04 * C) + (0.96 * Mn) + (0.49 * Ni) + (0.58 * Cr) + (0.97 * Mo) + (0.001 * Pa)))

  def m50: Double = T / exp(8.50 - ((4.12 * C) + (0.86 * Mn) + (0.57 * Ni) + (0.41 * Cr) + (0.94 * Mo) + (0.0012 * Pa)))

  def b100: Double = T / exp(10.17 - ((3.8 * C) + (1.07 * Mn) + (0.7 * Ni) + (0.57 * Cr) + (1.58 * Mo) + (0.0032 * Pa)))

  def b90: Double = T / exp(10.55 - ((3.65 * C) + (1.08 * Mn) + (0.77 * Ni) + (0.61 * Cr) + (1.49 * Mo) + (0.004 * Pa)))

  def b50: Double = T / exp(8.74 - ((2.23 * C) + (0.86 * Mn) + (0.56 * Ni) + (0.59 * Cr) + (1.6 * Mo) + (0.0032 * Pa)))

  def fp90: Double = T / exp(7.51 - ((1.38 * C) + (0.35 * Mn) + (0.93 * Ni) + (0.11 * Cr) + (2.31 * Mo) + (0.0033 * Pa)))

  def fp50: Double = T / exp(6.36 - ((0.43 * C) + (0.49 * Mn) + (0.78 * Ni) + (0.27 * Cr) + (0.38 * Mo) + (2 * sqrt(Mo)) + (0.0019 * Pa)))
}
