import scala.math.{log, log10, sqrt, pow}

/**
 * Calculation of the structure parameters for low alloy steel.
 * Quenching and tempering according to the cooling rate.
 *
 * @param C coal rate
 * @param Mn manganese rate
 * @param Ni nickel rate
 * @param Cr chromium rate
 * @param Mo molybdenum rate
 * @param Si silicon rate
 * @param V vanadium rate
 * @param aT aust. temperature [C]
 * @param at aust. time [s]
 * @param tT tempering temperature [C]
 * @param tt tempering time [s]
 *
 * @author Jan Paw
 *         Date: 1/22/14
 */
case class Mayiner(C: Double, Mn: Double, Ni: Double, Cr: Double, Mo: Double, Si: Double, V: Double, aT: Double, at: Double, tT: Double, tt: Double) {

  //Activation energy for low alloy steel [J/mol]
  val H: Double = 418000

  //Gas constant [J/mol*K]
  val R: Double = 8.3144621

  /**
   * Transformation parameter
   * @param T temperature
   * @param t time
   * @return transformation factor
   */
  def Pa(T: Double, t: Double): Double = 1 / ((1 / T) - ((R / H) * log(t / 3600)))

  val transitionPoints: Seq[Double] = {

    val m100: Double = pow(10, 9.81 - ((4.62 * C) + (1.05 * Mn) + (0.54 * Ni) + (0.5 * Cr) + (0.66 * Mo) + (0.00183 * Pa(aT, at))))

    val m90: Double = pow(10, 8.76 - ((4.04 * C) + (0.96 * Mn) + (0.49 * Ni) + (0.58 * Cr) + (0.97 * Mo) + (0.001 * Pa(aT, at))))

    val m50: Double = pow(10, 8.50 - ((4.12 * C) + (0.86 * Mn) + (0.57 * Ni) + (0.41 * Cr) + (0.94 * Mo) + (0.0012 * Pa(aT, at))))


    val b100: Double = pow(10, 10.17 - ((3.8 * C) + (1.07 * Mn) + (0.7 * Ni) + (0.57 * Cr) + (1.58 * Mo) + (0.0032 * Pa(aT, at))))

    val b90: Double = pow(10, 10.55 - ((3.65 * C) + (1.08 * Mn) + (0.77 * Ni) + (0.61 * Cr) + (1.49 * Mo) + (0.004 * Pa(aT, at))))

    val b50: Double = pow(10, 8.74 - ((2.23 * C) + (0.86 * Mn) + (0.56 * Ni) + (0.59 * Cr) + (1.6 * Mo) + (0.0032 * Pa(aT, at))))


    val fp90: Double = pow(10, 7.51 - ((1.38 * C) + (0.35 * Mn) + (0.93 * Ni) + (0.11 * Cr) + (2.31 * Mo) + (0.0033 * Pa(aT, at))))

    val fp50: Double = pow(10, 6.36 - ((0.43 * C) + (0.49 * Mn) + (0.78 * Ni) + (0.27 * Cr) + (0.38 * Mo) + (2 * sqrt(Mo)) + (0.0019 * Pa(aT, at))))

    Seq(m100, m90, m50, b100, b90, b50, fp90, fp50)
  }

  /**
   * @param Vr quenching speed [C/h]
   * @return martensite hardness after quenching
   */
  def martensiteQuenching(Vr: Double): Double = 127 + (949 * C) + (27 * Si) + (11 * Mn) + (8 * Ni) + (16 * Cr) + (21 * log10(Vr))

  /**
   * @param Vr quenching speed [C/h]
   * @return bainite hardness after quenching
   */
  def bainiteQuenching(Vr: Double): Double = -323 + (158 * C) + (330 * Si) + (153 * Mn) + (65 * Ni) + (144 * Cr) + (191 * Mo) + (log10(Vr) * (89 + (53 * C) - (55 * Si) - (22 * Mn) - (10 * Ni) - (20 * Cr) - (33 * Mo)))

  /**
   * @param Vr quenching speed [C/h]
   * @return perlite-ferrite hardness after quenching
   */
  def perliteFerriteQuenching(Vr: Double): Double = 42 + (223 * C) + (53 * Si) + (30 * Mn) + (12.6 * Ni) + (7 * Cr) + (19 * Mo) + (log10(Vr) * (10 - (19 * Si) + (4 * Ni) + (8 * Cr) + (130 * V)))

  /**
   * <p>Alloy hardness after quenching calculated from formula:</p>
   * HV += component hardness * component rate
   *
   * @param Vr quenching speed [C/h]
   * @return alloy hardness after quenching
   */
  def quenchingHardness(Vr: Double): Double = {
    val structure = findStructure(Vr)
    (martensiteQuenching(Vr) * structure._1) + (bainiteQuenching(Vr) * structure._2) + (perliteFerriteQuenching(Vr) * structure._3)
  }

  /**
   * @return martensite hardness after tempering
   */
  def martensiteTempering: Double = -74 - (434 * C) - (368 * Si) + (15 * Mn) + (37 * Ni) + (17 * Cr) - (335 * Mo) - (2235 * V) +
    ((1000 / Pa(tT + 273, tt)) * (260 + (616 * C) + (321 * Si) + (21 * Mn) - (35 * Ni) - (11 * Cr) + (352 * Mo) + (2345 * V)))

  /**
   * @return bainite hardness after tempering
   */
  def bainiteTempering: Double = 262 + (163 * C) - (349 * Si) - (64 * Mn) - (6 * Ni) - (186 * Cr) - (458 * Mo) - (867 * V) +
    ((1000 / Pa(tT + 273, tt)) * (-149 + (43 * C) + (336 * Si) + (79 * Mn) + (16 * Ni) + (196 * Cr) + (498 * Mo) + (1094 * V)))

  /**
   * <p>Alloy hardness after tempering calculated from formula:</p>
   * HV += component hardness + component rate
   *
   * @param Vr quenching speed [C/h]
   * @return alloy hardness after tempering
   */
  def temperingHardness(Vr: Double): Double = {
    val structure = findStructure(Vr)
    (martensiteTempering * structure._1) + (bainiteTempering * structure._2) + (perliteFerriteQuenching(Vr) * structure._3)
  }

  /**
   * Finding structure for cooling setpoint speed
   *
   * @param Vr quenching speed [C/h]
   * @return (martensit, bainite, perlite-ferrite)
   */
  def findStructure(Vr: Double): (Double, Double, Double) = {
    def line(p1: (Double, Double), p2: (Double, Double), x: Double): Double = {
      (p2._2 - p1._2) / (p2._1 - p1._1) * (x - p1._1) + p1._2
    }

    def find(v: Double, i: Int): Int = {
      if (transitionPoints(i) > v)
        i
      else {
        if (i > 0)
          find(v, i - 1)
        else
          i
      }
    }

    find(Vr, transitionPoints.length - 1) match {
      case 0 =>
        (1, 0, 0)
      case 1 =>
        (line((transitionPoints(0), 1.0), (transitionPoints(1), 0.9), Vr),
          line((transitionPoints(0), 0), (transitionPoints(1), 0.1), Vr), 0)
      case 2 =>
        (line((transitionPoints(1), 0.9), (transitionPoints(2), 0.5), Vr),
          line((transitionPoints(1), 0.1), (transitionPoints(2), 0.5), Vr), 0)
      case 3 =>
        (line((transitionPoints(2), 0.5), (transitionPoints(3), 0), Vr),
          line((transitionPoints(2), 0.5), (transitionPoints(3), 1), Vr), 0)
      case 4 =>
        (0,
          line((transitionPoints(3), 1), (transitionPoints(4), 0.9), Vr),
          line((transitionPoints(3), 0), (transitionPoints(4), 0.1), Vr))
      case 5 =>
        (0,
          line((transitionPoints(4), 0.9), (transitionPoints(5), 0.5), Vr),
          line((transitionPoints(4), 0.1), (transitionPoints(5), 0.5), Vr))
      case 6 =>
        (0,
          line((transitionPoints(5), 0.5), (transitionPoints(6), 0.1), Vr),
          line((transitionPoints(5), 0.5), (transitionPoints(6), 0.9), Vr))
      case 7 =>
        (0,
          line((transitionPoints(6), 0.1), (transitionPoints(7), 0), Vr),
          line((transitionPoints(6), 0.9), (transitionPoints(7), 1), Vr))
    }
  }
}
