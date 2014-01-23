import scala.math.{log, log10, sqrt, exp, abs}

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
 * @param aT aust. temperature
 * @param at aust. time
 * @param tT tempering temperature
 * @param tt tempering time
 *
 * @author Jan Paw
 *         Date: 1/22/14
 */
case class Mayiner(C: Double, Mn: Double, Ni: Double, Cr: Double, Mo: Double, S: Double, V: Double, aT: Double, at: Double, tT: Double, tt: Double) {

  //Activation energy for low alloy steel [J/mol]
  val H: Double = 418000

  //Gas constant [J/mol*K]
  val R: Double = 8.3144621

  def Pa(T: Double, t: Double): Double = 1 / ((1 / T) - ((R / H) * log(t)))

  val transitPoints: Seq[Double] = {

    def m100: Double = abs(aT - 700.0) / exp(9.81 - ((4.62 * C) + (1.05 * Mn) + (0.54 * Ni) + (0.5 * Cr) + (0.66 * Mo) + (0.00183 * Pa(aT, at))))

    def m90: Double = abs(aT - 700) / exp(8.76 - ((4.04 * C) + (0.96 * Mn) + (0.49 * Ni) + (0.58 * Cr) + (0.97 * Mo) + (0.001 * Pa(aT, at))))

    def m50: Double = abs(aT - 700) / exp(8.50 - ((4.12 * C) + (0.86 * Mn) + (0.57 * Ni) + (0.41 * Cr) + (0.94 * Mo) + (0.0012 * Pa(aT, at))))


    def b100: Double = abs(aT - 700) / exp(10.17 - ((3.8 * C) + (1.07 * Mn) + (0.7 * Ni) + (0.57 * Cr) + (1.58 * Mo) + (0.0032 * Pa(aT, at))))

    def b90: Double = abs(aT - 700) / exp(10.55 - ((3.65 * C) + (1.08 * Mn) + (0.77 * Ni) + (0.61 * Cr) + (1.49 * Mo) + (0.004 * Pa(aT, at))))

    def b50: Double = abs(aT - 700) / exp(8.74 - ((2.23 * C) + (0.86 * Mn) + (0.56 * Ni) + (0.59 * Cr) + (1.6 * Mo) + (0.0032 * Pa(aT, at))))


    def fp90: Double = abs(aT - 700) / exp(7.51 - ((1.38 * C) + (0.35 * Mn) + (0.93 * Ni) + (0.11 * Cr) + (2.31 * Mo) + (0.0033 * Pa(aT, at))))

    def fp50: Double = abs(aT - 700) / exp(6.36 - ((0.43 * C) + (0.49 * Mn) + (0.78 * Ni) + (0.27 * Cr) + (0.38 * Mo) + (2 * sqrt(Mo)) + (0.0019 * Pa(aT, at))))

    Seq(m100, m90, m50, b100, b90, b50, fp90, fp50)
  }

  def martensiteQuenching(Vr: Double): Double = 127 + (949 * C) + (27 * S) + (11 * Mn) + (8 * Ni) + (16 * Cr) + (21 * log10(Vr))

  def bainiteQuenching(Vr: Double): Double = -323 + (158 * C) + (330 * S) + (153 * Mn) + (65 * Ni) + (144 * Cr) + (191 * Mo) + (log10(Vr) * (89 + (53 * C) - (55 * S) - (22 * Mn) - (10 * Ni) - (20 * Cr) - (33 * Mo)))

  def perliteFerriteQuenching(Vr: Double): Double = 42 + (223 * C) + (53 * S) + (30 * Mn) + (12.6 * Ni) + (7 * Cr) + (19 * Mo) + (log10(Vr) * (10 - (19 * S) + (4 * Ni) + (8 * Cr) + (130 * V)))

  def quenchingHardness(Vr: Double): Double = {
    val structure = findStructure(Vr)
    (martensiteQuenching(Vr) * structure._1) + (bainiteQuenching(Vr) * structure._2) + (perliteFerriteQuenching(Vr) * structure._3)
  }

  def martensiteTempering: Double = -74 - (434 * C) - (368 * S) + (15 * Mn) + (37 * Ni) + (17 * Cr) - (335 * Mo) - (2235 * V) +
    ((1000 / Pa(tT, tt)) * (260 + (616 * C) + (321 * S) + (21 * Mn) - (35 * Ni) - (11 * Cr) + (352 * Mo) + (2345 * V)))

  def bainiteTempering: Double = 262 + (163 * C) - (349 * S) - (64 * Mn) - (6 * Ni) - (186 * Cr) - (458 * Mo) - (867 * V) +
    ((1000 / Pa(tT, tt)) * (-149 + (43 * C) + (336 * S) + (79 * Mn) + (16 * Ni) + (196 * Cr) + (498 * Mo) + (1094 * V)))

  def temperingHardness(Vr: Double): Double = {
    val structure = findStructure(Vr)
    (martensiteTempering * structure._1) + (bainiteTempering * structure._2)
  }


  def findStructure(Vr: Double): (Double, Double, Double) = {
    def line(p1: (Double, Double), p2: (Double, Double), x: Double): Double = {
      (p2._2 - p1._2) / (p2._1 - p1._1) * (x - p1._1) + p1._2
    }

    def find(v: Double, i: Int): Int = {
      if (transitPoints(i) > v)
        i
      else {
        if (transitPoints.length > i)
          find(v, i + 1)
        else
          i
      }
    }

    find(Vr, 0) match {
      case 0 =>
        (1, 0, 0)
      case 1 =>
        (line((transitPoints(0), 1.0), (transitPoints(1), 0.9), Vr),
          line((transitPoints(0), 0), (transitPoints(1), 0.1), Vr), 0)
      case 2 =>
        (line((transitPoints(1), 0.9), (transitPoints(2), 0.5), Vr),
          line((transitPoints(1), 0.1), (transitPoints(2), 0.5), Vr), 0)
      case 3 =>
        (line((transitPoints(2), 0.5), (transitPoints(3), 0), Vr),
          line((transitPoints(2), 0.5), (transitPoints(3), 1), Vr), 0)
      case 4 =>
        (0,
          line((transitPoints(3), 1), (transitPoints(4), 0.9), Vr),
          line((transitPoints(3), 0), (transitPoints(4), 0.1), Vr))
      case 5 =>
        (0,
          line((transitPoints(4), 0.9), (transitPoints(5), 0.5), Vr),
          line((transitPoints(4), 0.1), (transitPoints(5), 0.5), Vr))
      case 6 =>
        (0,
          line((transitPoints(5), 0.5), (transitPoints(6), 0.1), Vr),
          line((transitPoints(5), 0.5), (transitPoints(6), 0.9), Vr))
      case 7 =>
        (0,
          line((transitPoints(6), 0.1), (transitPoints(7), 0), Vr),
          line((transitPoints(6), 0.9), (transitPoints(7), 1), Vr))
    }
  }
}
