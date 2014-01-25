import javax.swing.{UIManager, BorderFactory}
import scala.swing.event.ButtonClicked
import scala.swing.Orientation.Vertical
import org.jfree.chart.axis.LogAxis
import scalax.chart.Charting._
import scalax.chart.XYChart
import scala.swing._

/**
 * @author Jan Paw 
 *         Date: 1/22/14
 */
object App extends SwingApplication {
  lazy val Vr: Seq[Double] = Seq(4.251735223702 * 3600, 4.237057009356 * 3600, 4.153866781770 * 3600, 4.252470701687 * 3600, 4.539818783175 * 3600, 4.539818783175 * 3600, 4.539818783175 * 3600, 4.539818783175 * 3600, 5.031269055548 * 3600, 5.031269055548 * 3600, 6.087656481774 * 3600, 8.805141677392 * 3600, 8.805141677392 * 3600, 13.663108419912 * 3600)

  lazy val CTPData = Seq((0, 0)).toXYSeriesCollection("default")
  lazy val CTPChart: XYChart = XYDeviationChart(CTPData, title = "CTP", rangeAxisLabel = "%")
  CTPChart.plot.setDomainAxis(new LogAxis())

  lazy val hardnessData = Seq((0, 0)).toXYSeriesCollection("default")
  lazy val hardnessChart: XYChart = XYDeviationChart(hardnessData, title = "Hardness", rangeAxisLabel = "HV")

  lazy val carbon = textField
  carbon.text = "0.35"
  lazy val carbonLabel = new Label("C")

  lazy val manganese = textField
  manganese.text = "0.72"
  lazy val manganeseLabel = new Label("Mn")

  lazy val nickel = textField
  nickel.text = "0.09"
  lazy val nickelLabel = new Label("Ni")

  lazy val chromium = textField
  chromium.text = "1.13"
  lazy val chromiumLabel = new Label("Cr")

  lazy val molybdenum = textField
  molybdenum.text = "0.26"
  lazy val molybdenumLabel = new Label("Mo")

  lazy val sulfur = textField
  sulfur.text = "0.40"
  lazy val sulfurLabel = new Label("S")

  lazy val vanadium = textField
  vanadium.text = "0.05"
  lazy val vanadiumLabel = new Label("V")

  lazy val astenitizintTemperature = textField
  astenitizintTemperature.text = "900"
  lazy val asutenitizintTemperatureLabel = new Label("T")

  lazy val astenitizintTime = textField
  astenitizintTime.text = "1800"
  lazy val astenitizingTimeLabel = new Label("t")

  lazy val temperingTemperature = textField
  temperingTemperature.text = "600"
  lazy val temperingTemperatureLabel = new Label("T")

  lazy val temperingTime = textField
  temperingTime.text = "7200"
  lazy val temperingTimeLabel = new Label("t")

  lazy val compute = new Button("compute")

  lazy val alloyingElements = new GridPanel(7, 2) {
    contents ++= carbon :: carbonLabel ::
      manganese :: manganeseLabel ::
      sulfur :: sulfurLabel ::
      chromium :: chromiumLabel ::
      nickel :: nickelLabel ::
      molybdenum :: molybdenumLabel ::
      vanadium :: vanadiumLabel :: Nil
    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("alloying elements"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  lazy val austenitizing = new GridPanel(2, 2) {
    contents ++= astenitizintTemperature :: asutenitizintTemperatureLabel ::
      astenitizintTime :: astenitizingTimeLabel :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("austenitizing"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  lazy val tempering = new GridPanel(2, 2) {
    contents ++= temperingTemperature :: temperingTemperatureLabel ::
      temperingTime :: temperingTimeLabel :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("tempering"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  lazy val menu = new BoxPanel(Vertical) {
    contents ++= alloyingElements :: austenitizing :: tempering :: compute :: Nil
  }

  lazy val charts = new BoxPanel(Vertical) {
    contents ++= CTPChart.toPanel :: hardnessChart.toPanel :: Nil
  }

  lazy val panel = new FlowPanel() {
    contents ++= menu :: charts :: Nil
  }

  lazy val scrollPanel = new ScrollPane(panel)

  def top = new MainFrame {

    title = "Maynier Model"
    contents = scrollPanel

    listenTo(compute)
    reactions += {
      case ButtonClicked(`compute`) =>
        val model: Mayiner = Mayiner(
          carbon,
          manganese,
          nickel,
          chromium,
          molybdenum,
          sulfur,
          vanadium,
          astenitizintTemperature, astenitizintTime,
          temperingTemperature, temperingTime)

        val transitPoints: Seq[Double] = model.transitionPoints

        CTPData.removeAllSeries()

        CTPData.addSeries(
          Seq(
            (speedToTime(transitPoints(0)), 100),
            (speedToTime(transitPoints(1)), 90),
            (speedToTime(transitPoints(2)), 50),
            (speedToTime(transitPoints(3)), 0),
            (speedToTime(transitPoints(7)), 0)).toXYSeries("martensite"))

        CTPData.addSeries(
          Seq(
            (speedToTime(transitPoints(0)), 0),
            (speedToTime(transitPoints(1)), 10),
            (speedToTime(transitPoints(2)), 50),
            (speedToTime(transitPoints(3)), 100),
            (speedToTime(transitPoints(4)), 90),
            (speedToTime(transitPoints(5)), 50),
            (speedToTime(transitPoints(6)), 10),
            (speedToTime(transitPoints(7)), 0)).toXYSeries("bainite"))

        CTPData.addSeries(
          Seq(
            (speedToTime(transitPoints(0)), 0),
            (speedToTime(transitPoints(3)), 0),
            (speedToTime(transitPoints(4)), 10),
            (speedToTime(transitPoints(5)), 50),
            (speedToTime(transitPoints(7)), 100),
            (speedToTime(transitPoints(6)), 90)).toXYSeries("ferrite-perlite"))

        hardnessData.removeAllSeries()

        val hardeningV: Seq[Double] = Vr.map(model.quenchingHardness)
        hardnessData.addSeries((for (i <- 1 to Vr.length + 1) yield i).view.zip(hardeningV).toXYSeries("quenching"))

        val temperingV: Seq[Double] = Vr.map(model.temperingHardness)
        hardnessData.addSeries((for (i <- 1 to Vr.length + 1) yield i).view.zip(temperingV).toXYSeries("tempering"))

    }
  }

  override def startup(args: Array[String]) {
    UIManager.setLookAndFeel(
      UIManager.getSystemLookAndFeelClassName)
    top.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)

  def textField = new TextField {
    text = "0"
    columns = 5
    horizontalAlignment = Alignment.Left
  }

  implicit def TextField2Double(f: TextField): Double = f.text.toDouble

  def speedToTime(Vr: Double): Double = (astenitizintTemperature - 20) / Vr * 3600
}
