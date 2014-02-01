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

  lazy val carbon: TextField = 0.35
  lazy val carbonLabel = new Label("C")

  lazy val manganese: TextField = 0.72
  lazy val manganeseLabel = new Label("Mn")

  lazy val nickel: TextField = 0.09
  nickel.text = "0.09"
  lazy val nickelLabel = new Label("Ni")

  lazy val chromium: TextField = 1.13
  lazy val chromiumLabel = new Label("Cr")

  lazy val molybdenum: TextField = 0.26
  lazy val molybdenumLabel = new Label("Mo")

  lazy val silicon: TextField = 0.4
  lazy val siliconLabel = new Label("Si")

  lazy val vanadium: TextField = 0.05
  lazy val vanadiumLabel = new Label("V")

  lazy val astenitizingTemperature: TextField = 900
  lazy val asutenitizintTemperatureLabel = new Label("T")

  lazy val astenitizingTime: TextField = 1800
  lazy val astenitizingTimeLabel = new Label("t")

  lazy val temperingTemperature: TextField = 600
  lazy val temperingTemperatureLabel = new Label("T")

  lazy val temperingTime: TextField = 7200
  lazy val temperingTimeLabel = new Label("t")

  lazy val compute = new Button("compute")

  lazy val alloyingElements = new GridPanel(7, 2) {
    contents ++= carbon :: carbonLabel ::
      manganese :: manganeseLabel ::
      silicon :: siliconLabel ::
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
    contents ++= astenitizingTemperature :: asutenitizintTemperatureLabel ::
      astenitizingTime :: astenitizingTimeLabel :: Nil

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
          silicon,
          vanadium,
          astenitizingTemperature, astenitizingTime,
          temperingTemperature, temperingTime)

        val transitPoints: Seq[Double] = model.transitionPoints
        def speedToTime(Vr: Int): Double = (astenitizingTemperature - 20) / transitPoints(Vr) * 3600

        CTPData.removeAllSeries()

        CTPData.addSeries(
          Seq(
            (speedToTime(0), 100),
            (speedToTime(1), 90),
            (speedToTime(2), 50),
            (speedToTime(3), 0),
            (speedToTime(7), 0)).toXYSeries("martensite"))

        CTPData.addSeries(
          Seq(
            (speedToTime(0), 0),
            (speedToTime(1), 10),
            (speedToTime(2), 50),
            (speedToTime(3), 100),
            (speedToTime(4), 90),
            (speedToTime(5), 50),
            (speedToTime(6), 10),
            (speedToTime(7), 0)).toXYSeries("bainite"))

        CTPData.addSeries(
          Seq(
            (speedToTime(0), 0),
            (speedToTime(3), 0),
            (speedToTime(4), 10),
            (speedToTime(5), 50),
            (speedToTime(7), 100),
            (speedToTime(6), 90)).toXYSeries("ferrite-perlite"))

        hardnessData.removeAllSeries()

        val hardeningHV: Seq[Double] = Vr.map(model.quenchingHardness)
        hardnessData.addSeries((for (i <- 1 to Vr.length + 1) yield i).view.zip(hardeningHV).toXYSeries("quenching"))

        val temperingHV: Seq[Double] = Vr.map(model.temperingHardness)
        hardnessData.addSeries((for (i <- 1 to Vr.length + 1) yield i).view.zip(temperingHV).toXYSeries("tempering"))

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

  def textField(d: Double) = new TextField {
    text = d.toString
    columns = 5
    horizontalAlignment = Alignment.Left
  }

  implicit def TextField2Double(f: TextField): Double = f.text.toDouble

  implicit def Double2TextField(d: Double): TextField = textField(d)
}
