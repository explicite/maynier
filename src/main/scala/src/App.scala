import javax.swing.BorderFactory
import org.jfree.chart.axis.LogAxis
import scala.swing.event.ButtonClicked
import scalax.chart.XYChart
import scala.swing._
import scalax.chart.Charting._
import scala.swing.Orientation._

/**
 * @author Jan Paw 
 *         Date: 1/22/14
 */
object App extends SwingApplication {
  lazy val Vr: Seq[Double] = Seq(4.271768789, 4.253662777, 4.186012187, 4.587602184, 4.563631816, 4.563631816, 5.064236646, 5.064236646, 6.147445089, 6.208296092, 6.347445089, 8.90295406, 9.10295406, 12.95147088)

  lazy val CTPData = Seq((0, 0)).toXYSeriesCollection("default")
  lazy val CTPChart: XYChart = XYDeviationChart(CTPData, title = "CTP", rangeAxisLabel = "%", domainAxisLabel = "T[s]")
  CTPChart.plot.setDomainAxis(new LogAxis())

  lazy val hardnessData = Seq((0, 0)).toXYSeriesCollection("default")
  lazy val hardnessChart: XYChart = XYDeviationChart(hardnessData, title = "Hardness", rangeAxisLabel = "HV")

  lazy val carbon = field
  carbon.text = "0.35"
  lazy val carbonLabel = new Label("C")

  lazy val manganese = field
  manganese.text = "0.72"
  lazy val manganeseLabel = new Label("Mn")

  lazy val nickel = field
  nickel.text = "0.09"
  lazy val nickelLabel = new Label("Ni")

  lazy val chromium = field
  chromium.text = "1.13"
  lazy val chromiumLabel = new Label("Cr")

  lazy val molybdenum = field
  molybdenum.text = "0.26"
  lazy val molybdenumLabel = new Label("Mo")

  lazy val sulfur = field
  sulfur.text = "0.40"
  lazy val sulfurLabel = new Label("S")

  lazy val vanadium = field
  vanadium.text = "0.05"
  lazy val vanadiumLabel = new Label("V")

  lazy val astenitizintTemperature = field
  astenitizintTemperature.text = "900"
  lazy val temperatureLabel = new Label("T")

  lazy val astenitizintTime = field
  astenitizintTime.text = "1800"
  lazy val timeLabel = new Label("t")

  lazy val temperingTemperature = field
  temperingTemperature.text = "400"

  lazy val temperingTime = field
  temperingTime.text = "1800"

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
    contents ++= astenitizintTemperature :: temperatureLabel ::
      astenitizintTime :: timeLabel :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("austenitizing"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  lazy val tempering = new GridPanel(2, 2) {
    contents ++= temperingTemperature :: temperatureLabel ::
      temperingTime :: timeLabel :: Nil

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
        val model: Mayiner = Mayiner(carbon, manganese, nickel, chromium, molybdenum, sulfur, vanadium, astenitizintTemperature, astenitizintTime, temperingTemperature, temperingTime)
        val points: Seq[Double] = model.transitPoints
        CTPData.removeAllSeries()
        CTPData.addSeries(
          Seq(
            (points(0), 100),
            (points(1), 90),
            (points(2), 50),
            (points(3), 0),
            (points(7), 0)).toXYSeries("martensite"))

        CTPData.addSeries(
          Seq(
            (points(0), 0),
            (points(1), 10),
            (points(2), 50),
            (points(3), 100),
            (points(4), 90),
            (points(5), 50),
            (points(6), 10),
            (points(7), 0)).toXYSeries("bainite"))

        CTPData.addSeries(
          Seq(
            (points(0), 0),
            (points(3), 0),
            (points(4), 10),
            (points(5), 50),
            (points(7), 100),
            (points(6), 90)).toXYSeries("ferrite-perlite"))

        hardnessData.removeAllSeries()

        val hardeningV: Seq[Double] = Vr.map(model.quenchingHardness)
        hardnessData.addSeries((for (i <- 1 to Vr.length + 1) yield i).view.zip(hardeningV).toXYSeries("quenching"))

        val temperingV: Seq[Double] = Vr.map(model.temperingHardness)
        hardnessData.addSeries((for (i <- 1 to Vr.length + 1) yield i).view.zip(temperingV).toXYSeries("tempering"))

    }
  }

  override def startup(args: Array[String]) {
    top.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)

  def field = new TextField {
    text = "0"
    columns = 5
    horizontalAlignment = Alignment.Left
  }

  implicit def TextField2Double(f: TextField): Double = f.text.toDouble
}
