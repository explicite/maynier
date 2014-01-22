import javax.swing.BorderFactory
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
  val data = Seq((0, 0)).toXYSeriesCollection("default")
  val chart: XYChart = XYDeviationChart(data, title = "CTPc" , rangeAxisLabel = "%")

  val carbon = field
  carbon.text = "0.35"
  val carbonLabel = new Label("C")

  val manganese = field
  manganese.text = "0.72"
  val manganeseLabel = new Label("Mn")

  val nickel = field
  nickel.text = "0.09"
  val nickelLabel = new Label("Ni")

  val chromium = field
  chromium.text = "1.13"
  val chromiumLabel = new Label("Cr")

  val molybdenum = field
  molybdenum.text = "0.26"
  val molybdenumLabel = new Label("Mo")

  val sulfur = field
  sulfur.text = "0.40"
  val sulfurLabel = new Label("S")

  val vanadium = field
  vanadium.text = "0.05"
  val vanadiumLabel = new Label("V")

  val temperature = field
  temperature.text = "900"
  val temperatureLabel = new Label("T")

  val time = field
  time.text = "1800"
  val timeLabel = new Label("t")

  val speed = field
  val speedLabel = new Label("v")

  val compute = new Button("compute")

  val period = new GridPanel(7, 2) {
    contents ++= carbon :: carbonLabel ::
      manganese :: manganeseLabel ::
      sulfur :: sulfurLabel ::
      chromium :: chromiumLabel ::
      nickel :: nickelLabel ::
      molybdenum :: molybdenumLabel ::
      vanadium :: vanadiumLabel :: Nil
    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("periods"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  val parameters = new GridPanel(3, 2) {
    contents ++= temperature :: temperatureLabel ::
      time :: timeLabel ::
      speed :: speedLabel :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("parameters"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  val menu = new BoxPanel(Vertical) {
    contents ++= period :: parameters :: compute :: Nil
  }

  lazy val panel = new FlowPanel() {
    contents ++= menu :: chart.toPanel :: Nil
  }

  def top = new MainFrame {

    title = "Maynier"
    contents = panel

    listenTo(compute)
    reactions += {
      case ButtonClicked(`compute`) =>
        val model: Mayiner = Mayiner(carbon, manganese, nickel, chromium, molybdenum, sulfur, vanadium, temperature, time, speed)

        data.removeAllSeries()
        data.addSeries(
          Seq(
            (model.m100, 100),
            (model.m90, 90),
            (model.m50, 50),
            (model.b100, 0),
            (model.fp50, 0)).toXYSeries("martensite"))

        data.addSeries(
          Seq(
            (model.m100, 0),
            (model.m90, 10),
            (model.m50, 50),
            (model.b100, 100),
            (model.b90, 90),
            (model.b50, 50),
            (model.fp90, 10),
            (model.fp50, 0)).toXYSeries("bainite"))

        data.addSeries(
          Seq(
            (model.m100, 0),
            (model.b100, 0),
            (model.b90, 10),
            (model.b50, 50),
            (model.fp50, 100),
            (model.fp90, 90)).toXYSeries("ferrite-perlite"))
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
