package recog

import org.opencv.core.Mat
import scala.collection.Seq
import minefinder.grid.Grid
import minefinder.grid.PeriodDetector
import minefinder.grid.GridDetector
import org.opencv.core.Core
import org.opencv.core.Scalar
import org.opencv.core.Point


class GridProcessor extends ImageProcessor[Seq[Array[Double]], Option[Grid]] {

  def name(): String = { "Grid" }

  val falsePositive = new DoubleParameter("falsePositive", 1)
  val falseNegative = new DoubleParameter("falseNegative", 1)
  val rejectThreshold = new DoubleParameter("rejectThreshold", 30)
  def parameters(): Seq[DoubleParameter] = Seq(falsePositive, falseNegative, rejectThreshold)

  def draw(target: Mat, result: Any) {
    result.asInstanceOf[Option[Grid]].map(grid => {
      for (tick <- grid.x.ticks) {
        Core.line(target, new Point(tick, grid.y.start), new Point(tick, grid.y.stop), new Scalar(0, 0, 255), 1)
      }

      for (tick <- grid.y.ticks) {
        Core.line(target, new Point(grid.x.start, tick), new Point(grid.x.stop, tick), new Scalar(0, 0, 255), 1)
      }
    })
    
  }
  
  def apply(input:Seq[Array[Double]]) : Option[Grid] = {
    val gd = new GridDetector(falsePositive.value, falseNegative.value, rejectThreshold.value)
    gd(input)
  }
  
}