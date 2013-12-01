package recog

import org.opencv.core.Mat

trait ImageProcessor[T, R] extends Function1[T, R] {
	def name: String
	def parameters: Seq[DoubleParameter]
	def draw(target: Mat, result:R)
}