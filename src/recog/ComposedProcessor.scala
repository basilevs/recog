package recog

import org.opencv.core.Mat
import scala.collection.Seq

object ComposedProcessor {
  
  def findSecond[T, R](where:ImageProcessor[_,_], what: ImageProcessor[T, R]): Option[ComposedProcessor[_, T, R]] = {
    where match {
      case composed: ComposedProcessor[_, _, _] => {
        if (composed.second == what) {
          Some(where.asInstanceOf[ComposedProcessor[_, T, R]])
        } else {
        	findSecond(composed.first, what) orElse { findSecond(composed.second, what) }
        }
      }
      case _ => None
    }
  }
  
  def findFirst[T, R](where:ImageProcessor[_,_], what: ImageProcessor[T, R]): Option[ComposedProcessor[T, R, _]] = {
    where match {
      case composed: ComposedProcessor[_, _, _] => {
        if (composed.first == what) {
          Some(where.asInstanceOf[ComposedProcessor[T, R, _]])
        } else {
        	findFirst(composed.first, what) orElse { findFirst(composed.second, what) }
        }
      }
      case _ => None
    }
  }

}

class ComposedProcessor[-T, M, R] (val second:ImageProcessor[M, R], val first:ImageProcessor[T, M])
extends ImageProcessor[T, R] {

  def name(): String = second.name + " o " + first.name

  def parameters = second.parameters ++ first.parameters
  
  def apply(input:T): R = {
    second.apply(first.apply(input))
  }
  
  def draw(target: Mat, result: Any) {
	  second.draw(target, result.asInstanceOf[R])
  }
  
  def applyWithHook[X](input:T)(hook: (ImageProcessor[_, _], Any) => Unit):R = {
    val intermediateResult = first.apply(input)    
    hook(first, intermediateResult)
    val rv = second.apply(intermediateResult)
    hook(second, rv)
    rv
  }
  
  def +[X](next:ImageProcessor[R, X]): ComposedProcessor[T, R, X] = {
    new ComposedProcessor(next, this)
  }
}