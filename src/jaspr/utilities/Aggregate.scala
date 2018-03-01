package jaspr.utilities

/**
  * Created by phil on 18/01/2017.
  */
class Aggregate(val result: Double = 0, val size: Double = 0) {

  def +(that: Aggregate): Aggregate = {
//    println(this, that)
    new Aggregate(result + that.result, size+1)
  }

  def *(that: Double): Aggregate = {
    new Aggregate(result * that, size)
  }

  override def toString: String = "["+result+","+size+"]"
}