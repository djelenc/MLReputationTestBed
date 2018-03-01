package jaspr.marketsim.strategy

import jaspr.core.agent.AdvertProperties
import jaspr.core.service.Payload

/**
  * Created by phil on 19/01/17.
  */
trait ContextCore {

  def contextMatchThreshold: Double = 0.7

  def context(payload: Payload): List[Any] = {
    val x =payload match {
      case p: AdvertProperties =>
        if (p.adverts.isEmpty) p.name :: Nil
        else p.adverts.values.map(_.value.toString).toList
      case p => p.name :: Nil
    }
//    println(x)
    x
  }

  def contextMatch(a: Payload, b: Payload): Double = {
    (a,b) match {
      case (x:AdvertProperties,y:AdvertProperties) =>
        if (x.adverts.isEmpty || y.adverts.isEmpty) if (a.name == b.name) 1d else 0d
        else {
          val xy = x.adverts.values.map(_.booleanValue).zip(y.adverts.values.map(_.booleanValue))
          val xandy = xy.count(p => { p._1 && p._2 })
          val xory = xy.count(p => { p._1 || p._2 })
//          println(xandy.toDouble, xory.toDouble, xy)
          xandy.toDouble / xory.toDouble
        }
      case _ => if (a.name == b.name) 1d else 0d
    }
  }

  def contextMatches(a: Payload, b: Payload): Boolean = {
    contextMatch(a,b) >= contextMatchThreshold
  }

}
