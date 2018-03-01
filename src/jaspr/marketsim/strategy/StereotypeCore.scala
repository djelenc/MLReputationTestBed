package jaspr.marketsim.strategy

import jaspr.core.agent.{AdvertProperties, Provider}

/**
  * Created by phil on 19/01/17.
  */
trait StereotypeCore {

  def stereotypeMatchThreshold: Double = 0.75

  def adverts(provider: Provider): List[Any] = {
    provider.adverts.values.map(_.value.toString).toList
  }

  def stereotypeMatch(a: Provider, b: Provider): Double = {
    val x = if (a.name == b.name) 1d
//    else 0
    else (a,b) match {
      case (x:AdvertProperties,y:AdvertProperties) =>
        if (x.adverts.isEmpty || y.adverts.isEmpty) 0d
        else {
          val xy = x.adverts.values.map(_.booleanValue).zip(y.adverts.values.map(_.booleanValue))
          val xandy = xy.count(p => { p._1 && p._2 })
          val xory = xy.count(p => { p._1 || p._2 })
          xandy.toDouble / xory.toDouble
        }
      case _ => 0d
    }
//    println(a.name, b.name, x)
    x
  }

  def stereotypeMatches(a: Provider, b: Provider): Boolean = {
    stereotypeMatch(a,b) >= stereotypeMatchThreshold
  }
}
