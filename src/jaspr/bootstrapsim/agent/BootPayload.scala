package jaspr.bootstrapsim.agent

import jaspr.core.agent.Property
import jaspr.core.service.Payload

import scala.collection.immutable.SortedMap

/**
  * Created by phil on 29/09/2016.
  */
class BootPayload(override val name: String, val quality: SortedMap[String, Property] = Nil) extends Payload {

  override def toString: String = name + " " + quality.values.map(_.value)+"-"+quality.values

  def copy(name: String = this.name,
           properties: SortedMap[String,Property] = this.quality) = {
    new BootPayload(name, quality = properties)
  }
}
