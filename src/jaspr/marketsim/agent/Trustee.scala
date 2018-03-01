package jaspr.marketsim.agent

import jaspr.core.agent.{FixedProperty, Property, Provider}
import jaspr.core.provenance.{Provenance, Record}
import jaspr.core.service.{Payload, Service, ServiceRequest}
import jaspr.marketsim.MarketSimulation
import jaspr.utilities.Chooser

import scala.collection.immutable.SortedMap

/**
  * Created by phil on 18/01/17.
  */
class Trustee(override val simulation: MarketSimulation) extends Provider {
  override def capableOf(payload: Payload, duration: Int): Boolean = {
    capabilities.contains(payload.name)
  }

  override def receiveRequest(request: ServiceRequest): Boolean = {
    currentServices += new MarketService(request)
    true
  }

  override def affectService(service: Service): Unit = {
    val payload = capabilities(service.payload.name)
    if (Chooser.randomBoolean(simulation.config.freakEventLikelihood)) {
      service.serviceContext.addEvent(new MarketEvent("Storm"))
      service.payload = payload.copy(
        properties = payload.properties.map(x => x._1 -> FixedProperty(x._1,-1d))
      )
    } else {
      service.payload = payload.copy(
        properties = payload.properties.map(x => x._1 -> x._2.sample)
      )
    }
  }

  override def getProvenance[T <: Record](agent: Provenance): Seq[T] = ???

  override val memoryLimit: Int = simulation.config.memoryLimit


  override val properties: SortedMap[String, Property] = simulation.config.properties(this)
  val capabilities: Map[String, MarketPayload] = simulation.config.capabilities(this).map(x => x.name -> x).toMap

  override val adverts: SortedMap[String, Property] = simulation.config.adverts(this)

  override def utility: Double = ???
}
