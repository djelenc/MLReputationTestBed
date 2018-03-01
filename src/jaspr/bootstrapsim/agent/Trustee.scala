package jaspr.bootstrapsim.agent

import jaspr.bootstrapsim.BootSimulation
import jaspr.core.agent.{Property, Provider}
import jaspr.core.provenance.{Provenance, Record}
import jaspr.core.service.{Payload, Service, ServiceRequest}

import scala.collection.immutable.SortedMap

/**
  * Created by phil on 27/09/2016.
  */
class Trustee(override val simulation: BootSimulation) extends Provider {
  override def capableOf(payload: Payload, duration: Int): Boolean = {
    true
  }

  override def receiveRequest(request: ServiceRequest): Boolean = {
    currentServices += new BootService(request)
    true
  }

  override def affectService(service: Service): Unit = {
    service.payload = service.payload.asInstanceOf[BootPayload]copy(properties = properties)
  }

  override def getProvenance[T <: Record](agent: Provenance): Seq[T] = ???

  override val memoryLimit: Int = simulation.config.memoryLimit


  override val properties: SortedMap[String, Property] = simulation.config.properties(this)
  override val adverts: SortedMap[String, Property] = simulation.config.adverts(this)

  override def utility: Double = ???
}
