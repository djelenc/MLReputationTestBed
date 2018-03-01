package jaspr.marketsim.agent

import jaspr.core.agent.{Event, Provider}
import jaspr.core.service.Service

/**
  * Created by phil on 04/07/17.
  */
class MarketEvent(override val name: String) extends Event {

  override val providers: Seq[Provider] = Nil

  override def affect(service: Service): Unit = {}

  override def affect(provider: Provider): Unit = {}
}
