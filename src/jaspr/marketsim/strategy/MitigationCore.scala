package jaspr.marketsim.strategy

import jaspr.core.provenance.ServiceRecord

/**
  * Created by phil on 05/07/17.
  */
trait MitigationCore {

  val baseMitigation = 0.1

  def needsMitigation(record: ServiceRecord): Boolean = {
    record.service.serviceContext.events.isEmpty
  }

  def mitigationWeight(record: ServiceRecord): Double = {
    if (needsMitigation(record)) 1d else baseMitigation
  }
}