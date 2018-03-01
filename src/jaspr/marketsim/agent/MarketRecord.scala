package jaspr.marketsim.agent

import jaspr.core.provenance.{RatingRecord, Record, ServiceRecord, TrustAssessmentRecord}
import jaspr.core.service.{Service, TrustAssessment}

/**
  * Created by phil on 18/01/17.
  */
class MarketRecord(override val assessment: TrustAssessment,
                   override val service: Service,
                   _rating: Double = Double.NaN
                  ) extends Record with TrustAssessmentRecord with ServiceRecord with RatingRecord {



  val delivered: MarketPayload = service.payload.asInstanceOf[MarketPayload]

  override val rating: Double = {
    if (_rating.isNaN) {
      val requested = service.request.payload.asInstanceOf[MarketPayload]
      val disparity = requested.properties.map(r =>
        delivered.properties.get(r._1) match {
          case Some(d) => d.doubleValue - r._2.doubleValue
          case None => 0
        }
      )
//      println(requested.properties,delivered.properties,disparity,disparity.sum / disparity.size.toDouble)
      disparity.sum / disparity.size.toDouble
//      if (disparity.sum > 0d) 1d else 0d
    } else {
      _rating
    }
  }

  val success: Boolean = rating > 0d

  def copy(assessment: TrustAssessment = assessment,
           service: Service = service,
           rating: Double = rating): MarketRecord = {
    new MarketRecord(assessment, service, rating)
  }

}
