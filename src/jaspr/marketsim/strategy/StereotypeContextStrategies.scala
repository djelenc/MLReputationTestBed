package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest}
import jaspr.core.strategy.StrategyInit
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */

class FireStereotypeContext(witnessWeight: Double = 2d) extends FireStereotype(witnessWeight) with ContextCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * contextMatch(record.service.payload, context.payload)
  }
}


class BRSStereotypeContext(witnessWeight: Double = 2d) extends BRSStereotype(witnessWeight) with ContextCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * contextMatch(record.service.payload, context.payload)
  }
}


class TravosStereotypeContext extends TravosStereotype with ContextCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * contextMatch(record.service.payload, context.payload)
  }
}


class BladeStereotypeContext(numBins: Int, lower: Double, upper: Double
                            ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.BladeContext("+numBins+":"+lower+":"+upper+")") {

}

class HabitStereotypeContext(numBins: Int, lower: Double, upper: Double
                            ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.HabitContext("+numBins+":"+lower+":"+upper+")") {

}



class FireStereotypeContextLike(witnessWeight: Double = 2d,
                                baseLearner: Classifier,
                                numBins: Int,
                                lower: Double,
                                upper: Double) extends FireLike(witnessWeight, baseLearner, numBins, lower, upper) with StereotypeCore with ContextCore {

  override val contextMatchThreshold: Double = Double.NaN

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      adverts(record.provider) ++
      context(record.service.request.payload)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      adverts(request.provider) ++
      context(request.payload)
  }
}


class BRSStereotypeContextLike(baseLearner: Classifier,
                               numBins: Int,
                               lower: Double,
                               upper: Double) extends BRSLike(baseLearner, numBins, lower, upper) with StereotypeCore with ContextCore {

  override val contextMatchThreshold: Double = Double.NaN

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      adverts(record.provider) ++
      context(record.service.request.payload)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      adverts(request.provider) ++
      context(request.payload)
  }
}

class TravosStereotypeContextLike(baseLearner: Classifier,
                                  numBins: Int,
                                  lower: Double,
                                  upper: Double) extends TravosLike(baseLearner, numBins, lower, upper) with StereotypeCore with ContextCore {

  override val contextMatchThreshold: Double = Double.NaN

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      adverts(record.provider) ++
      context(record.service.request.payload)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      adverts(request.provider) ++
      context(request.payload)
  }
}


class HabitStereotypeContextLike(witnessWeight: Double = 2d,
                                 baseLearner: Classifier,
                                 numBins: Int,
                                 lower: Double,
                                 upper: Double) extends HabitLike(witnessWeight, baseLearner, numBins, lower, upper) with StereotypeCore with ContextCore {

  override val contextMatchThreshold: Double = Double.NaN

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      adverts(record.provider) ++
      context(record.service.request.payload)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      adverts(request.provider) ++
      context(request.payload)
  }
}