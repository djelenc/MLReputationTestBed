package jaspr.marketsim.strategy

import jaspr.core.agent.Provider
import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest}
import jaspr.core.strategy.StrategyInit
import jaspr.utilities.matrix.RowVector
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */

class FireContext(witnessWeight: Double = 2d) extends Fire(witnessWeight) with ContextCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record,context) * contextMatch(record.service.payload, context.payload)
  }
}


class BRSContext(witnessWeight: Double = 2d) extends BRS(witnessWeight) with ContextCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record,context) * contextMatch(record.service.payload, context.payload)
  }
}



class TravosContext extends Travos with ContextCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record,context) * contextMatch(record.service.payload, context.payload)
  }
}



class BladeContext(numBins: Int, lower: Double, upper: Double) extends Blade(numBins, lower, upper) with ContextCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record,context) * contextMatch(record.service.payload, context.payload)
  }
}



class HabitContext(numBins: Int, lower: Double, upper: Double) extends Habit(numBins, lower, upper) with ContextCore {

  override def getDirectRecordWeights(directRatings: Seq[ServiceRecord with RatingRecord], context: ClientContext): Map[Provider,RowVector] = {
    directRatings.groupBy(_.provider).map(x => x._1 ->
      new RowVector(x._2.map(r => weightedRating(r, context)))
    )
  }

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record,context) * contextMatch(record.service.payload, context.payload)
  }
}






class FireContextLike(witnessWeight: Double = 2d,
                      baseLearner: Classifier,
                      numBins: Int,
                      lower: Double,
                      upper: Double) extends FireLike(witnessWeight, baseLearner, numBins, lower, upper) with ContextCore {

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      context(record.service.request.payload)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      context(request.payload)
  }
}

class BRSContextLike(baseLearner: Classifier,
                     numBins: Int,
                     lower: Double,
                     upper: Double) extends BRSLike(baseLearner, numBins, lower, upper) with ContextCore {

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      context(record.service.request.payload)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      context(request.payload)
  }
}

class TravosContextLike(baseLearner: Classifier,
                         numBins: Int,
                         lower: Double,
                         upper: Double) extends TravosLike(baseLearner, numBins, lower, upper) with ContextCore {

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      context(record.service.request.payload)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      context(request.payload)
  }
}


class HabitContextLike(witnessWeight: Double = 2d,
                        baseLearner: Classifier,
                        numBins: Int,
                        lower: Double,
                        upper: Double) extends HabitLike(witnessWeight, baseLearner, numBins, lower, upper) with ContextCore {

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      context(record.service.request.payload)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      context(request.payload)
  }
}