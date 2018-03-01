package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.ClientContext
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */
class FireMitigation(witnessWeight: Double = 2d) extends Fire(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}

class BRSMitigation(witnessWeight: Double = 2d) extends BRS(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class TravosMitigation extends Travos with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class BladeMitigation(numBins: Int, lower: Double, upper: Double) extends Blade(numBins, lower, upper) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class HabitMitigation(numBins: Int, lower: Double, upper: Double) extends Habit(numBins, lower, upper) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}








class FireMitigationLike(witnessWeight: Double,
                         baseLearner: Classifier,
                         numBins: Int,
                         lower: Double,
                         upper: Double) extends FireLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}


class BRSMitigationLike(baseLearner: Classifier,
                        numBins: Int,
                        lower: Double,
                        upper: Double) extends BRSLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}


class TravosMitigationLike(baseLearner: Classifier,
                           numBins: Int,
                           lower: Double,
                           upper: Double) extends TravosLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class HabitMitigationLike(witnessWeight: Double,
                          baseLearner: Classifier,
                          numBins: Int,
                          lower: Double,
                          upper: Double) extends HabitLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}