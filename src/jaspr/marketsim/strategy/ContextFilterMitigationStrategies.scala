package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.ClientContext
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */


class FireContextFilterMitigation(witnessWeight: Double = 2d) extends FireContextFilter(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}

class BRSContextFilterMitigation(witnessWeight: Double = 2d) extends BRSContextFilter(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}

class TravosContextFilterMitigation extends TravosContextFilter with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class BladeContextFilterMitigation(numBins: Int, lower: Double, upper: Double) extends BladeContextFilter(numBins, lower, upper) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class HabitContextFilterMitigation(numBins: Int, lower: Double, upper: Double) extends HabitContextFilter(numBins, lower, upper) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}





class FireContextFilterMitigationLike(witnessWeight: Double,
                                baseLearner: Classifier,
                                numBins: Int,
                                lower: Double,
                                upper: Double) extends FireContextFilterLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class BRSContextFilterMitigationLike(baseLearner: Classifier,
                               numBins: Int,
                               lower: Double,
                               upper: Double) extends BRSContextFilterLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}


class TravosContextFilterMitigationLike(baseLearner: Classifier,
                                  numBins: Int,
                                  lower: Double,
                                  upper: Double) extends TravosContextFilterLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class HabitContextFilterMitigationLike(witnessWeight: Double,
                                 baseLearner: Classifier,
                                 numBins: Int,
                                 lower: Double,
                                 upper: Double) extends HabitContextFilterLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}
