package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.ClientContext
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */

class FireContextMitigation(witnessWeight: Double = 2d) extends FireContext(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class BRSContextMitigation(witnessWeight: Double = 2d) extends BRSContext(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}



class TravosContextMitigation extends TravosContext with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}



class BladeContextMitigation(numBins: Int, lower: Double, upper: Double) extends BladeContext(numBins, lower, upper) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}



class HabitContextMitigation(numBins: Int, lower: Double, upper: Double) extends HabitContext(numBins, lower, upper) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}





class FireContextMitigationLike(witnessWeight: Double,
                                baseLearner: Classifier,
                                numBins: Int,
                                lower: Double,
                                upper: Double) extends FireContextLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class BRSContextMitigationLike(baseLearner: Classifier,
                               numBins: Int,
                               lower: Double,
                               upper: Double) extends BRSContextLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}


class TravosContextMitigationLike(baseLearner: Classifier,
                                  numBins: Int,
                                  lower: Double,
                                  upper: Double) extends TravosContextLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class HabitContextMitigationLike(witnessWeight: Double,
                                 baseLearner: Classifier,
                                  numBins: Int,
                                  lower: Double,
                                  upper: Double) extends HabitContextLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

