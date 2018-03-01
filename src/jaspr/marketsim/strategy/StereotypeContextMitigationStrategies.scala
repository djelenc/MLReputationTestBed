package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.ClientContext
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */
class FireStereotypeContextMitigation(witnessWeight: Double = 2d) extends FireStereotypeContext(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class BRSStereotypeContextMitigation(witnessWeight: Double = 2d) extends BRSStereotypeContext(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class TravosStereotypeContextMitigation extends TravosStereotypeContext with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class BladeStereotypeContextMitigation(numBins: Int, lower: Double, upper: Double
                            ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.BladeContextMitigation("+numBins+":"+lower+":"+upper+")") {

}

class HabitStereotypeContextMitigation(numBins: Int, lower: Double, upper: Double
                            ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.HabitContextMitigation("+numBins+":"+lower+":"+upper+")") {

}




class FireStereotypeContextMitigationLike(witnessWeight: Double,
                                   baseLearner: Classifier,
                                   numBins: Int,
                                   lower: Double,
                                   upper: Double) extends FireStereotypeContextLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class BRSStereotypeContextMitigationLike(baseLearner: Classifier,
                                  numBins: Int,
                                  lower: Double,
                                  upper: Double) extends BRSStereotypeContextLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}


class TravosStereotypeContextMitigationLike(baseLearner: Classifier,
                                     numBins: Int,
                                     lower: Double,
                                     upper: Double) extends TravosStereotypeContextLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class HabitStereotypeContextMitigationLike(witnessWeight: Double,
                                    baseLearner: Classifier,
                                    numBins: Int,
                                    lower: Double,
                                    upper: Double) extends HabitStereotypeContextLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}
