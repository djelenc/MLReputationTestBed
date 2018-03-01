package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.ClientContext
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */

class FireStereotypeMitigation(witnessWeight: Double = 2d) extends FireStereotype(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class BRSStereotypeMitigation(witnessWeight: Double = 2d) extends BRSStereotype(witnessWeight) with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}


class TravosStereotypeMitigation extends TravosStereotype with MitigationCore {

  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    super.weightedRating(record, context) * mitigationWeight(record)
  }
}

class BladeStereotypeMitigation(numBins: Int, lower: Double, upper: Double
                            ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.BladeMitigation("+numBins+":"+lower+":"+upper+")") {

}

class HabitStereotypeMitigation(numBins: Int, lower: Double, upper: Double
                            ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.HabitMitigation("+numBins+":"+lower+":"+upper+")") {

}






class FireStereotypeMitigationLike(witnessWeight: Double,
                                baseLearner: Classifier,
                                numBins: Int,
                                lower: Double,
                                upper: Double) extends FireStereotypeLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class BRSStereotypeMitigationLike(baseLearner: Classifier,
                               numBins: Int,
                               lower: Double,
                               upper: Double) extends BRSStereotypeLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}


class TravosStereotypeMitigationLike(baseLearner: Classifier,
                                  numBins: Int,
                                  lower: Double,
                                  upper: Double) extends TravosStereotypeLike(baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}

class HabitStereotypeMitigationLike(witnessWeight: Double,
                                 baseLearner: Classifier,
                                 numBins: Int,
                                 lower: Double,
                                 upper: Double) extends HabitStereotypeLike(witnessWeight, baseLearner, numBins, lower, upper) with MitigationCore {
  override val makeWeight: ServiceRecord => Double = mitigationWeight
}
