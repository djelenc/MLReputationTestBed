package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.ClientContext
import jaspr.utilities.Discretization

/**
  * Created by phil on 05/07/17.
  */
class DiscreteFire(witnessWeight: Double = 2d,
                   override val numBins: Int,
                   override val lower: Double,
                   override val upper: Double) extends Fire(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
//    if (record.success) 1d else 0d
  }
}

class DiscreteFireContext(witnessWeight: Double = 2d,
                   override val numBins: Int,
                   override val lower: Double,
                   override val upper: Double) extends FireContext(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
//    if (record.success) super.weightedRating(record,context) else 0d
  }
}

class DiscreteFireContextFilter(witnessWeight: Double = 2d,
                          override val numBins: Int,
                          override val lower: Double,
                          override val upper: Double) extends FireContextFilter(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
  }
}

class DiscreteFireStereotype(witnessWeight: Double = 2d,
                   override val numBins: Int,
                   override val lower: Double,
                   override val upper: Double) extends FireStereotype(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
  }
}

class DiscreteFireStereotypeContext(witnessWeight: Double = 2d,
                   override val numBins: Int,
                   override val lower: Double,
                   override val upper: Double) extends FireStereotype(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
  }
}

class DiscreteFireMitigation(witnessWeight: Double = 2d,
                   override val numBins: Int,
                   override val lower: Double,
                   override val upper: Double) extends FireMitigation(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
  }
}

class DiscreteFireContextMitigation(witnessWeight: Double = 2d,
                             override val numBins: Int,
                             override val lower: Double,
                             override val upper: Double) extends FireContextMitigation(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
  }
}

class DiscreteFireContextFilterMitigation(witnessWeight: Double = 2d,
                                    override val numBins: Int,
                                    override val lower: Double,
                                    override val upper: Double) extends FireContextFilterMitigation(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
  }
}

class DiscreteFireStereotypeMitigation(witnessWeight: Double = 2d,
                             override val numBins: Int,
                             override val lower: Double,
                             override val upper: Double) extends FireStereotypeMitigation(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
  }
}


class DiscreteFireStereotypeContextMitigation(witnessWeight: Double = 2d,
                             override val numBins: Int,
                             override val lower: Double,
                             override val upper: Double) extends FireStereotypeContextMitigation(witnessWeight) with Discretization {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    discretizeDouble(super.weightedRating(record,context))
  }
}
