package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.ClientContext

/**
  * Created by phil on 19/07/17.
  */
class BinaryFire(witnessWeight: Double = 2d) extends Fire(witnessWeight) {

  override val defaultMean: Double = 0.0d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) 1d else -1d
  }
}

class BinaryFireContext(witnessWeight: Double = 2d) extends Fire(witnessWeight) with ContextCore {

  override val defaultMean: Double = 0.0d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) contextMatch(record.service.payload, context.payload) else -contextMatch(record.service.payload, context.payload)
  }
}

class BinaryFireContextFilter(witnessWeight: Double = 2d) extends FireContextFilter(witnessWeight) {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) 1d else -1d
  }
}

class BinaryFireStereotype(witnessWeight: Double = 2d) extends FireStereotype(witnessWeight) {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) 1d else -1d
  }
}

class BinaryFireStereotypeContext(witnessWeight: Double = 2d) extends FireStereotype(witnessWeight) with ContextCore {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) contextMatch(record.service.payload, context.payload) else -contextMatch(record.service.payload, context.payload)
  }
}

class BinaryFireMitigation(witnessWeight: Double = 2d) extends FireMitigation(witnessWeight) {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) mitigationWeight(record) else -mitigationWeight(record)
  }
}

class BinaryFireContextMitigation(witnessWeight: Double = 2d) extends FireContextMitigation(witnessWeight) {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) contextMatch(record.service.payload, context.payload) * mitigationWeight(record)
    else -contextMatch(record.service.payload, context.payload) * mitigationWeight(record)
  }
}

class BinaryFireContextFilterMitigation(witnessWeight: Double = 2d) extends FireContextFilterMitigation(witnessWeight) {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) mitigationWeight(record) else -mitigationWeight(record)
  }
}

class BinaryFireStereotypeMitigation(witnessWeight: Double = 2d) extends FireStereotypeMitigation(witnessWeight) {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) mitigationWeight(record) else -mitigationWeight(record)
  }
}


class BinaryFireStereotypeContextMitigation(witnessWeight: Double = 2d) extends FireStereotypeContextMitigation(witnessWeight) {

  override val defaultMean: Double = 0.5d
  override def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    if (record.success) contextMatch(record.service.payload, context.payload) * mitigationWeight(record)
    else -contextMatch(record.service.payload, context.payload) * mitigationWeight(record)
  }
}
