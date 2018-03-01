package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord, TrustAssessmentRecord}
import jaspr.core.service.ClientContext
import jaspr.core.simulation.Network
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */

class FireContextFilter(override val witnessWeight: Double = 2d) extends Fire(witnessWeight) with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}


class BRSContextFilter(override val witnessWeight: Double = 2d) extends BRS(witnessWeight) with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}


class TravosContextFilter extends Travos with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}


class BladeContextFilter(numBins: Int, lower: Double, upper: Double) extends Blade(numBins, lower, upper) with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}


class HabitContextFilter(numBins: Int, lower: Double, upper: Double) extends Habit(numBins, lower, upper) with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}






class FireContextFilterLike(witnessWeight: Double = 2d,
                      baseLearner: Classifier,
                      numBins: Int,
                      lower: Double,
                      upper: Double) extends FireLike(witnessWeight, baseLearner, numBins, lower, upper) with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}

class BRSContextFilterLike(baseLearner: Classifier,
                     numBins: Int,
                     lower: Double,
                     upper: Double) extends BRSLike(baseLearner, numBins, lower, upper) with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}

class TravosContextFilterLike(baseLearner: Classifier,
                        numBins: Int,
                        lower: Double,
                        upper: Double) extends TravosLike(baseLearner, numBins, lower, upper) with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}


class HabitContextFilterLike(witnessWeight: Double = 2d,
                       baseLearner: Classifier,
                       numBins: Int,
                       lower: Double,
                       upper: Double) extends HabitLike(witnessWeight, baseLearner, numBins, lower, upper) with ContextCore {

  override def getDirectRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getDirectRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }

  override def getWitnessRecords(network: Network, context: ClientContext): Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = {
    super.getWitnessRecords(network, context).filter(x => x.service.payload.name == context.payload.name)
  }
}
