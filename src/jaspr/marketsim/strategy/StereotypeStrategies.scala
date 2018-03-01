package jaspr.marketsim.strategy

import jaspr.core.agent.Provider
import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest}
import jaspr.core.strategy.StrategyInit
import jaspr.utilities.{Aggregate, BetaDistribution}
import weka.classifiers.Classifier

/**
  * Created by phil on 05/07/17.
  */

class FireStereotype(witnessWeight: Double = 2d) extends Fire(witnessWeight) with StereotypeCore {

  override def getRequestOpinion(request: ServiceRequest,
                                 aggregates: Map[Provider,Aggregate],
                                 default: Aggregate = new Aggregate(0,0)
                                ): Aggregate = {
    aggregates.map(x =>
      x._2 * stereotypeMatch(x._1,request.provider)
    ).foldLeft(default)(_ + _)
  }
}

class BRSStereotype(witnessWeight: Double = 2d) extends BRS(witnessWeight) with StereotypeCore {

  override def makeOpinions[K](records: Iterable[ServiceRecord with RatingRecord],
                               context: ClientContext,
                               grouping: ServiceRecord with RatingRecord => K): Map[K,BetaDistribution] = {
    records.groupBy(
      grouping
    ).map(
      rs => rs._1 -> makeBetaDistribution(records.filter(x => stereotypeMatches(rs._1.asInstanceOf[Provider], x.provider)), context)
    )
  }
//  def makeBetaDistribution(records: Iterable[ServiceRecord with RatingRecord], context: ClientContext, provider: Provider): BetaDistribution = {
//    val alpha: Double = records.map(x => if (x.success) weightedRating(x, context, provider) else 0d).sum
//    val beta: Double = records.map(x => if (!x.success) weightedRating(x, context, provider) else 0d).sum
//    new BetaDistribution(alpha + 1d, beta + 1d)
//  }
//
//
//  def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext, provider: Provider): Double = {
//    super.weightedRating(record,context) * stereotypeMatch(record.provider, provider)
//  }
//  override def getRequestOpinion(request: ServiceRequest,
//                                 aggregates: Map[Provider,BetaDistribution],
//                                 default: BetaDistribution = new BetaDistribution(0,0)
//                                ): BetaDistribution = {
//    aggregates.map(x =>
//      x._2 * stereotypeMatch(x._1,request.provider)
//    ).foldLeft(default)(_ + _)
//  }
}


class TravosStereotype extends Travos with StereotypeCore {

  override def getRequestOpinion(request: ServiceRequest,
                                 aggregates: Map[Provider,BetaDistribution],
                                 default: BetaDistribution = new BetaDistribution(0,0)
                                ): BetaDistribution = {
    aggregates.map(x =>
      x._2 * stereotypeMatch(x._1,request.provider)
    ).foldLeft(default)(_ + _)
  }
}
//class FireStereotype(witnessWeight: Double = 2d
//                    ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.Fire("+witnessWeight+")")
//
//class BRSStereotype(witnessWeight: Double = 2d
//                    ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.BRS("+witnessWeight+")")
//
//class TravosStereotype extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.Travos[]")

class BladeStereotype(numBins: Int, lower: Double, upper: Double
                     ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.Blade("+numBins+":"+lower+":"+upper+")")

class HabitStereotype(numBins: Int, lower: Double, upper: Double
                     ) extends StereotypeWeightedStrategy("jaspr.marketsim.strategy.Habit("+numBins+":"+lower+":"+upper+")")



class FireStereotypeLike(witnessWeight: Double = 2d,
                         baseLearner: Classifier,
                         numBins: Int,
                         lower: Double,
                         upper: Double) extends FireLike(witnessWeight, baseLearner, numBins, lower, upper) with StereotypeCore {

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      adverts(record.provider)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      adverts(request.provider)
  }
}

class BRSStereotypeLike(baseLearner: Classifier,
                        numBins: Int,
                        lower: Double,
                        upper: Double) extends BRSLike(baseLearner, numBins, lower, upper) with StereotypeCore {

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      adverts(record.provider)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      adverts(request.provider)
  }
}


class TravosStereotypeLike(baseLearner: Classifier,
                           numBins: Int,
                           lower: Double,
                           upper: Double) extends TravosLike(baseLearner, numBins, lower, upper) with StereotypeCore {

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++ record.service.request.provider.adverts.values.map(_.value.toString).toList
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      adverts(request.provider)
  }
}

class HabitStereotypeLike(witnessWeight: Double = 2d,
                          baseLearner: Classifier,
                          numBins: Int,
                          lower: Double,
                          upper: Double) extends HabitLike(witnessWeight, baseLearner, numBins, lower, upper) with StereotypeCore {

  override def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    super.makeTrainRow(record) ++
      adverts(record.provider)
  }

  override def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    super.makeTestRow(init, request) ++
      adverts(request.provider)
  }
}