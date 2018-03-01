package jaspr.marketsim.strategy

import jaspr.core.agent.{Client, Provider}
import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.StrategyInit
import jaspr.utilities.Aggregate

/**
  * Created by phil on 18/01/2017.
  */
class Fire(val witnessWeight: Double = 2d) extends StrategyCore {

  val discountOpinions: Boolean = false
  val defaultMean: Double = 0d

  override val name: String =
    this.getClass.getSimpleName+"-"+witnessWeight+
      (if (discountOpinions) "-discountOpinions" else "")

  override def compute(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val init = baseInit.asInstanceOf[FireInit]

    val direct: Aggregate = getRequestOpinion(request, init.directAggregate)
    val opinions: Iterable[Aggregate] = init.witnessAggregate.values.map(
      getRequestOpinion(request, _)
    )

    val aggregate = getCombinedOpinions(direct, opinions, witnessWeight)
    val score = aggregate.result / (aggregate.size+1d)

    new TrustAssessment(init.context, request, score)
  }

  def getRequestOpinion(request: ServiceRequest,
                        aggregates: Map[Provider,Aggregate],
                        default: Aggregate = new Aggregate(defaultMean,1)
                       ): Aggregate = {
    aggregates.getOrElse(request.provider, default)
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): StrategyInit = {
    val directRecords: Seq[ServiceRecord with RatingRecord] = getDirectRecords(network, context)
    val witnessRecords: Seq[ServiceRecord with RatingRecord] = getWitnessRecords(network, context)

    val directAggregate: Map[Provider,Aggregate] = makeDirectAggregate(directRecords, context)
    val witnessAggregate: Map[Client,Map[Provider,Aggregate]] = makeWitnessAggregate(witnessRecords, context)

    new FireInit(context, directAggregate, witnessAggregate)
  }

  def getCombinedOpinions(direct: Aggregate,
                          opinions: Iterable[Aggregate],
                          witnessWeight: Double): Aggregate = {
    if (witnessWeight == 0) direct
    else if (witnessWeight == 1) opinions.foldLeft(new Aggregate())(_ + _)
    else if (witnessWeight == 2) opinions.foldLeft(direct)(_ + _)
    else getCombinedOpinions(direct * (1-witnessWeight), opinions.map(_ * witnessWeight), witnessWeight = 2)
  }

  def makeDirectAggregate(directRecords: Seq[ServiceRecord with RatingRecord], context: ClientContext): Map[Provider,Aggregate] = {
    if (witnessWeight != 1) makeOpinions(directRecords, context, r => r.service.request.provider)
    else Map()
  }

  def makeWitnessAggregate(witnessRecords: Seq[ServiceRecord with RatingRecord], context: ClientContext): Map[Client,Map[Provider,Aggregate]] = {
    if (witnessWeight > 0) makeOpinions(witnessRecords, context, r => r.service.request.client, r => r.service.request.provider)
    else Map()
  }

  def makeOpinions[K](records: Iterable[ServiceRecord with RatingRecord],
                      context: ClientContext,
                      grouping: ServiceRecord with RatingRecord => K): Map[K,Aggregate] = {
    records.groupBy(
      grouping
    ).map(
      rs => rs._1 -> new Aggregate(rs._2.map(x => weightedRating(x, context)).sum/rs._2.size.toDouble, 1)
    )
  }

  def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    record.rating
  }

  def makeOpinions[K1,K2](records: Iterable[ServiceRecord with RatingRecord],
                          context: ClientContext,
                          grouping1: ServiceRecord with RatingRecord => K1,
                          grouping2: ServiceRecord with RatingRecord => K2): Map[K1,Map[K2,Aggregate]] = {
    records.groupBy(
      grouping1
    ).map(
      rs => rs._1 -> makeOpinions(rs._2, context, grouping2)
    )
  }

}

