package jaspr.marketsim.strategy

import jaspr.core.agent.{Client, Provider}
import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.StrategyInit
import jaspr.utilities.matrix.RowVector
import jaspr.utilities.{Dirichlet, Discretization}

/**
  * Created by phil on 12/06/17.
  */
class Habit(override val numBins: Int,
            override val lower: Double,
            override val upper: Double) extends StrategyCore with Discretization {

  override def compute(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val init = baseInit.asInstanceOf[HabitInit]
    val directObs = init.directObs
    val directRecordWeights = init.directRecordWeights
    val repModels = init.repModels
    val trustees = init.trustees
    val priorDist = init.priorDist

    val trustee = request.provider

    if (directObs.isEmpty && repModels.isEmpty) return new TrustAssessment(baseInit.context, request, 0d)

    // Generate mixture components by observing the direct observations for each trustee.
    //TODO: For some reason the below cannot be val directComponent = init.directComponent.
    // It's supposed to be immutable all the way down, but for some reason it makes the strategy stupid.
    val directComponent = trustees.map(x =>
      x -> priorDist.observe(directObs.getOrElse(x, Seq()), directRecordWeights.getOrElse(x, Seq()))
    )

    // Initialise the component weights to account for the marginal likelihood of the direct observations according to each component.
    val directWeights: RowVector = directComponent.map(x =>
      if (x._1 == trustee) priorDist.marginalLogLikelihood(directObs.getOrElse(trustee, Seq()), directRecordWeights.getOrElse(trustee, Seq()))
      else x._2.marginalLogLikelihood(directObs.getOrElse(trustee, Seq()), directRecordWeights.getOrElse(trustee, Seq()))
    ).toList

    // Observe direct observations of this trustee for components containing information about other trustees.
    val mixComponents: Seq[Dirichlet] = directComponent.map(x =>
      if (x._1 == trustee) x._2
      else x._2.observe(directObs.getOrElse(trustee, Seq()), Seq())
    ).toList

    // Now calculate the log weights based on each reported opinion distribution, and sum them for each component
    val repWeights: Map[Provider, Double] = (for (((w, te), model) <- repModels) yield {
      if (te == trustee) (w, trustee) -> model.logweight(priorDist)
      else (w, te) -> model.logweight(repModels.getOrElse((w, trustee), priorDist))
    }).groupBy(x => x._1._2).mapValues(x => x.values.sum)

    val weights: RowVector = (trustees zip directWeights).map(x => x._2 + repWeights.getOrElse(x._1, 0d))

    //weights = unNormalisedLogWeights - max(unNormalisedLogWeights);
    def minusMax(weights: RowVector) = weights @- weights.max
    //weights = weights - log(sum(exp(weights)));
    def minusLogSumExp(weights: RowVector) = weights @- Math.log(weights.exp.sum)
    //    def minusLogSumExp_broken(weights: RowVector) = weights @- weights.exp.log.sum
    //weights = exp(weights) ./ sum(exp(weights)); % too be sure
    def expDivSumExp(weights: RowVector) = weights.exp @/ weights.exp.sum

    //    println("--")
    val normWeights = expDivSumExp(minusLogSumExp(minusMax(weights)))
    //    val normWeights_broken = expDivSumExp(minusLogSumExp_broken(minusMax(weights)))

    // Calculate the expected utility and standard error according to each individual component.
    val expval = (normWeights @* mixComponents.map(x => x.expval())).sum

    new TrustAssessment(baseInit.context, request, expval)
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): StrategyInit = {
    val directRecords: Seq[ServiceRecord with RatingRecord] = getDirectRecords(network, context)
    val witnessRecords: Seq[ServiceRecord with RatingRecord] = getWitnessRecords(network, context)

    val trustees: Seq[Provider] = (directRecords.map(_.provider).distinct ++ witnessRecords.map(_.provider).distinct).distinct.sortBy(_.id)
    val witnesses: Seq[Client] = witnessRecords.map(_.client).distinct

    val priorDist = getDirectPrior(context)
    val directObs = getDirectObs(directRecords)

    new HabitInit(
      context,
      witnesses,trustees,
      priorDist,
      directObs,
      getDirectRecordWeights(directRecords, context),
      getRepModels(context, witnessRecords, trustees, witnesses)
    )
  }

  def getDirectPrior(context: ClientContext): Dirichlet = {
    new Dirichlet(numBins)
  }

  def getDirectRecordWeights(directRatings: Seq[ServiceRecord with RatingRecord], context: ClientContext): Map[Provider,RowVector] = {
    Map()
  }

  def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    1d
  }

  def getDirectObs(directRatings: Seq[ServiceRecord with RatingRecord]): Map[Provider, RowVector] = {
    //    directRatings.groupBy(_.provider).mapValues(_.map(x => discretizeDouble(x.rating)))
    directRatings.groupBy(_.provider).map(x => x._1 ->
      new RowVector(x._2.map(r => discretizeDouble(r.rating)))
    )
  }

  def getRepModels(context: ClientContext,
                   witnessReports: Seq[ServiceRecord with RatingRecord],
                   trustees: Seq[Provider],
                   witnesses: Seq[Client]): Map[(Client, Provider), Dirichlet] = {
    (for (w <- witnesses; t <- trustees) yield {
      (w, t) -> new Dirichlet(numBins, 1).observe(
        witnessReports.withFilter(x => x.client == w && x.provider == t).map(x => discretizeDouble(x.rating)),
        witnessReports.withFilter(x => x.client == w && x.provider == t).map(x => weightedRating(x, context))
      )
    }).toMap
  }
}





