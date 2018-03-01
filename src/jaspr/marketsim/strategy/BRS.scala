package jaspr.marketsim.strategy

import jaspr.core.agent.{Client, Provider}
import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.StrategyInit
import jaspr.utilities.BetaDistribution

/**
  * Created by phil on 18/01/2017.
  */
class BRS(override val witnessWeight: Double = 2d) extends StrategyCore with BRSCore {

  override val prior: Double = 0.5
  //  override val witnessWeight: Double = 3d

  override val name: String =
    this.getClass.getSimpleName+"-"+witnessWeight+
      (if (discountOpinions) "-discountOpinions" else "")

  override def compute(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val init = baseInit.asInstanceOf[BRSInit]

    val direct = getRequestOpinion(request, init.directBetas, new BetaDistribution(1,1)) // 1,1 for uniform
    val opinions = init.witnessBetas.values.map(
      getRequestOpinion(request, _, new BetaDistribution(0,0)) // 0,0 if the witness had no information about provider
    )

    val beta = getCombinedOpinions(direct, opinions, witnessWeight)
    val score = beta.belief + prior*beta.uncertainty()

    new TrustAssessment(init.context, request, score)
  }

  def getRequestOpinion(request: ServiceRequest,
                        aggregates: Map[Provider,BetaDistribution],
                        default: BetaDistribution = new BetaDistribution(0,0)
                       ): BetaDistribution = {
    aggregates.getOrElse(request.provider, default)
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): StrategyInit = {
    val directRecords: Seq[ServiceRecord with RatingRecord] = getDirectRecords(network, context)
    val witnessRecords: Seq[ServiceRecord with RatingRecord] = getWitnessRecords(network, context)

    val directBetas: Map[Provider,BetaDistribution] = makeDirectBetas(directRecords, context)
    val witnessBetas: Map[Client, Map[Provider, BetaDistribution]] = makeWitnessBetas(witnessRecords, context)

    val weightedWitnessBetas: Map[Client, Map[Provider, BetaDistribution]] =
      if (discountOpinions) weightWitnessBetas(witnessBetas, directBetas)
      else witnessBetas

    new BRSInit(context, directBetas, weightedWitnessBetas)
  }
}













