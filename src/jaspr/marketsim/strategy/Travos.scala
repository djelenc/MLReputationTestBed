package jaspr.marketsim.strategy
import jaspr.core.agent.Client
import jaspr.core.provenance.{RatingRecord, ServiceRecord, TrustAssessmentRecord}
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.StrategyInit
import jaspr.utilities.BetaDistribution



/**
  * Created by phil on 28/06/17.
  */
class Travos extends BRS {
  override val prior: Double = 0.5
  override val witnessWeight: Double = 2d

  val confidenceThreshold = 1
  val numWeightingBins = 5
  val eps: Double = 0.1

  trait BetaOpinions {
    val opinions: List[(Client, BetaDistribution)]
  }

  class TravosTrustAssessment(context: ClientContext,
                              request: ServiceRequest,
                              trustValue: Double,
                              val opinions: Map[Client, BetaDistribution]
                             ) extends TrustAssessment(context, request, trustValue) {
    override def copy(_context: ClientContext = context,
             _request: ServiceRequest = request,
             _trustValue: Double = trustValue): TravosTrustAssessment = {
      new TravosTrustAssessment(_context, _request, _trustValue, opinions)
    }
  }

  override def computeAssessment(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val requestScores: Seq[TravosTrustAssessment] = request.flatten().map(x => compute(baseInit, request))
    new TravosTrustAssessment(baseInit.context, request, requestScores.map(_.trustValue).sum, requestScores.flatMap(_.opinions).toMap)
  }

  override def compute(baseInit: StrategyInit, request: ServiceRequest): TravosTrustAssessment = {
    val init = baseInit.asInstanceOf[TravosInit]

    val direct = getRequestOpinion(request, init.directBetas, new BetaDistribution(1,1)) // 1,1 for uniform
    val directConfidence: Double =
      direct.integrate(direct.expected - eps, direct.expected + eps)

    val opinions: Map[Client,BetaDistribution] = init.witnessBetas.map(x =>
      x._1 -> getRequestOpinion(request, x._2, new BetaDistribution(1,1)) // 0,0 if the witness had no information about provider
    )

    // Observations in client's provenance about witness opinion providers
    val observations: Map[Client, Seq[(Boolean, BetaDistribution)]] =
      if (directConfidence < confidenceThreshold) // if interaction trust confidence is low, use witness opinions
        init.observations
      else
        Map()

    // weight the witness opinions by their expected accuracy
    val weightedOpinions: Iterable[BetaDistribution] = opinions.map(x =>
      weightOpinion(x._2, observations.getOrElse(x._1, List()), numWeightingBins
    ))

    val beta = getCombinedOpinions(direct, weightedOpinions, witnessWeight)
    val score = beta.belief + prior*beta.uncertainty()

    new TravosTrustAssessment(init.context, request, score, opinions)
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): StrategyInit = {
    val brsInit = super.initStrategy(network, context, requests).asInstanceOf[BRSInit]

    val directRecords: Seq[ServiceRecord with RatingRecord with TrustAssessmentRecord] = getDirectRecords(network, context)

    new TravosInit(
      context,
      brsInit.directBetas,
      brsInit.witnessBetas,
      getObservations(directRecords)
    )
  }

  def weightOpinion(opinion: BetaDistribution,
                    observations: Seq[(Boolean, BetaDistribution)],
                    numWeightingBins: Int): BetaDistribution = {
    // Get the bin of the opinion raw expected value
    val bin = opinion.getBin(numWeightingBins)
    val similarObs: Seq[Boolean] = observations.filter(x =>
      x._2.isInBin(bin, numWeightingBins)
    ).map(x => x._1)
    val opObsArea: Double = makeBetaDistribution(similarObs).integrate(
      bin, // Low bin
      bin + (1d / numWeightingBins) // High bin
    )
    opinion.getWeighted(opObsArea)
  }

  def getObservations(directRecords: Seq[RatingRecord with TrustAssessmentRecord]): Map[Client, Seq[(Boolean, BetaDistribution)]] = {
    directRecords.flatMap(x =>
      x.assessment.asInstanceOf[TravosTrustAssessment].opinions.map(obsOp =>
        obsOp._1 -> (x.success, obsOp._2)
      )
    ).groupBy(_._1).mapValues(_.map(_._2))
  }
}







