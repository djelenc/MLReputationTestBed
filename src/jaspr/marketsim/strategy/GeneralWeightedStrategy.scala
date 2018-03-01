package jaspr.marketsim.strategy

import jaspr.core.agent.Provider
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.{Strategy, StrategyInit}

/**
  * Created by phil on 13/06/17.
  */
class GeneralWeightedStrategy(val baseStrategyStr: String = "jaspr.marketsim.strategy.BRS[2d]") extends StrategyCore {

  val baseStrategy: StrategyCore = Strategy.forName(baseStrategyStr.replace("[","(").replace("]",")").replace(":",";")).asInstanceOf[StrategyCore]
  override val name: String = this.getClass.getSimpleName + "-" + baseStrategy.name

  class MultiInitStrategy(val baseInit: MarketStrategyInit,
                          val trustAssessments: Map[ServiceRequest,TrustAssessment]
                          ) extends MarketStrategyInit(baseInit.context) {
    val trustees: Seq[Provider] = baseInit.trustees
  }


  override def compute(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val init = baseInit.asInstanceOf[MultiInitStrategy]
    val tv = getTrustValue(init.trustAssessments, request)
//    print(init.trustAssessments(request).trustValue+" ==== ")
    val ret = init.trustAssessments(request).copy(_trustValue = tv)
//    println(ret.trustValue)
    ret
  }

  // This is a hack to ensure that the returned trust assessment has the same type as base strategy
  // - we can't assume that trust assessments will have a copy method in general
  override def computeAssessment(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val requestScores: Seq[TrustAssessment] = request.flatten().map(x => compute(baseInit, request))
    requestScores.head.copy(_context = baseInit.context, _request = request, _trustValue = requestScores.map(_.trustValue).sum)
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): MarketStrategyInit = {
    val baseInit = baseStrategy.initStrategy(network, context, requests).asInstanceOf[MarketStrategyInit]

    val trustAssessments: Map[ServiceRequest,TrustAssessment] = allRequests(baseInit, network, context, requests).map(req =>
      req -> baseStrategy.compute(baseInit, req)
    ).toMap
    new MultiInitStrategy(baseInit, trustAssessments)
  }

  def allRequests(baseInit: MarketStrategyInit,
                  network: Network,
                  context: ClientContext,
                  requests: Seq[ServiceRequest]
                 ): Seq[ServiceRequest] = {
    requests
  }

  def getTrustValue(trustAssessments: Map[ServiceRequest,TrustAssessment], request: ServiceRequest): Double = {
    trustAssessments(request).trustValue
  }
}

class StereotypeWeightedStrategy(baseStrategyStr: String
                               ) extends GeneralWeightedStrategy(baseStrategyStr) with StereotypeCore {

  override def allRequests(baseInit: MarketStrategyInit,
                  network: Network,
                  context: ClientContext,
                  requests: Seq[ServiceRequest]
                 ): Seq[ServiceRequest] = {
    (requests ++ baseInit.trustees.map(te =>
      new ServiceRequest(context.client, te, context.round, 0, context.payload, context.market)
    )).groupBy(_.provider).map(_._2.head).toSeq
  }

  override def getTrustValue(trustAssessments: Map[ServiceRequest,TrustAssessment], request: ServiceRequest): Double = {
    val weights = trustAssessments.map(x => x._1 -> stereotypeMatch(x._1.provider, request.provider))
//    println(weights.values.sum)
    trustAssessments.map(x => x._2.trustValue * weights(x._1)).sum / weights.values.sum
//    trustAssessments.map(x => if (stereotypeMatches(x._1.provider, request.provider)) x._2.trustValue else 0d).sum / trustAssessments.size.toDouble
  }
}

