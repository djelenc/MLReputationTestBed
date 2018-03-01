package jaspr.marketsim.strategy

import jaspr.core.agent.{Client, Provider}
import jaspr.core.service.ClientContext
import jaspr.core.strategy.StrategyInit
import jaspr.strategy.mlr.MlrModel
import jaspr.utilities.matrix.{Matrix, RowVector}
import jaspr.utilities.{Aggregate, BetaDistribution, Dirichlet}

/**
  * Created by phil on 18/01/2017.
  */

abstract class MarketStrategyInit(context: ClientContext) extends StrategyInit(context) {
  def trustees: Seq[Provider]
}

class BRSInit(context: ClientContext,
              val directBetas: Map[Provider,BetaDistribution],
              val witnessBetas: Map[Client,Map[Provider,BetaDistribution]]
             ) extends MarketStrategyInit(context) {
  override val trustees: Seq[Provider] = (directBetas.keys ++ witnessBetas.flatMap(_._2.keys)).toSeq.distinct
}

class FireInit(context: ClientContext,
               val directAggregate: Map[Provider,Aggregate],
               val witnessAggregate: Map[Client,Map[Provider,Aggregate]]
              ) extends MarketStrategyInit(context) {
  override val trustees: Seq[Provider] = (directAggregate.keys ++ witnessAggregate.flatMap(_._2.keys)).toSeq.distinct
}

class TravosInit(context: ClientContext,
                 directBetas: Map[Provider,BetaDistribution],
                 witnessBetas: Map[Client,Map[Provider,BetaDistribution]],
                 val observations: Map[Client, Seq[(Boolean, BetaDistribution)]]
                ) extends BRSInit(context, directBetas, witnessBetas) {
}

class BladeInit(context: ClientContext,
                override val trustees: Seq[Provider],
                val witnesses: Seq[Client],
                val dirModelPrior: Dirichlet,
                val repModelPrior: Dirichlet,
                val directModels: Map[Provider, Dirichlet],
                val repModels: Map[(Client, Provider), Dirichlet],
                val repMatrix: Map[(Client, Provider), Matrix]
               ) extends MarketStrategyInit(context)

class HabitInit(context: ClientContext,
                val witnesses: Seq[Client],
                val trustees: Seq[Provider],
                val priorDist: Dirichlet,
                val directObs: Map[Provider, RowVector],
                val directRecordWeights: Map[Provider, RowVector],
                val repModels: Map[(Client, Provider), Dirichlet]
               ) extends MarketStrategyInit(context)

class BRSLikeInit(context: ClientContext,
                   val trustModel: Option[MlrModel]
                  ) extends StrategyInit(context)

class FireLikeInit(context: ClientContext,
                   val directModel: Option[MlrModel],
                   val witnessModels: Map[Client,MlrModel]
                  ) extends StrategyInit(context)

class TravosLikeInit(context: ClientContext,
                   val directModel: Option[MlrModel],
                   val witnessModels: Map[Client,MlrModel],
                   val witnessWeights: Map[Client, Double]
                  ) extends StrategyInit(context)

class BurnettInit(context: ClientContext,
                  directBetas: Map[Provider,BetaDistribution],
                  witnessBetas: Map[Client,Map[Provider,BetaDistribution]],
                  val directStereotypeModel: Option[MlrModel],
                  val witnessStereotypeModels: Map[Client,MlrModel],
                  val directStereotypeWeight: Double,
                  val witnessStereotypeWeights: Map[Client,Double]
                 ) extends BRSInit(context, directBetas, witnessBetas)

class HabitLikeInit(context: ClientContext,
                   val directModel: Option[MlrModel],
                   val witnessModels: Map[Client,MlrModel],
                   val translationModels: Map[Client,MlrModel]
                  ) extends StrategyInit(context)