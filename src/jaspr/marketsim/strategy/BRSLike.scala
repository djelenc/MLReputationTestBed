package jaspr.marketsim.strategy

import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.StrategyInit
import jaspr.strategy.mlr.{MlrCore, MlrModel}
import jaspr.utilities.Chooser
import weka.classifiers.Classifier

/**
  * Created by phil on 19/01/17.
  */
class BRSLike(val baseLearner: Classifier,
              override val numBins: Int,
              override val lower: Double,
              override val upper: Double) extends StrategyCore with MlrCore {

  override val name: String =
    this.getClass.getSimpleName+"-"+baseLearner.getClass.getSimpleName+"-"+numBins

  setupLearner(baseLearner)

  override def compute(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val init: BRSLikeInit = baseInit.asInstanceOf[BRSLikeInit]
    init.trustModel match {
      case None => new TrustAssessment(baseInit.context, request, Chooser.randomDouble(0,1))
      case Some(trustModel) =>
        val row = makeTestRow(init, request)
        val query = convertRowToInstance(row, trustModel.attVals, trustModel.train)
        val result = makePrediction(query, trustModel)

        new TrustAssessment(baseInit.context, request, result)
    }
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): StrategyInit = {
    val directRecords = getDirectRecords(network, context)
    val witnessRecords = getWitnessRecords(network, context)

    if (directRecords.isEmpty && witnessRecords.isEmpty) {
      new BRSLikeInit(context, None)
    } else {
      val trustModel: MlrModel = makeMlrsModel(directRecords ++ witnessRecords, baseLearner, makeTrainRow, makeWeight)

      new BRSLikeInit(context, Some(trustModel))
    }
  }

  val makeWeight: ServiceRecord => Double = null

  def makeTestRow(init: StrategyInit, request: ServiceRequest): Seq[Any] = {
    0d ::
      request.provider.name ::
      Nil
  }

  def makeTrainRow(record: ServiceRecord with RatingRecord): Seq[Any] = {
    label(record) ::
      record.service.request.provider.name ::
      Nil
  }

  def label(record: RatingRecord): Any = {
    if (numBins < 1) record.rating
    else if (numBins == 2) record.success
    else discretizeInt(record.rating)
  }
}
