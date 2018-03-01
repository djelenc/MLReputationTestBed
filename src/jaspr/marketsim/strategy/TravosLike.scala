package jaspr.marketsim.strategy

import jaspr.core.agent.Client
import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.StrategyInit
import jaspr.strategy.mlr.{MlrCore, MlrModel}
import weka.classifiers.Classifier

/**
  * Created by phil on 28/06/17.
  */
class TravosLike(val baseLearner: Classifier,
                 override val numBins: Int,
                 override val lower: Double,
                 override val upper: Double) extends StrategyCore with MlrCore {

  override val name: String =
    this.getClass.getSimpleName+"-"+baseLearner.getClass.getSimpleName+"-"+numBins

  setupLearner(baseLearner)

  override def compute(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val init: TravosLikeInit = baseInit.asInstanceOf[TravosLikeInit]

    val directResult = init.directModel match {
      case None =>
        0
      case Some(directModel) =>
        val directRow = makeTestRow(init, request)
        val directQuery = convertRowToInstance(directRow, directModel.attVals, directModel.train)
        makePrediction(directQuery, directModel)
    }

    val witnessResults = init.witnessModels.map(w => {
      val witnessRow = makeTestRow(init, request)
      val witnessQuery = convertRowToInstance(witnessRow, w._2.attVals, w._2.train)
      w._1 -> makePrediction(witnessQuery, w._2)
    })

    new TrustAssessment(baseInit.context, request, getCombinedOpinions(directResult, witnessResults, init.witnessWeights))
  }

  def getCombinedOpinions(direct: Double,
                          opinions: Map[Client,Double],
                          witnessWeights: Map[Client,Double]): Double = {
    direct + opinions.map(w => w._2 * witnessWeights.getOrElse(w._1, 1d)).sum
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): StrategyInit = {
    val directRecords = getDirectRecords(network, context)
    val witnessRecords = getWitnessRecords(network, context)

    if (directRecords.isEmpty) {
      val witnessModels: Map[Client, MlrModel] = makeOpinions(witnessRecords, r => r.service.request.client)
      val witnessWeights = Map[Client,Double]()
      new TravosLikeInit(context, None, witnessModels, witnessWeights)
    } else {
      val directModel: MlrModel = makeMlrsModel(directRecords, baseLearner, makeTrainRow, makeWeight)
      val witnessModels: Map[Client, MlrModel] = makeOpinions(witnessRecords, r => r.service.request.client)
      val witnessWeights = witnessModels.map(w =>
        w._1 -> this.evaluateMlrsModel(directRecords, w._2, makeTrainRow).map(x => Math.abs(x.actual()-x.predicted())).sum/directRecords.size.toDouble
      )
      new TravosLikeInit(context, Some(directModel), witnessModels, witnessWeights)
    }
  }

  val makeWeight: ServiceRecord => Double = null

  def makeOpinions[K1](records: Seq[ServiceRecord with RatingRecord],
                       grouping1: ServiceRecord with RatingRecord => K1): Map[K1,MlrModel] = {
    records.groupBy(
      grouping1
    ).map(
      rs => rs._1 -> makeMlrsModel(rs._2, baseLearner, makeTrainRow, makeWeight)
    )
  }

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



