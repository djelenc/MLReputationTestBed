package jaspr.marketsim.strategy

import jaspr.core.agent.Client
import jaspr.core.provenance.{RatingRecord, Record, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.StrategyInit
import jaspr.strategy.mlr.{MlrCore, MlrModel}
import jaspr.utilities.Chooser
import weka.classifiers.functions.LinearRegression
import weka.classifiers.{AbstractClassifier, Classifier}

import scala.collection.mutable

/**
  * Created by phil on 20/01/17.
  */
class HabitLike(val witnessWeight: Double = 2d,
                val baseLearner: Classifier,
                override val numBins: Int,
                override val lower: Double,
                override val upper: Double) extends StrategyCore with MlrCore {

  override val name: String =
    this.getClass.getSimpleName+"-"+baseLearner.getClass.getSimpleName+"-"+numBins+"-"+witnessWeight

  setupLearner(baseLearner)


  override def compute(baseInit: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val init = baseInit.asInstanceOf[HabitLikeInit]

    val directResult = init.directModel match {
      case None =>
        0d
      case Some(directModel) =>
        val directRow = makeTestRow(init, request)
        val directQuery = convertRowToInstance(directRow, directModel.attVals, directModel.train)
        makePrediction(directQuery, directModel)
    }

    val witnessResults = init.witnessModels.map(wm => {
      val witnessRow = makeTestRow(init, request)
      val witnessQuery = convertRowToInstance(witnessRow, wm._2.attVals, wm._2.train)
      val raw = makePrediction(witnessQuery, wm._2)
      val translationRow = 0 :: raw :: Nil
      val x = init.translationModels.get(wm._1) match {
        case None =>
          raw
        case Some(tm) =>
          val inst = convertRowToInstance(translationRow, tm.attVals, tm.train)
          makePrediction(inst, tm, discreteClass = translationDiscrete)
      }
//      println(directResult, raw, x)
      x
    })

    if (init.directModel.isEmpty && witnessResults.isEmpty) {
      new TrustAssessment(baseInit.context, request, Chooser.randomDouble(0,1))
    } else {
      new TrustAssessment(baseInit.context, request, getCombinedOpinions(directResult, witnessResults, witnessWeight))
    }
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): StrategyInit = {
    val directRecords = getDirectRecords(network, context)
    val witnessRecords = getWitnessRecords(network, context)

    if (directRecords.isEmpty) {
      val witnessModels: Map[Client, MlrModel] = makeOpinions(witnessRecords, r => r.client)
      new HabitLikeInit(context, None, witnessModels, Map())
    } else {
      val directModel: MlrModel = makeMlrsModel(directRecords, baseLearner, makeTrainRow, makeWeight)
      val witnessModels: Map[Client, MlrModel] = makeOpinions(witnessRecords, r => r.client)
//      print(context.client+ ": " )
      val reinterpretationModels: Map[Client, MlrModel] =
        makeTranslationModels(directRecords, witnessRecords, directModel, witnessModels, r => r.client)
//      println()
      new HabitLikeInit(context, Some(directModel), witnessModels, reinterpretationModels)
    }
  }

  val makeWeight: ServiceRecord => Double = null


  def getCombinedOpinions(direct: Double,
                          opinions: Iterable[Double],
                          witnessWeight: Double): Double = {
    if (witnessWeight == 0) direct
    else if (witnessWeight == 1) opinions.sum/(opinions.size+1)
    else if (witnessWeight == 2) (direct + opinions.sum)/(opinions.size+1)
    else getCombinedOpinions(direct * (1-witnessWeight), opinions.map(_ * witnessWeight), witnessWeight = 2)
  }



  val translationDiscrete: Boolean = false
  val baseTranslationLearner = new LinearRegression
  def makeTranslationModel[T <: Record](directRecords: Seq[T],
                                        witnessRecords: Seq[T],
                                        directModel: MlrModel,
                                        witnessModel: MlrModel,
                                        otherWitnessModels: Iterable[MlrModel],
                                        makeTranslationRow: (T,MlrModel,MlrModel) => Seq[Any]): MlrModel = {
    val reinterpretationRows: Seq[Seq[Any]] =
      directRecords.map(record => makeTranslationRow(record, witnessModel, directModel)) ++
        witnessRecords.map(record => makeTranslationRow(record, witnessModel, directModel)) //++
//        directRecords.flatMap(record =>
//          otherWitnessModels.map(owm => makeTranslationRow(record, witnessModel, owm))
//        ) ++
//        witnessRecords.flatMap(record =>
//          otherWitnessModels.map(owm => makeTranslationRow(record, witnessModel, owm))
//        )
    val reinterpretationAttVals: Iterable[mutable.Map[Any, Double]] = List.fill(reinterpretationRows.head.size)(mutable.Map[Any, Double]())
    val doubleRows = convertRowsToDouble(reinterpretationRows, reinterpretationAttVals, classIndex, discreteClass = translationDiscrete)
    val atts = makeAtts(reinterpretationRows.head, reinterpretationAttVals, classIndex, discreteClass = translationDiscrete)
    val reinterpretationTrain = makeInstances(atts, doubleRows)
    reinterpretationTrain.setClassIndex(classIndex)
    val reinterpretationModel = AbstractClassifier.makeCopy(baseTranslationLearner)
//    print(reinterpretationTrain.numInstances()+" ")
    reinterpretationModel.buildClassifier(reinterpretationTrain)
    new MlrModel(reinterpretationModel, reinterpretationTrain, reinterpretationAttVals)
  }

  def makeTranslationRow(record: Record with ServiceRecord with RatingRecord,
                         fromModel: MlrModel,
                         toModel: MlrModel): Seq[Any] = {
    val fromRow = makeTrainRow(record)
    val fromQuery = convertRowToInstance(fromRow, fromModel.attVals, fromModel.train)
    val toRow = makeTrainRow(record)
    val toQuery = convertRowToInstance(toRow, toModel.attVals, toModel.train)
    (if (translationDiscrete) makePrediction(toQuery, toModel, discreteClass = false) // So we translate to a full discrete value (not the probability distribution inferred one.
    else makePrediction(toQuery, toModel)) ::
      makePrediction(fromQuery, fromModel) ::
      Nil
  }

  def makeTranslationModels[K1](directRecords: Seq[ServiceRecord with RatingRecord],
                                witnessRecords: Seq[ServiceRecord with RatingRecord],
                                directModel: MlrModel,
                                witnessModels: Map[K1,MlrModel],
                                grouping1: ServiceRecord with RatingRecord => K1): Map[K1,MlrModel] = {
    witnessRecords.groupBy(
      grouping1
    ).map(
      wrs => wrs._1 -> makeTranslationModel(
        directRecords, witnessRecords, directModel, witnessModels(wrs._1), witnessModels.filterNot(_._1 == wrs._1).values, makeTranslationRow
      )
    )
  }

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
      record.provider.name ::
      Nil
  }

  def label(record: RatingRecord): Any = {
    if (numBins < 1) record.rating
    else if (numBins == 2) record.success
    else discretizeInt(record.rating)
  }

}



