package jaspr.strategy.mlr

import java.text.DecimalFormat
import java.util

import jaspr.core.provenance.Record
import jaspr.utilities.{Chooser, Discretization}
import jaspr.weka.classifiers.meta.MultiRegression
import jaspr.weka.utilities.EvaluatingUtils
import weka.classifiers.bayes.NaiveBayes
import weka.classifiers.evaluation.{NominalPrediction, NumericPrediction, Prediction}
import weka.classifiers.meta.Bagging
import weka.classifiers.trees.{J48, M5P, RandomForest}
import weka.classifiers.{AbstractClassifier, Classifier}
import weka.core.{Attribute, DenseInstance, Instance, Instances}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by phil on 03/11/15.
  */
trait MlrCore extends Discretization {

  override val upper: Double = 1d
  override val lower: Double = 0d

  val classIndex: Int = 0
  lazy val discreteClass: Boolean = if (numBins <= 1) false else true

  def setupLearner(baseLearner: Classifier): Unit = {
    baseLearner match {
      case x: Bagging =>
        val j48 = new J48
        j48.setUnpruned(true)
        //      val nb = new NaiveBayes
        //      nb.setUseSupervisedDiscretization(true)
        x.setClassifier(j48)
        x.setNumExecutionSlots(1)
//        x.setBagSizePercent(50)
      case x: M5P =>
        x.setBuildRegressionTree(true)
        x.setMinNumInstances(0)
//        x.setUseUnsmoothed(true)
        x.setNumDecimalPlaces(3)
        x.setUnpruned(true)
      case x: NaiveBayes => x.setUseSupervisedDiscretization(true)
      case x: MultiRegression =>
        val bayes = new NaiveBayes
        bayes.setUseSupervisedDiscretization(true)
        x.setClassifier(bayes)
        x.setSplitAttIndex(-1)
      case x: J48 =>
        x.setUnpruned(true)
      case x: RandomForest =>
        x.setNumExecutionSlots(1)
      //      x.setNumFeatures(100)
      case _ => // do nothing
    }
  }


  def makeMlrsModel[T <: Record](records: Seq[T], baseModel: Classifier,
                                 makeTrainRow: T => Seq[Any],
                                 makeWeight: T => Double = null): MlrModel = {
    val rows = records.map(makeTrainRow)
    val weights = if (makeWeight == null) Nil else records.map(makeWeight)
    val directAttVals: Iterable[mutable.Map[Any, Double]] = List.fill(rows.head.size)(mutable.Map[Any, Double]())
    val doubleRows = convertRowsToDouble(rows, directAttVals)
    val atts = makeAtts(rows.head, directAttVals)
    val train = makeInstances(atts, doubleRows, weights)
    val directModel = AbstractClassifier.makeCopy(baseModel)
    directModel.buildClassifier(train)
    new MlrModel(directModel, train, directAttVals)
  }

  def evaluateMlrsModel[T <: Record](records: Seq[T], mlrsModel: MlrModel,
                                     makeTestRow: T => Seq[Any],
                                     makeWeight: T => Double = null): Iterable[Prediction] = {
    val rows = records.map(makeTestRow)
    val weights = if (makeWeight == null) Nil else records.map(makeWeight)
    val queries = convertRowsToInstances(rows, mlrsModel.attVals, mlrsModel.train, weights)
    if (discreteClass) queries.map(q => new NominalPrediction(q.classValue(), mlrsModel.model.distributionForInstance(q)))
    else queries.map(q => new NumericPrediction(q.classValue(), mlrsModel.model.classifyInstance(q)))
  }

  def cut[A](xs: Seq[A], n: Int): List[Seq[A]] = {
    val (quot, rem) =
      if (xs.size <= n) (1, 0)
      else (xs.size / n, xs.size % n)
    val (smaller, bigger) = xs.splitAt(xs.size - rem * (quot + 1))
    (smaller.grouped(quot) ++ bigger.grouped(quot + 1)).toList
  }

  def crossValidate[T <: Record](records: Seq[T],
                                 baseModel: Classifier,
                                 makeRow: T => Seq[Any],
                                 numFolds: Int = 5,
                                 makeweight: T => Double = null) = {
    if (records.size <= 1) {
      0d
    } else {
      val shuffled = Chooser.shuffle(records)
      val folds = cut(shuffled, numFolds)
      val preds = for (fold <- 0 until folds.size) yield {
        val trainRecords = folds.take(fold) ++ folds.drop(fold + 1)
        val testRecords = folds.get(fold)
        val model = makeMlrsModel[T](trainRecords.flatten, baseModel, makeRow)
        evaluateMlrsModel(testRecords, model, makeRow)
      }
      val aucs = preds.map(x => EvaluatingUtils.weightedAUC(new util.ArrayList(x)))
      val df = new DecimalFormat("#.##")
      println(aucs.map(df.format))
      //      EvaluatingUtils.weightedAUC(new util.ArrayList(preds.flatten))
      aucs.sum / aucs.size
    }
  }

  def makePrediction(query: Instance, model: MlrModel, discreteClass: Boolean = discreteClass, numBins: Int = numBins): Double = {
    if (discreteClass && numBins <= 2) {
      val dist = model.model.distributionForInstance(query)
      dist.zipWithIndex.map(x => x._1 * model.train.classAttribute().value(x._2).toDouble).sum
    } else if (discreteClass) {
      val dist = model.model.distributionForInstance(query)
      dist.zipWithIndex.map(x => x._1 * model.train.classAttribute().value(x._2).toDouble).sum
//      val pred = model.model.classifyInstance(query)
//      model.train.classAttribute().value(pred.toInt).toDouble
    } else {
      model.model.classifyInstance(query)
    }
  }

  def lookup[T](map: mutable.Map[T, Double], item: T): Double = {
    if (map.contains(item)) map(item)
    else {
      map(item) = map.size
      map(item)
    }
  }

  def convertRowsToDouble(rows: Iterable[Seq[Any]], attVals: Iterable[mutable.Map[Any, Double]], classIndex: Int = classIndex, discreteClass: Boolean = discreteClass, numBins: Int = numBins): Iterable[Seq[Double]] = {
    for (row <- rows) yield {
      convertRowToDouble(row, attVals, classIndex, discreteClass, numBins)
    }
  }

  def convertRowToDouble(row: Seq[Any], attVals: Iterable[mutable.Map[Any, Double]], classIndex: Int = classIndex, discreteClass: Boolean = discreteClass, numBins: Int = numBins): Seq[Double] = {
    for (((item, vals), i) <- row.zip(attVals).zipWithIndex) yield {
      item match {
        case x: Int => if (i == classIndex) x else lookup(vals, x)
        case x: Boolean => if (x && i == classIndex) 1 else if (i == classIndex) 0 else lookup(vals, x)
        case x: Double => x
        case x: String => if (i == classIndex) vals(x) else lookup(vals, x)
        case whatever => throw new Exception("Unknown type to build attrbute from. "+whatever)
      }
    }
  }

  def convertRowsToInstances(rows: Iterable[Seq[Any]], attVals: Iterable[mutable.Map[Any, Double]], dataset: Instances, weights: Iterable[Double] = Nil): Iterable[Instance] = {
    if (weights.isEmpty) rows.map(convertRowToInstance(_, attVals, dataset))
    else (rows zip weights).map(r => convertRowToInstance(r._1, attVals, dataset, r._2))
  }

  def convertRowToInstance(row: Seq[Any], attVals: Iterable[mutable.Map[Any, Double]], dataset: Instances, weight: Double = 1d): Instance = {
    val inst = new DenseInstance(dataset.numAttributes())
    inst.setDataset(dataset)
    for (((item, vals), i) <- row.zip(attVals).zipWithIndex) {
      item match {
        case x: Int =>
          if (i == classIndex) inst.setValue(i, x.toDouble)
          else inst.setValue(i, vals.getOrElse(x, weka.core.Utils.missingValue))
        case x: Boolean =>
          if (x && i == classIndex) inst.setValue(i, 1)
          else if (i ==  classIndex) inst.setValue(i, 0)
          else inst.setValue(i, vals.getOrElse(x, weka.core.Utils.missingValue))
        case x: Double => inst.setValue(i, x)
        case x: String => inst.setValue(i, vals.getOrElse(x, weka.core.Utils.missingValue))
        case whatever => throw new Exception("Unknown type to build attrbute from. "+item.getClass+":"+item)
      }
    }
    inst
  }

  def makeAtts(row: Seq[Any], attVals: Iterable[mutable.Map[Any, Double]], classIndex: Int = classIndex, discreteClass: Boolean = discreteClass): Iterable[Attribute] = {
    for (((item, vals), i) <- row.zip(attVals).zipWithIndex) yield {
      if (i == classIndex) if (discreteClass) new Attribute("target", discVals) else new Attribute("target")
      else {
        item match {
          case x: Double => new Attribute(i.toString)
          case x: Int => new Attribute(i.toString, vals.toList.sortBy(_._2).map(_._1.toString))
          case x: String => new Attribute(i.toString, vals.toList.sortBy(_._2).map(_._1.toString))
          case whatever => throw new Exception("Unknown type to build attrbute from. "+item.getClass+":"+item)
        }
      }
    }
  }

  def makeInstances(atts: Iterable[Attribute], doubleRows: Iterable[Seq[Double]], weights: Iterable[Double] = Nil): Instances = {
    val directTrain: Instances = new Instances("data", new util.ArrayList(atts), doubleRows.size)
    directTrain.setClassIndex(classIndex)
    if (weights.isEmpty) doubleRows.foreach(r => directTrain.add(new DenseInstance(1d, r.toArray)))
    else (doubleRows zip weights).foreach(r => directTrain.add(new DenseInstance(r._2, r._1.toArray)))
    directTrain
  }

}
