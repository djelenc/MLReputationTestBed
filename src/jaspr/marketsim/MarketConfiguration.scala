package jaspr.marketsim

import jaspr.core.agent.{FixedProperty, _}
import jaspr.core.service.{ClientContext, ServiceRequest}
import jaspr.core.simulation._
import jaspr.core.strategy.Strategy
import jaspr.marketsim.agent._
import jaspr.utilities.Chooser

import scala.collection.immutable.SortedMap

/**
  * Created by phil on 18/01/17.
  */

object MarketMultiConfiguration extends App {

  val parser = new scopt.OptionParser[MarketMultiConfiguration]("SellerConfiguration") {
    opt[Seq[String]]("strategy") required() action { (x, c) => c.copy(strategies = x) }
    opt[Int]("numSimulations") required() action { (x, c) => c.copy(numSimulations = x) }
    opt[Int]("numRounds") required() action { (x, c) => c.copy(numRounds = x) }
    opt[Int]("numTrustees") required() action { (x, c) => c.copy(numTrustees = x) }
    opt[Int]("numTrustors") required() action { (x, c) => c.copy(numTrustors = x) }
    opt[Double]("trusteesAvailable") required() action { (x, c) => c.copy(trusteesAvailable = x) }
    opt[Double]("advisorsAvailable") required() action { (x, c) => c.copy(advisorsAvailable = x) }
    opt[Double]("trusteeLeaveLikelihood") required() action { (x, c) => c.copy(trusteeLeaveLikelihood = x) }
    opt[Double]("trustorLeaveLikelihood") required() action { (x, c) => c.copy(trustorLeaveLikelihood = x) }
    opt[Double]("stereotypeFeatureNoise") required() action { (x, c) => c.copy(stereotypeFeatureNoise = x) }
    opt[Double]("contextFeatureNoise") required() action { (x, c) => c.copy(contextFeatureNoise = x) }
    opt[Int]("numPreferences") required() action { (x, c) => c.copy(numPreferences = x) }
    opt[Int]("numContexts") required() action { (x, c) => c.copy(numContexts = x) }
//    opt[Double]("honestLikelihood") required() action { (x, c) => c.copy(honestLikelihood = x) }
    opt[Double]("negationLikelihood") required() action { (x, c) => c.copy(negationLikelihood = x) }
    opt[Double]("slanderLikelihood") required() action { (x, c) => c.copy(slanderLikelihood = x) }
    opt[Double]("randomLikelihood") required() action { (x, c) => c.copy(randomLikelihood = x) }
    opt[Double]("freakEventLikelihood") required() action { (x, c) => c.copy(freakEventLikelihood = x) }
  }

//  val classifierStr = "weka.classifiers.bayes.NaiveBayes;5;-2d;2d"
  val classifierStr = "weka.classifiers.trees.RandomForest;2;-2d;2d"
//  val classifierStr = "weka.classifiers.meta.Bagging;5"
//  val classifierStr = "weka.classifiers.trees.J48;2"
  val contClassifierStr = "weka.classifiers.trees.M5P;0"
//  val contCclassifier = weka.classifiers.functions.l
  val argsplt =
    if (args.length == 0) {
      ("--strategy " +
//        "jaspr.marketsim.strategy.DiscreteFire(0d;2;-1d;1d)," +
//        "jaspr.marketsim.strategy.Fire(0d)," +
//        "jaspr.marketsim.strategy.BinaryFireContext(2d)," +
//        "jaspr.marketsim.strategy.BinaryFire(2d)," +
//        "jaspr.marketsim.strategy.BRSStereotype(2d)," +
//        "jaspr.marketsim.strategy.BRS(2d)," +
//        "jaspr.marketsim.strategy.FireLike(2d;"+classifierStr+")," +
//        "jaspr.marketsim.strategy.FireContextLike(2d;"+classifierStr+")," +
//        "jaspr.marketsim.strategy.Habit(2;-1d;1d)," +
//        "jaspr.marketsim.strategy.HabitContext(2;-1d;1d)," +
//        "jaspr.marketsim.strategy.HabitContextFilter(2;-1d;1d)," +
//        "jaspr.marketsim.strategy.Blade(2;-1d;1d)," +
//        "jaspr.marketsim.strategy.BladeContext(2;-1d;1d)," +
//        "jaspr.marketsim.strategy.BladeContextFilter(2;-1d;1d)," +
//        "jaspr.marketsim.strategy.StereotypeWeightedStrategy(jaspr.marketsim.strategy.BRS[2d])," +
//        "jaspr.marketsim.strategy.BladeStereotypeMitigation(2;-2d;2d)," +
//        "jaspr.marketsim.strategy.BladeStereotypeContext(2;-2d;2d)," +
//        "jaspr.marketsim.strategy.BRSStereotype(2d)," +
//        "jaspr.marketsim.strategy.BRS(2d)," +
//        "jaspr.marketsim.strategy.BRSStereotypeLike("+classifierStr+")," +
//        "jaspr.marketsim.strategy.BRSLike("+classifierStr+")," +
        "jaspr.strategy.NoStrategy," +
        " --numSimulations 5 " +
        "--numRounds 100 " +
        "--numTrustees 100 " +
        "--numTrustors 10 " +
        "--trusteesAvailable 10 " +
        "--advisorsAvailable 10 " +
        "--trusteeLeaveLikelihood 0.05 " +
        "--trustorLeaveLikelihood 0.05 " +
        "--freakEventLikelihood 0.0 " +
        "--stereotypeFeatureNoise 0.0 " +
        "--contextFeatureNoise 0.0 " +
        "--numContexts 1 " +
        "--numPreferences 10 " +
//        "--honestLikelihood 1d " +
        "--slanderLikelihood 0.5d " +
        "--negationLikelihood 0d " +
        "--randomLikelihood 0d" +
        "").split(" ")
    } else args

  println(argsplt.toList mkString("[", " ", "]"))

  parser.parse(argsplt, MarketMultiConfiguration()) match {
    case Some(x) =>
      val results = Simulation(x)
      results.printChange(0, -1, _.recordsStored)
    case None =>
  }
}



case class MarketMultiConfiguration(strategies: Seq[String] = Nil,
                                  override val numSimulations: Int = 1,
                                  numRounds: Int = 50,
                                  numTrustees: Int = 100,
                                  numTrustors: Int = 10,
                                  trusteesAvailable: Double = 0.1,
                                  advisorsAvailable: Double = 1,
                                  trusteeLeaveLikelihood: Double = 0.05,
                                  trustorLeaveLikelihood: Double = 0.05,
                                  freakEventLikelihood: Double = 0.0,
                                  stereotypeFeatureNoise: Double = 0d,
                                  contextFeatureNoise: Double = 0d,
                                  numPreferences: Int = 1,
                                  numContexts: Int = 1,
                                  honestLikelihood: Double = 0d,
                                  negationLikelihood: Double = 0d,
                                  slanderLikelihood: Double = 0d,
                                  randomLikelihood: Double = 0d
                                 ) extends MultiConfiguration {

  override val directComparison = true

  override val resultStart: Int = 0
  override val resultEnd: Int = -1
  //  override val _seed = 1

  override lazy val configs: Seq[Configuration] =
    strategies.map(x => {
      new MarketConfiguration(
        _strategy = Strategy.forName(x),
        numRounds = numRounds,
        numTrustees = numTrustees,
        numTrustors = numTrustors,
        trusteesAvailable = trusteesAvailable,
        advisorsAvailable = advisorsAvailable,
        trusteeLeaveLikelihood = trusteeLeaveLikelihood,
        trustorLeaveLikelihood = trustorLeaveLikelihood,
        freakEventLikelihood = freakEventLikelihood,
        stereotypeFeatureNoise = stereotypeFeatureNoise,
        contextFeatureNoise = contextFeatureNoise,
        numPreferences = numPreferences,
        numContexts = numContexts,
        honestLikelihood = honestLikelihood,
        negationLikelihood = negationLikelihood,
        slanderLikelihood = slanderLikelihood,
        randomLikelihood = randomLikelihood
      )
    })
}



class MarketConfiguration(val _strategy: Strategy,
                          override val numRounds: Int,
                          numTrustees: Int,
                          numTrustors: Int,
                          val trusteesAvailable: Double,
                          val advisorsAvailable: Double,
                          val trusteeLeaveLikelihood: Double,
                          val trustorLeaveLikelihood: Double,
                          val freakEventLikelihood: Double,
                          val stereotypeFeatureNoise: Double,
                          val contextFeatureNoise: Double,
                          val numPreferences: Double,
                          val numContexts: Int,
                          honestLikelihood: Double,
                          negationLikelihood: Double,
                          slanderLikelihood: Double,
                          randomLikelihood: Double
                          ) extends Configuration {

  override def toString: String = _strategy.name

  override def newSimulation(): Simulation = {
    resetSimCapabilities()
    new MarketSimulation(this)
  }

  override def strategy(agent: Client): Strategy = _strategy

  val numClients: Int = numTrustors
  val numProviders: Int = numTrustees
  val trustorParticipation: Double = 1
  val memoryLimit: Int = numRounds

  override val numAgents: Int = numClients + numProviders

  def clientContext(client: Trustor, round: Int): ClientContext = {
    val simcap = Chooser.choose(simCapabilities)
    val clientcap = simcap.copy(
      properties = client.preferences.map(x => x._1 -> x._2.sample),
//      adverts = simcap.adverts.map(x =>
//        if (Chooser.randomBoolean(contextFeatureNoise)) {
//          x._1 -> FixedProperty(x._1, Chooser.randomBoolean(0.5))
//        } else {
//          x
//        }
//      )
      adverts = simcap.adverts.map(x => {
        val v = x._2.booleanValue
        x._1 -> FixedProperty(x.toString, Chooser.ifHappens(contextFeatureNoise)(Chooser.randomBoolean(0.5))(v))
      })
    )
    new ClientContext(client, round, clientcap, new MarketMarket)
  }

  def request(context: ClientContext, trustee: Trustee): ServiceRequest = {
    new ServiceRequest(
      context.client, trustee, context.round, 0, context.payload, context.market
    )
  }

  def witnessModel(witness: Witness, network: Network): WitnessModel = {
    val honest = 1d - (negationLikelihood + randomLikelihood + slanderLikelihood)
    if (Chooser.randomBoolean(honest)) {
      new HonestWitnessModel
    } else {
      Chooser.choose(
          new NegationWitnessModel ::
            new SlanderWitnessModel(Chooser.sample(network.providers, numProviders / 2)) ::
            new RandomWitnessModel ::
            Nil,
          negationLikelihood ::
            slanderLikelihood ::
            randomLikelihood ::
            Nil
      )
    }
  }

  def simCapabilities: Seq[MarketPayload] = _simCapabilities
  // Services that exist in the simulation
  private var _simCapabilities: Seq[MarketPayload] = Nil //set in newSimulation(..)
  private def resetSimCapabilities() = {

    val simcaps =
//      (1 to numContexts).map(x => {
      Range.Double.inclusive(-0.5,0.5,0.1).map(x => {
        val properties = GaussianProperty("a", x, 1) :: Nil
        val ads: SortedMap[String,Property] = properties.head match {
          case GaussianProperty(_,-0.5,_) => (1 :: 2 :: 3 :: 4 :: 5 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,-0.4,_) => (2 :: 3 :: 4 :: 5 :: 6 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,-0.3,_) => (3 :: 4 :: 5 :: 6 :: 7 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,-0.2,_) => (4 :: 5 :: 6 :: 7 :: 8 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,-0.1,_) => (5 :: 6 :: 7 :: 8 :: 9 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,0.0,_) => (6 :: 7 :: 8 :: 9 :: 10 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,0.1,_) => (7 :: 8 :: 9 :: 10 :: 11 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,0.2,_) => (8 :: 9 :: 10 :: 11 :: 12 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,0.3,_) => (9 :: 10 :: 11 :: 12 :: 13 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,0.4,_) => (10 :: 11 :: 12 :: 13 :: 14 :: Nil).map(x => FixedProperty(x.toString, true))
          case GaussianProperty(_,0.5,_) => (11 :: 12 :: 13 :: 14 :: 15 :: Nil).map(x => FixedProperty(x.toString, true))
        }
        val fullAds: SortedMap[String,Property] = (1 to 15).map(x => {
          //          if (Chooser.randomBoolean(contextFeatureNoise)) {
          //            FixedProperty(x.toString, Chooser.randomBoolean(0.5))
          //          } else
          //          if (ads.contains(x.toString)) {
          //            ads(x.toString)
          //          } else {
          //            FixedProperty(x.toString, false)
          //          }
          val v =
            if (ads.contains(x.toString)) ads(x.toString).booleanValue
            else false
          FixedProperty(x.toString, v)
        }).toList
        new MarketPayload(x.toString, properties, fullAds)
      })
    _simCapabilities = Chooser.sample(simcaps, numContexts)
  }

  /*al ads: SortedMap[String,Property] = agent.properties.head._2 match {
    case GaussianProperty(_,0.9,_) => FixedProperty("1", true) :: FixedProperty("6", true) :: Nil
    case GaussianProperty(_,0.6,_) => FixedProperty("2", true) :: FixedProperty("4", true) :: Nil
    case GaussianProperty(_,0.4,_) => FixedProperty("3", true) :: FixedProperty("4", true) :: Nil
    case GaussianProperty(_,0.3,_) => FixedProperty("2", true) :: FixedProperty("3", true) :: FixedProperty("5", true) :: Nil
    case GaussianProperty(_,0.5,_) => FixedProperty("2", true) :: FixedProperty("3", true) :: FixedProperty("6", true) :: Nil
  }*/
  def adverts(agent: Trustee): SortedMap[String, Property] = {
    val ads: SortedMap[String,Property] = agent.properties.head._2 match {
    case GaussianProperty(_,0.0,_) => (1 :: 2 :: 3 :: 4 :: 5 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.1,_) => (2 :: 3 :: 4 :: 5 :: 6 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.2,_) => (3 :: 4 :: 5 :: 6 :: 7 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.3,_) => (4 :: 5 :: 6 :: 7 :: 8 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.4,_) => (5 :: 6 :: 7 :: 8 :: 9 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.5,_) => (6 :: 7 :: 8 :: 9 :: 10 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.6,_) => (7 :: 8 :: 9 :: 10 :: 11 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.7,_) => (8 :: 9 :: 10 :: 11 :: 12 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.8,_) => (9 :: 10 :: 11 :: 12 :: 13 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,0.9,_) => (10 :: 11 :: 12 :: 13 :: 14 :: Nil).map(x => FixedProperty(x.toString, true))
    case GaussianProperty(_,1.0,_) => (11 :: 12 :: 13 :: 14 :: 15 :: Nil).map(x => FixedProperty(x.toString, true))
  }
//    val ads: SortedMap[String,Property] = agent.properties.head._2 match {
//      case GaussianProperty(_,0.0,_) => (1 :: 6 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.1,_) => (1 :: 6 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.2,_) => (2 :: 4 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.3,_) => (2 :: 4 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.4,_) => (3 :: 4 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.5,_) => (3 :: 4 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.6,_) => (2 :: 3 :: 5 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.7,_) => (2 :: 3 :: 5 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.8,_) => (2 :: 3 :: 6 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,0.9,_) => (2 :: 3 :: 6 :: Nil).map(x => FixedProperty(x.toString, true))
//      case GaussianProperty(_,1.0,_) => (2 :: 4 :: 6 :: Nil).map(x => FixedProperty(x.toString, true))
//    }
    val fullAds: SortedMap[String,Property] = (1 to 15).map(x => {
  //      if (Chooser.randomBoolean(stereotypeFeatureNoise)) {
  //        FixedProperty(x.toString, Chooser.randomBoolean(0.5))
  //      } else if (ads.contains(x.toString)) {
  //        ads(x.toString)
  //      } else {
  //        FixedProperty(x.toString, false)
  //      }
        val v =
          if (ads.contains(x.toString)) ads(x.toString).booleanValue
          else false
        FixedProperty(x.toString, Chooser.ifHappens(stereotypeFeatureNoise)(Chooser.randomBoolean(0.5))(v))
      }).toList
    fullAds
  }

  def properties(agent: Trustee): SortedMap[String, Property] = {
//    Chooser.select(
//      GaussianProperty("a", 0.9, 0.05),
//      GaussianProperty("a", 0.8, 0.5), //asdf
//      GaussianProperty("a", 0.6, 0.15),
//      GaussianProperty("a", 0.4, 0.15),
//      GaussianProperty("a", 0.3, 0.05), //0.3,0
//      GaussianProperty("a", 0.2, 0.5), //asdf
//      GaussianProperty("a", 0.5, 1) //0.1 1
//    ) :: Nil
    GaussianProperty(
      "a",
      Chooser.choose(Range.Double.inclusive(0d,1d,0.1)),
      Chooser.choose(Range.Double.inclusive(0.1d,0.5d,0.1))
    ) :: Nil
  }

  def capabilities(agent: Trustee): Seq[MarketPayload] = {
    val caps = simCapabilities.map(cap =>
      cap.copy(properties = agent.properties.map(x => {
        val prop: GaussianProperty = x._2.asInstanceOf[GaussianProperty]
//        prop.name -> GaussianProperty(prop.name, prop.mean + Chooser.randomGaussian(0,0.5), prop.std)
        prop.name -> GaussianProperty(prop.name, prop.mean + cap.properties(x._1).doubleValue, prop.std)
//        prop.name -> FixedProperty(prop.name, prop.value)
      }))
//      cap.copy(properties = properties(agent))
    )
//    println(agent.properties)
//    println("\t", simCapabilities)
//    println("\t", caps)
//    println()
    caps
  }

  def preferences(agent: Trustor): SortedMap[String, Property] = {
    if (numPreferences <= 1) FixedProperty("a", 0.5) :: Nil
    else FixedProperty("a", Chooser.choose(Range.Double.inclusive(0d,1d,1/numPreferences.toDouble))) :: Nil
  }

}
