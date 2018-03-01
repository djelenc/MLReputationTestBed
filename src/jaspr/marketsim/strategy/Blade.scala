package jaspr.marketsim.strategy

import jaspr.core.agent.{Client, Provider}
import jaspr.core.provenance.{RatingRecord, ServiceRecord}
import jaspr.core.service.{ClientContext, ServiceRequest, TrustAssessment}
import jaspr.core.simulation.Network
import jaspr.core.strategy.StrategyInit
import jaspr.utilities.matrix.{Matrix, RowVector}
import jaspr.utilities.{Dirichlet, Discretization}

/**
  * Created by phil on 12/06/17.
  */
class Blade(override val numBins: Int,
            override val lower: Double,
            override val upper: Double) extends StrategyCore with Discretization {

  override def compute(init: StrategyInit, request: ServiceRequest): TrustAssessment = {
    val directModels = init.asInstanceOf[BladeInit].directModels
    val repModels = init.asInstanceOf[BladeInit].repModels
    val trustees = init.asInstanceOf[BladeInit].trustees
    val witnesses = init.asInstanceOf[BladeInit].witnesses
    val dirModelPrior = init.asInstanceOf[BladeInit].dirModelPrior
    val repModelPrior = init.asInstanceOf[BladeInit].repModelPrior

    val repMatrix = init.asInstanceOf[BladeInit].repMatrix
    val trustee = request.provider

    val priorModel = directModels.getOrElse(trustee, dirModelPrior)

    val likelihoods = for (tr <- witnesses) yield {
      val cpt: Matrix = repMatrix
        .filter(x => x._1._1 == tr && x._1._2 != trustee).values
        .foldLeft(new Matrix(dirModelPrior.size, repModelPrior.size, 1d))(_ @+ _)

      val cptnorm = cpt @/ cpt.colsum().sum

      val rcond: Matrix = divRcondRows(divMeanPrior(priorModel, cptDivRowSum(cptnorm)))

      val sourceAlpha: RowVector = repModels.getOrElse((tr, trustee), repModelPrior).alpha @- 1d

      (rcond @* sourceAlpha).rowsum().transpose() // total likelihood
    }

    val postModel = new Dirichlet(likelihoods.foldLeft(priorModel.alpha)(_ @+ _), priorModel.domain)

    new TrustAssessment(init.context, request, postModel.expval())
  }

  override def initStrategy(network: Network, context: ClientContext, requests: Seq[ServiceRequest]): StrategyInit = {
    val directRecords: Seq[ServiceRecord with RatingRecord] = getDirectRecords(network, context)
    val witnessRecords: Seq[ServiceRecord with RatingRecord] = getWitnessRecords(network, context)

    val trustees: Seq[Provider] = (directRecords.map(_.provider).distinct ++ witnessRecords.map(_.provider).distinct).distinct.sortBy(_.id)
    val witnesses: Seq[Client] = witnessRecords.map(_.client).distinct

    val dirPrior = this.getDirectPrior(context)
    val repPrior = this.getRepPrior(context.client)
    val dirModels = this.getDirectModels(directRecords, context, dirPrior)

    val repModels = this.getRepModels(witnessRecords, context, trustees, witnesses)

    val repMatrix: Map[(Client, Provider), Matrix] =
      (for (tr <- witnesses; te <- trustees) yield {
        val model = repModels.getOrElse((tr, te), repPrior)
        val opinionObs: RowVector = model.alpha @- repPrior.alpha
        (tr, te) -> dirModels.getOrElse(te, dirPrior).mean().transpose() @* opinionObs
      }).toMap

    new BladeInit(
      context,
      trustees, witnesses,
      dirPrior, repPrior,
      dirModels, repModels, repMatrix
    )}

  def getRepPrior(client: Client): Dirichlet = {
    new Dirichlet(numBins)
  }

  def getDirectPrior(context: ClientContext): Dirichlet = {
    new Dirichlet(numBins)
  }

  def getDirectModels(directRatings: Seq[ServiceRecord with RatingRecord],
                      context: ClientContext,
                      prior: Dirichlet): Map[Provider, Dirichlet] = {
    //    val provratings: Map[AbstractAgent,List[Rating]] = directRatings.groupBy(_.provider)
    //    val asdf: Map[AbstractAgent, Map[Double, Int]] = provratings.mapValues(x => x.groupBy(y => discretize(y.rating)).mapValues(_.size+1))
    //    val out = asdf.mapValues(x => new Dirichlet((0 until numbins).map(y => x.getOrElse(y, 1).toDouble)))
    //    directRatings.groupBy(_.provider).mapValues(x => prior.observe(x.map(y => discretizeDouble(y.rating))))
    directRatings.groupBy(_.provider).map(x => x._1 ->
      prior.observe(x._2.map(y => discretizeDouble(y.rating)), x._2.map(y => weightedRating(y, context)))
    )
  }

  def getRepModels(witnessReports: Seq[ServiceRecord with RatingRecord],
                   context: ClientContext,
                   trustees: Seq[Provider],
                   witnesses: Seq[Client]): Map[(Client, Provider), Dirichlet] = {
    (for (w <- witnesses; t <- trustees) yield {
      val wr = witnessReports.filter(x => x.client == w && x.provider == t)
      (w, t) -> getRepPrior(w).observe(
        wr.map(x => discretizeDouble(x.rating)), wr.map(x => weightedRating(x, context))
      )
    }).toMap
  }

  def weightedRating(record: ServiceRecord with RatingRecord, context: ClientContext): Double = {
    1d
  }

  def cptDivRowSum(cpt: Matrix) = cpt @/ cpt.rowsum()

  def divMeanPrior(priorModel: Dirichlet, rcond: Matrix) = rcond @* priorModel.mean().transpose()

  def divRcondRows(rcond: Matrix) = rcond @/ rcond.colsum()
}


