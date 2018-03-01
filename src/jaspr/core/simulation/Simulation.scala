package jaspr.core.simulation

import jaspr.core.results.{Result, Results}
import jaspr.utilities.Chooser

/**
  * Created by phil on 15/03/16.
  */


object Simulation {

  def apply(multiConfig: MultiConfiguration): Results = {
    val results: Results = new Results

    val iter = multiConfig.configs.zipWithIndex
    for ((config, configIndex) <- iter) {
      for (simulationIndex <- 0 until multiConfig.numSimulations) {
        val simulationSeed: Int = multiConfig.seed(configIndex, simulationIndex)
        Chooser.setSeed(simulationSeed)
        jaspr.debug(1000, "\n\n----- CONFIG " + configIndex + ", SIMULATION " + simulationIndex + ", seed: " + simulationSeed + " (" + config + ") -----")
        val simulation = config.newSimulation()
        val simResults: List[Result] = simulation.run()
        results.record(config, simResults)
        println()
        results.printIndividual(config, simResults, _.totalUtility)
      }
    }

    println("\n--- RESULTS ---\n")
    results.printAll(_.totalUtility)
    results.printAverage(_.totalUtility)
    println(results.results.keys.mkString("\t"))
    results.printChange(multiConfig.resultStart, multiConfig.resultEnd, _.totalUtility)
    results
  }
}


abstract class Simulation {

  val config: Configuration
  val network: Network

  private var currentRound = 0

  def round = currentRound

  private var results: List[Result] = Nil

  def run(): List[Result] = {
    while (round <= config.numRounds) {
      currentRound += 1
      jaspr.debug(100, "\n------ ROUND " + round + ", Utility: " + network.utility() + " ------")
      results = act() :: results
    }
    results
  }

  def act(): Result
}
