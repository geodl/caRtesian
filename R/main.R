#' cgp
#'
#' Performs Cartesian Genetic Programming using the parameters provided.
#'
#' @param dataset the dataset to use for training
#' @param model a model specifying the output and input fields of the dataset
#' @param functionSet the functions that can be used in the function nodes
#' @param maxGenerations the maximum generations of the evolutionary process
#' @param fitnessFunction the fitness function to use
#' @param selectionMethod the function and parameters to use for selection
#' @param rowsFuncNodes the number of rows to use in the function node structure
#' @param colsFuncNodes the number of columns to use in the function node structure
#' @param levelsBack the number of columns back that a function node can access
#' @param popSize the number of solutions to generate in each generation
#'
#' @return a list containing the best solution found and the chosen functionSet
#' @export
cgp <- function(dataset, model, functionSet,
                maxGenerations, fitnessFunction,
                selectionMethod = list(func = muLambda,
                                       args = c(population = NA, 4)),
                rowsFuncNodes, colsFuncNodes, levelsBack,
                popSize) {

  #Extract only the required fields from the dataset
  dataset <- extractRequiredFields(dataset, model)

  #Calculate the input and output sizes
  inputSize <- calculateInputSize(model)
  outputSize <- calculateOutputSize(model)

  #Generate initial population
  population <- initPopulation(popSize, functionSet, inputSize, outputSize,
                               rowsFuncNodes, colsFuncNodes, levelsBack)

  #Calculate the fitness values of the population
  population <- calculatePopFitness(population, dataset, fitnessFunction)



  #Setup variables required for evolutionary process
  selection <- selectionMethod$func
  args <- selectionMethod$args
  population <- sortPopulation(population)
  currGeneration <- 0
  solutionFound <- FALSE

  #Run evolution
  while(currGeneration < maxGeneration && !solutionFound) {
    population <- selection(population, args[1])

    population <- calculatePopFitness(population, dataset, fitnessFunction)

    solutionFound <- checkSolutionFound(population)
    currGeneration <- currGeneration + 1
  }


  #Return results to top level
  return(population)
}
