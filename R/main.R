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
cgp <- function(dataset, model, functionSet = mathOpSet(),
                maxGenerations, fitnessFunction = mae,
                selectionMethod = list(func = muLambdaStrategy,
                                       args = c(population = NA, 4, NA)),
                rowsFuncNodes, colsFuncNodes, levelsBack,
                popSize = 5) {

  #Extract only the required fields from the dataset
  dataset <- extractRequiredFields(dataset, model)

  #Calculate the input and output sizes
  inputSize <- calculateInputSize(model)
  outputSize <- calculateOutputSize(model)

  #Generate initial population
  population <- initPopulation(popSize, functionSet, inputSize, outputSize,
                               rowsFuncNodes, colsFuncNodes, levelsBack)

  #Calculate the fitness values of the population
  population <- calculatePopFitness(population, dataset, fitnessFunction,
                                    functionSet)

  #Setup variables required for evolutionary process
  selection <- selectionMethod$func
  args <- selectionMethod$args
  population <- sortPopulation(population)
  currGeneration <- 1
  solutionFound <- FALSE

  #Wrap the parameters used to create the functionNodes into a list
  functionNodeStructure <- list(rows = rowsFuncNodes,
                                cols = colsFuncNodes,
                                levelsBack = levelsBack,
                                functionSet = functionSet)

  #Run evolution
  while (currGeneration <= maxGenerations && !solutionFound) {

    #Store the best solution found
    bestSolution <- sortPopulation(population)[[1]]

    printEvolutionDetails(currGeneration, maxGenerations,
                          bestSolution, population)

    #Perform selection on the population to generate the new population
    population <- selection(population, args[2], functionNodeStructure)

    #Calculate the fitness of the new population
    population <- calculatePopFitness(population, dataset,
                                      fitnessFunction, functionSet)

    #Check if a solution has been found
    solutionFound <- checkSolutionFound(population)
    currGeneration <- currGeneration + 1
  }

  #Store the best solution found
  bestSolution <- sortPopulation(population)[[1]]

  printEvolutionDetails(currGeneration, maxGenerations,
                        bestSolution, population)

  #Extract only the nodes used to get an output value
  bestSolution <- extractRequiredNodes(bestSolution)

  #Create a character vector showing the functions applied
  #to get an output value
  text <- printSolution(bestSolution, functionSet)

  #Create a list containing the bestSolution, textual format
  #of the bestSolution, and the functionSet used so that the
  #solution can be used outside of this program
  solution <- list(bestSolution = bestSolution,
                   textualFormat = text,
                   functionSet = functionSet)

  #Return results to top level
  return(solution)
}
