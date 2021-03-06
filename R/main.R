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
#' @param updateFreq how many generations pass between updates on progress
#'
#' @return a list containing the best solution found and the chosen functionSet
#' @export
cgp <- function(dataset, model, functionSet = mathOpSet(),
                maxGenerations, fitnessFunction = mae,
                selectionMethod = list(func = muLambdaStrategy,
                                       args = c(population = NA, 4, NA)),
                rowsFuncNodes, colsFuncNodes, levelsBack,
                popSize = 5, updateFreq = 10) {

  #Extract only the required fields from the dataset
  dataset <- extractRequiredFields(dataset, model)

  #Group the dataset and model into a single list
  dataModel <- list(dataset = dataset, model = model)

  #Calculate the input and output sizes
  inputSize <- calculateInputSize(model)
  outputSize <- calculateOutputSize(model)

  #Generate initial population
  population <- initPopulation(popSize, functionSet, inputSize, outputSize,
                               rowsFuncNodes, colsFuncNodes, levelsBack)

  #Calculate the fitness values of the population
  population <- calculatePopFitness(population, dataModel, fitnessFunction,
                                    functionSet)

  #Setup variables required for evolutionary process
  selection <- selectionMethod$func
  args <- selectionMethod$args
  currGeneration <- 0
  solutionFound <- FALSE
  updateCount <- updateFreq

  #Wrap the parameters used to create the functionNodes into a list
  functionNodeStructure <- list(rows = rowsFuncNodes,
                                cols = colsFuncNodes,
                                levelsBack = levelsBack,
                                functionSet = functionSet)

  #Create a dataframe to hold the results to be plotted
  plotData <- data.frame(generation = integer(),
                         bestFitness = numeric(),
                         averageFitness = numeric())

  #Run evolution
  while (currGeneration < maxGenerations && !solutionFound) {

    currGeneration <- currGeneration + 1

    #Sort the population and store the best solution
    population <- sortPopulation(population)
    bestSolution <- population[[1]]

    updateCount <- printEvolutionDetails(currGeneration, maxGenerations,
                          bestSolution, population, updateFreq, updateCount)

    #Store the fitness data in plotData
    avgFitness <- mean(sapply(population, "[[", "fitness"))
    plotData[currGeneration, ] <- c(currGeneration,
                                    bestSolution$fitness,
                                    avgFitness)

    #Perform selection on the population to generate the new population
    population <- selection(population, args[2], functionNodeStructure)

    #Calculate the fitness of the new population
    population <- calculatePopFitness(population, dataModel,
                                      fitnessFunction, functionSet)

    #Check if a solution has been found
    solutionFound <- checkSolutionFound(population)
  }

  #Store the best solution found
  bestSolution <- sortPopulation(population)[[1]]

  printFinalDetails(currGeneration, maxGenerations, bestSolution, population)

  #Store the fitness data in plotData
  avgFitness <- mean(sapply(population, "[[", "fitness"))
  plotData[currGeneration, ] <- c(currGeneration,
                                  bestSolution$fitness,
                                  avgFitness)

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
                   functionSet = functionSet,
                   plotData = plotData)

  #Return results to top level
  return(solution)
}
