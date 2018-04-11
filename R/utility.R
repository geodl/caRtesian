#' sampleWithoutBiasOrNA
#'
#' Perform the sample function after removing any duplicate or NA values from
#' the vector
#'
#' @param x the vector to choose from
#' @param size the number of items to choose
#' @param replace should sampling be with replacement?
#'
#' @return the result of the sample function on x
#' @examples
#' sampleWithoutBiasOrNA(c(1, 2, 3, 1), 2)
#' sampleWithoutBiasOrNA(c(3, 4, NA), 4, replace = TRUE)
sampleWithoutBiasOrNA <- function(x, size, replace = FALSE) {

  #Convert matrix to vector
  x <- c(x)

  #Remove any NA values
  x <- x[!is.na(x)]

  #Remove duplicates to avoid bias
  x <- unique(x)

  return(sample(x, size = size, replace = replace))
}

#' getValidInputs
#'
#' Determines the valid range of input chromoIDs for a given chromoID
#'
#' @param chromoID the chromoID to calculate the range of
#' @param functionNodeRange all the chromoIDs contained in functionNodes
#' @param functionNodeStructure the parameters used to create functionNodes
#'
#' @return the valid chromoIDs
#'
getValidInputs <- function(chromoID, functionNodeRange, functionNodeStructure) {

  #Put the functionNode chromoIDs into a matrix
  functionNodeMatrix <- matrix(functionNodeRange,
                               nrow = functionNodeStructure$rows,
                               ncol = functionNodeStructure$cols)

  #Find the column index containing chromoID
  column <- which(functionNodeMatrix == chromoID, arr.ind = TRUE)[[1, 2]]


  #Get the columns that are in the levelsBack range
  validColumns <- (column - functionNodeStructure$levelsBack):(column - 1)
  validColumns <- validColumns[validColumns >= 1]

  #Extract the chromoIDs from the validColumns
  validChromoIDs <- functionNodeMatrix[, validColumns]

  return(as.vector(validChromoIDs))
}

#' is.functionNode
#'
#' Checks if the node specified by chromoID is a functionNode
#'
#' @param solution the solution containing the nodes
#' @param chromoID the chromoID of the node to check
#'
#' @return a logical value stating if the node is an functionNode or not
#'
is.functionNode <- function(solution, chromoID) {

  return(is.element(chromoID, solution$functionNodes$chromoID))
}

#' is.outputNode
#'
#' Checks if the node specified by chromoID is an outputNode
#'
#' @param solution the solution containing the nodes
#' @param chromoID the chromoID of the node to check
#'
#' @return a logical value stating if the node is an outputNode or not
#'
is.outputNode <- function(solution, chromoID) {

  return(is.element(chromoID, solution$outputNodes$chromoID))
}

#' findRow
#'
#' Finds the row in the solution that contains chromoID
#'
#' @param solution the solution containing a row with chromoID
#' @param chromoID the row to find
#'
#' @return the row found
#'
findRow <- function(solution, chromoID) {

  #Get the chromoIDs of the functionNodes
  chromoIDs <- solution$functionNodes$chromoID

  #If the chromoID is the ID of an inputNode
  if (chromoID < min(chromoIDs)) {

    #Find the rowIndex of the node with this chromoID
    rowIndex <- which(solution$inputNodes$chromoID == chromoID)

    return(solution$inputNodes[rowIndex, ])

    #If the chromoID is the ID of an outputNode
  } else if (chromoID > max(chromoIDs)) {

    #Find the rowIndex of the node with this chromoID
    rowIndex <- which(solution$outputNodes$chromoID == chromoID)

    return(solution$outputNodes[rowIndex, ])

  } else {
    #The chromoID is the ID of a functionNode

    #Find the rowIndex of the node with this chromoID
    rowIndex <- which(solution$functionNodes$chromoID == chromoID)

    return(solution$functionNodes[rowIndex, ])
  }
}

#' sortPopulation
#'
#' Sorts the population from lowest fitness value to highest
#'
#' @param population the population to be sorted
#'
#' @return the population after sorting
sortPopulation <- function(population) {

  #Extract the fitness values from the population
  fitnessValues <- sapply(population, "[[", "fitness")

  #Get the index ordering that will put the values
  #into ascending order and reorder the population
  return(population[order(fitnessValues, decreasing = FALSE, na.last = TRUE)])
}

#' printEvolutionDetails
#'
#' Print information about the progress of evolution
#'
#' @param currGeneration the current generation of evolution
#' @param maxGeneration the maximum generations for evolution
#' @param bestSolution the best solution found so far
#' @param population the population of all solutions
#'
#' @return
printEvolutionDetails <- function(currGeneration, maxGeneration,
                                  bestSolution, population) {

  avgFitness <- mean(sapply(population, "[[", "fitness"))
  cat("\nGeneration:", currGeneration, "/", maxGeneration)
  cat("\nFitness of best solution so far:", bestSolution$fitness)
  cat("\nAverage fitness of population:", avgFitness)

  return(NULL)
}
