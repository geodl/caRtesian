#' calculatePopFitness
#'
#' Calculates the fitness of each element in the population
#'
#' @param population the list of elements to be evaluated
#' @param dataset the dataset used to evaluate the element against
#' @param fitnessFunction the fitness function to use
#'
#' @return
#' @export
#'
#' @examples
calculatePopFitness <- function(population, dataset, fitnessFunction) {

  for(i in population) {

    fitness = calculateFitness(i, dataset)

    population[[i]] <- c(population[[i]], fitness = fitness)
  }

  return(population)
}

calculateFitness <- function(solution, dataset, fitnessFunction) {


  fitness <- as.numeric(0)

#' rmse
#'
#' Calculates the Root Mean Squared Error on a set of predicted and actual
#' values. RMSE gives a large weight to large errors and should be used where
#' large errors are undesirable.
#'
#' @param data a dataset containing predicted and actual values
#'
#' @return the rmse value
#' @export
rmse <- function(data) {

  results <- vector(mode = "numeric", length = nrow(data))
  for(i in 1:nrow(data)) {
    #Substract actual value from predicted value and square result
    results[i] <- (data[i, ]$predicted - data[i, ]$actual)^2
  }

  #Square root the mean of the results
  return (sqrt(mean(results)))
}

#' mae
#'
#' Calculates the Mean Absolute Error on a set of predicted and actual values.
#' MAE produces a clear indicator of the average absolute difference between
#' predicted and actual values.
#'
#' @param data a dataset containing predicted and actual values
#'
#' @return the mae value
#' @export
mae <- function(data) {

  results <- vector(mode = "numeric", length = nrow(data))
  for(i in 1:nrow(data)) {

    #Subtract the predicted value from actual value and take the absolute value
    results[i] <- abs(data[i, ]$actual - data[i, ]$predicted)
  }

  #Mean of the results
  return(mean(results))
}

#' checkSolutionFound
#'
#' Checks if a solution has been found. A solution has been found if there is
#' a individual in the population with a fitness value of zero.
#'
#' @param population the population which contains fitness values
#'
#' @return a boolean stating whether a solution has been found or not
checkSolutionFound <- function(population) {

  #Check if 0 is within the fitness values
  return(is.element(0, sapply(population, "[[", "fitness")))
}


#' nodesToProcess
#'
#' Find the functionNodes which are required by the outputNodes.
#'
#' @param solution The solution containing the nodes
#'
#' @return the functionNodes required
#'
nodesToProcess <- function(solution) {

  functionNodes <- solution$functionNodes

  outputID <- solution$outputNodes[1, ]$inputs

  nodesUsed <- vector(mode = "logical", length = nrow(functionNodes))

  nodesUsed <- traverseFunctionNodes(functionNodes, nodesUsed, outputID)

  return(functionNodes[nodesUsed, ])
}

#' traverseFunctionNodes
#'
#' Traverses through the functionNode structure starting at chromoID
#' and then recursively running on each of the nodes inputs.
#'
#' @param functionNodes the functionNode structure
#' @param nodesUsed a boolean vector signifying if a node was used
#' @param chromoID the chromoID of the starting node
#'
#' @return a boolean vector signifying the nodes used
#'
traverseFunctionNodes <- function(functionNodes, nodesUsed, chromoID) {

  #If the chromoID is now an inputNode
  if(chromoID < functionNodes[1, ]$chromoID) {

    return(nodesUsed)
  } else {

    #Find the index of the node with this chromoID
    index <- which(functionNodes$chromoID == chromoID)

    #Set the corresponding row in nodesUsed to TRUE
    nodesUsed[index] <- TRUE

    #Recursively loop over the inputs of each node used
    inputs <- unlist(functionNodes[index, ]$inputs)
    for(input in inputs) {
      nodesUsed <- traverseFunctionNodes(functionNodes, nodesUsed, input)
    }

    return(nodesUsed)
  }
}

#' calculateValue2
#'
#' Calculates the output value after propagating the values of the inputNodes
#' through each of the functionNodes. This function is similar to calculateValue
#' except that it is recursive.
#'
#' @param node the current node
#' @param solution the solution containing all nodes
#' @param functionSet the functionSet used when creating the population
#'
#' @return the value calculated
#'
calculateValue2 <- function(node, solution, functionSet) {

  if(is.null(node$inputs)) {
    return(node$value)
  } else {

    inputs <- unlist(node$inputs)

    #Get the name of the function to call from the functionSet
    funcToCall <- functionSet[node$funcID, ]$funcName

    firstInput <- findRow(solution, inputs[1])
    result1 <- calculateValue2(firstInput, solution, functionSet)

    if(length(inputs) == 2) {
      secondInput <- findRow(solution, inputs[2])
      result2 <- calculateValue2(secondInput, solution, functionSet)
      nodeValue <- do.call(funcToCall, list(result1, result2))
    } else {
      nodeValue <- do.call(funcToCall, list(result1))
    }

    return(nodeValue)
  }
}
