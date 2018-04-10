#' calculatePopFitness
#'
#' Calculates the fitness of each element in the population
#'
#' @param population the list of elements to be evaluated
#' @param dataset the dataset used to evaluate the element against
#' @param fitnessFunction the fitness function to use
#' @param functionSet the functionSet used with the population
#'
#' @return the population with fitness values nested inside
#'
calculatePopFitness <- function(population, dataset,
                                fitnessFunction, functionSet) {

  for (i in 1:length(population)) {

    fitness <- calculateFitness(population[[i]], dataset, fitnessFunction,
                               functionSet)

    population[[i]]$fitness <- fitness
  }

  return(population)
}

calculateFitness <- function(solution, dataset, fitnessFunction, functionSet) {

  #Get only the required nodes
  functionNodesUsed <- nodesToProcess(solution)

  #Get the numbers of rows up to the random constant row
  inputs <- 1:(nrow(solution$inputNodes) - 1)

  #Create vector to hold results
  results <- vector(mode = "numeric", length = nrow(dataset))

  for (i in 1:nrow(dataset)) {

    #Load the inputNodes with input data from the dataset
    for (input in inputs) {
      solution$inputNodes[input, ]$value <- dataset[i, ][[input + 1]]
    }

    #Decode the solution
    solution <- decode(solution, functionNodesUsed, functionSet)

    #Write the result of decoding into the results vector
    results[i] <- solution$outputNodes[1, ]$value
  }


  #Combine the dataset outputs and results into a single list
  outputs <- data.frame(actual = dataset$output, predicted = results)

  return(fitnessFunction(outputs))
}

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
  for (i in 1:nrow(data)) {
    #Substract actual value from predicted value and square result
    results[i] <- (data[i, ]$predicted - data[i, ]$actual) ^ 2
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
  for (i in 1:nrow(data)) {

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
  if (chromoID < functionNodes[1, ]$chromoID) {

    return(nodesUsed)
  } else {

    #Find the index of the node with this chromoID
    index <- which(functionNodes$chromoID == chromoID)

    #Set the corresponding row in nodesUsed to TRUE
    nodesUsed[index] <- TRUE

    #Recursively loop over the inputs of each node used
    inputs <- unlist(functionNodes[index, ]$inputs)
    for (input in inputs) {
      nodesUsed <- traverseFunctionNodes(functionNodes, nodesUsed, input)
    }

    return(nodesUsed)
  }
}

#' decode
#'
#' Decodes the solution
#'
#' @param solution the solution containing the nodes
#' @param functionNodesUsed the function nodes required to get an output value
#' @param functionSet the functionSet used when creating the population
#'
#' @return the solution with updated values inside
#'
decode <- function(solution, functionNodesUsed, functionSet) {

  #Get the chromoID of the last functionNode used
  #endFunctionNode <- solution$outputNodes[1,]$inputs

  #Find the row which contains the chromoID found
  #row <- findRow(solution, endFunctionNode)

  #solution <- calculateValue2(row, solution, functionSet)

  #Calculate the values of all the required nodes
  solution <- calculateValueInSolution(solution, functionNodesUsed, functionSet)

  return(solution)
}

#' calculateValue
#'
#' Calculates the output value after propagating the values of the inputNodes
#' through each of the functionNodes.
#'
#' @param node the current functionNode
#' @param solution the solution containing the nodes
#' @param functionSet the functionSet used when creating the population
#'
#' @return the value for the currentNode
#'
calculateValue <- function(node, solution, functionSet) {

  #Get the name of the functiob to call from the functionSet
  funcToCall <- functionSet[node$funcID, ]$funcName

  inputs <- unlist(node$input[[1]])

  #Get the value of the first argument of the funcToCall
  firstArgument <- findRow(solution, inputs[1])$value

  #If the function takes two parameters
  if (length(inputs) == 2) {
    #Get the value of the second argument of the funcToCall
    secondArgument <- findRow(solution, inputs[2])$value
    value <- do.call(funcToCall, list(firstArgument, secondArgument))
  } else {
    #The function takes one parameter
    value <- do.call(funcToCall, list(firstArgument))
  }

  return(value)
}

#' calculateValueInSolution
#'
#' Calculates the value that should be stored in each entry of functionNodesUsed
#' and stores it in the corresponding location of solution
#'
#' @param solution the solution containing the nodes
#' @param functionNodesUsed the function nodes required to get an output value
#' @param functionSet the functionSet used when creating the population
#'
#' @return the solution after writing the calculated values in
#'
calculateValueInSolution <- function(solution, functionNodesUsed, functionSet) {

  #If the solution uses functionNodes
  if (nrow(functionNodesUsed) != 0) {
    for (i in 1:nrow(functionNodesUsed)) {

      #Store the current node
      currentNode <- functionNodesUsed[i, ]

      #Calculate the value for this node
      value <- calculateValue(currentNode, solution, functionSet)

      #Find the index of the currentNode in the solution
      index <- which(solution$functionNodes$chromoID == currentNode$chromoID)

      #Write the value into the solution
      solution$functionNodes[index, ]$value <- value
    }

    #Write the last value into outputNodes
    solution$outputNodes[1, ]$value <- solution$functionNodes[index, ]$value

  } else {
    #The solution takes its output directly from an inputNode

    #Get the chromoID of the inputNode used
    chromoID <- solution$outputNodes[1, ]$inputs

    #Write the value of the inputNode into the solution
    solution$outputNodes[1, ]$value <- findRow(solution, chromoID)$value
  }

  return(solution)
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

  #If this is null then the node is an input node and the value can be extracted
  if (is.null(node$inputs)) {
    return(node$value)
  } else {

    inputs <- unlist(node$inputs)

    #Get the name of the function to call from the functionSet
    funcToCall <- functionSet[node$funcID, ]$funcName

    #Get the node which is the first argument of the funcToCall
    firstInput <- findRow(solution, inputs[1])
    #Calculate the value of this node
    firstArgument <- calculateValue2(firstInput, solution, functionSet)

    if (length(inputs) == 2) {
      #Get the node which is the second argument of the funcToCall
      secondInput <- findRow(solution, inputs[2])
      #Calculate the value of this node
      secondArgument <- calculateValue2(secondInput, solution, functionSet)
      nodeValue <- do.call(funcToCall, list(firstArgument, secondArgument))
    } else {
      nodeValue <- do.call(funcToCall, list(firstArgument))
    }

    return(nodeValue)
  }
}
