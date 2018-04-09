#' muLambdaStrategy
#'
#' Runs the Mu + Lambda Evolutionary Strategy. Mu is set as 1 and Lambda can be
#' changed through the lambda parameter. Mu is the number of parents and Lambda
#' is the number of offspring to generate.
#'
#' @param population the population to be evolved
#' @param lambda the number of offspring to generate
#' @param functionNodeStructure the parameters used when creating functionNodes
#'
#' @return a new population containing the parent used and the offspring
#'
muLambdaStrategy <- function(population, lambda, functionNodeStructure) {

  lambda <- 4

  #Extract the fittest individual
  parent <- population[[1]]

  offspring <- vector(mode = "list", length = lambda)

  for(i in 1:lambda) {

    offspring[[i]] <- pointMutation(parent, functionNodeStructure)

  }

  #Extract the fittest offspring
  bestOffspring <- offspring[[which.min(sapply(offspring, "[[", "fitness"))]]

  #If any offspring has an equal or better fitness than the parent
  #then set it as the new fittest
  if(bestOffspring$fitness <= parent$fitness) {

    #Move the fittest individual to the start of the list
    offspring <- sortPopulation(offspring)
    #Add the parent to the end of the list
    return(c(offspring, list(parent)))

  } else {
    #Parent is still fittest so add the offspring to the end of the list
    return(c(list(parent), offspring))
  }
}

#' pointMutation
#'
#' Peforms point mutation on the solution. It changes up to 5% of the genes
#' and can change either the function used, or the inputs used.
#'
#' @param solution the solution to be mutated
#' @param functionNodeStructure the parameters used when creating functionNodes
#'
#' @return the solution after mutation
#'
pointMutation <- function(solution, functionNodeStructure) {

  #Mutate 5% of the genes
  maxGenesToMutate <- ceiling(0.05 * (nrow(solution$functionNodes) +
                                        nrow(solution$outputNodes))) * 3

  #Randomly choose how many genes to change
  numGenesToMutate <- sample(0:maxGenesToMutate, size = 1)


  #Get the chromoIDs of the nodes that can be mutated
  functionNodeIDs <- solution$functionNodes$chromoID
  outputNodeIDs <- solution$outputNodes$chromoID
  mutableChromoIDs <- c(functionNodeIDs, outputNodeIDs)

  #Randomly choose the nodes to mutate (can choose same node multiple times)
  nodesToMutate <- sample(mutableChromoIDs, size = numGenesToMutate,
                          replace = TRUE)

  for(i in nodesToMutate) {

    #If the node is a functionNode and we want to mutate the function
    if(is.functionNode(solution, i) && sample(0:1, size = 1) == 0) {
      #Mutate the function
      solution <- mutateFunction(solution, i, functionNodeStructure)

    } else {
      #Mutate the nodes inputs
      mutation <- mutateInput(solution, i, functionNodeStructure)

      if(is.functionNode(solution, i)) {
        #Get the index of the node to be mutated
        nodeChanged <- which(solution$functionNodes$chromoID == i)
        solution$functionNodes$input[[nodeChanged]] <- mutation
      } else {
        #Get the index of the node to be mutated
        nodeChanged <- which(solution$outputNodes$chromoID == i)
        solution$outputNodes$input[[nodeChanged]] <- mutation
      }
    }
  }

  return(solution)
}

#' mutateInput
#'
#' Generates a random new input chromoID for the node given by chromoID
#'
#' @param solution the solution containing the nodes
#' @param chromoID the node to be given new inputs
#' @param functionNodeStructure the parameters used when creating functionNodes
#'
#' @return the new input values
#'
mutateInput <- function(solution, chromoID, functionNodeStructure) {

  #Get the chromoIDs from each data frame
  inputNodeRange <- solution$inputNodes$chromoID
  functionNodeRange <- solution$functionNodes$chromoID

  #If the node to change is an outputNode
  if(is.outputNode(solution, chromoID)) {
    #Set the new input as a random node
    newInputs <- sample(c(inputNodeRange, functionNodeRange), size = 1)
  } else {
    #Get the chromoIDs of the nodes that are in range
    functionNodeRange <- getValidInputs(chromoID,
                                        functionNodeRange,
                                        functionNodeStructure)

    #Get the number of inputs to the functionNode
    numInputs <- length(unlist(findRow(solution, chromoID)$inputs))

    #Randomly generate input chromoIDs
    newInputs <- sample(c(inputNodeRange, functionNodeRange),
                        size = numInputs,
                        replace = TRUE)
  }

  return(newInputs)
}

#' mutateFunction
#'
#' Generates a random new funcID for the node given by chromoID. Where the new
#' function has a different number of inputs than the current function, new
#' inputs are generated or removed as fit
#'
#' @param solution the solution containing the nodes
#' @param chromoID the node to be given a new function
#' @param functionNodeStructure the parameters used when creating functionNodes
#'
#' @return the solution after mutation
#'
mutateFunction <- function(solution, chromoID, functionNodeStructure) {

  #Get the index of the node to be mutated
  nodeChanged <- which(solution$functionNodes$chromoID == chromoID)

  funcSet <- functionNodeStructure$functionSet

  #Randomly choose a new function
  chosenFunc <- sample(1:nrow(funcSet), size = 1)
  solution$functionNodes[nodeChanged, ]$funcID <- chosenFunc

  #Get the number of arguments that is currently used
  numArguments <- length(unlist((solution$functionNodes[nodeChanged, ]$inputs)))

  #Get the inputs currently used
  oldInput <- unlist(solution$functionNodes[nodeChanged, ]$inputs)

  if(numArguments < length(oldInput)) { #Need to add another input

    #Get the chromoIDs from functionNodes data frame
    functionNodeRange <- solution$functionNodes$chromoID

    #Get the chromoIDs of the nodes that are in range
    functionNodeRange <- getValidInputs(chromoID,
                                        functionNodeRange,
                                        functionNodeStructure)

    #Randomly generate an input chromoID
    newInput <- sample(functionNodeRange, size = 1)

    solution$functionNodes$inputs[[nodeChanged]] <- c(oldInput, newInput)

  } else if( numArguments > length(oldInput)) { #Need to remove an input

    #Choose a random input to remove
    remove <- sample(oldInput, size = 1)
    remove <- solution$functionNodes$inputs[[nodeChanged]] != remove

    #Remove the random input
    solution$functionNodes$inputs[[nodeChanged]] <-
      solution$functionNodes$inputs[[nodeChanged]][remove]

  }

  return(solution)
}
