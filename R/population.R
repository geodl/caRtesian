initPopulation <- function(popsize, functionSet) {

  population <- vector(mode = "list", length = popsize)

  nrows <- 2
  ncols <- 3
  levelsBack <- 2

  inputNodes <- generateInputs()
  for (i in popsize) {

    functionNodes <- generateFunctionNodes(nrows = nrows,
                                           ncols = ncols,
                                           levelsBack = 2)

    startOutputID <- nrow(inputNodes) + nrows * ncols
    outputNodes <- generateOutputs(startID = startOutputID)

    #population[[i]] <- combination of three above
  }

  return(population)
}

#' generateInputs
#'
#' Generates a data frame containing a row for each input attribute. Each row is
#' given a unique ID (chromoID) which is unique within the entire chromosome.
#' The value field of each row is set as NA initially.
#'
#' @return a data frame containing the input nodes
generateInputs <- function() {

  return(data.frame(chromoID = 1:inputSize,
                    value = rep(as.numeric(NA), inputSize)))
}

#' generateFunctionNodes
#'
#' Generates a data frame containing a row for each function node required. The
#' number of rows is equal to the product of the nrows and ncols parameters.
#' Each row is given a unique ID (chromoID) which is unique within the entire
#' chromosome. The value field of each row is set as NA initially. The funcID
#' field is the ID a random function chosen from the functionSet. The input
#' field is a vector of valid nodes to use as arguments to the function field.
#'
#' @param nrows the number of rows required in the chromosome
#' @param ncols the number of columns required in the chromosome
#' @param levelsBack the number of columns back that each column can access
#'
#' @return a data frame containing the function nodes
#'
#' @examples
#' generateFunctionNodes(2, 4, 2)
#' generateFunctionNodes(nrows = 3, ncols = 4, levelsBack = 3)
generateFunctionNodes <- function(nrows, ncols, levelsBack) {

  functionNodes <- createFunctionNodesStructure(nrows * ncols)

  rowFunctionNodes <- 1

  #Create a matrix containing the chromoIDs of the nodes that can be used as
  #input for each node
  validInputIDs <- matrix(1:inputSize,
                          nrow = levelsBack + 1,
                          ncol = nrows,
                          byrow = TRUE)
  rowValidInputIDs <- 2

  for (i in seq(from = inputSize + 1, to = ncols * nrows + inputSize,
               by = nrows)) {

    #Reset the mostRecentLevel
    mostRecentLevel <- c()

    for (j in i:(i + nrows - 1)) {

      #Create a function node and store it in the appropriate position
      functionNodes[rowFunctionNodes, ] <-
        makeFunctionNode(chromoID = j, validInputs = c(validInputIDs))

      rowFunctionNodes <- rowFunctionNodes + 1
      mostRecentLevel <- c(mostRecentLevel, j)
    }

    #Replace the no longer valid row of chromoIDs with the most recent level
    updateValidInputs(row = rowValidInputIDs,
                      level = mostRecentLevel,
                      validInputs = validInputIDs)

    #If reached the end of validInputIDs
    if (rowValidInputIDs == nrow(validInputIDs)) {
      #Reset row counter
      rowValidInputIDs <- 2
    }
  }

  return(functionNodes)
}

#' generateOutputs
#'
#' Generates a data frame containing a row for each output attribute. Each row
#' is given a unique ID (chromoID) which is unique within the entire chromosome.
#' The value field of each row is set as NA initially.
#'
#' @param startID the starting chromoID
#'
#' @return a data frame containing the output nodes
generateOutputs <- function(startID) {
  return(data.frame(chromoID = startID:outputSize,
                    value = rep(as.numeric(NA), outputSize)))
}

#' createFunctionNodesStructure
#'
#' Creates a data frame containing the structure of a function node. The data
#' frame is the length set in the generateFunctionNodes parameters.
#'
#' @param rowsRequired the number of rows to create
#'
#' @return the data frame created
#' @examples
#' createFunctionNodesStructure()
createFunctionNodesStructure <- function(rowsRequired) {

  #Create integer vectors containing rowsRequired NA values
  naColumn <- rep(as.integer(NA), rowsRequired)

  #Create a data frame with the length required to store the function nodes
  functionNodes <- data.frame(chromoID = naColumn,
                              value = as.numeric(naColumn),
                              funcID = naColumn)

  #Add a column to store a vector specifiying the input nodes
  functionNodes$inputs <- vector(mode = "list", length = nrow(functionNodes))

  return(functionNodes)
}

#' makeFunctionNode
#'
#' Creates a valid function node using the chromoID and value passed as a
#' parameter. A random function is chosen from the functionSet and random inputs
#' are chosen to satisfy the arity of the chosen function.
#'
#' @param chromoID the unique ID of the node
#' @param validInputs a vector containing the valid input chromoIDs
#'
#' @return the node created
#' @examples
#' makeFunctionNode(chromoID = 3)
#' makeFunctionNode(4)
#'
makeFunctionNode <- function(chromoID, validInputs) {

  #Store the maximum function ID
  maxFuncID <- nrow(functionSet)
  #Select a random function to use by choosing a random value
  funcID <- sample(1:maxFuncID, size = 1)
  #Retrieve the arity of the chosen function
  arity <- functionSet[funcID, ]$arity

  #Select the number of values that satisfies the arity of the chosen function
  #by randomly choosing a valid input node
  inputs <- sample(validInputs, size = arity, replace = TRUE)

  #Create a node and store the fields inside
  node <- createFunctionNodesStructure(rowsRequired = 1)
  node[1, ]$chromoID <- as.integer(chromoID)
  node[1, ]$value <- as.numeric(NA)
  node[1, ]$funcID <- funcID
  node[1, ]$inputs <- list(inputs)

  return(node)
}

#' updateValidInputs
#'
#' Replaces the no longer valid chromoIDs with the new level of
#' valid chromoIDs
#'
#' @param row the row to replace
#' @param level the new valid chromoIDs
#' @param validInputs a matrix containing the valid input chromoIDs
#'
#' @examples
#' updateValidInputIDs(2, c(3, 4, 5))
#' updateValidInputIDs(4, c(7, 8))
updateValidInputs <- function(row, level, validInputs) {

  #Replace the no longer valid chromoIDs with the
  #most recent valid chromoIDs
  validInputs[row,] <- level
}