initPopulation <- function(popsize) {

  population <- vector(mode = "list", length = popsize)

  nrows <- 2
  ncols <- 3
  levelsBack <- 2

  inputNodes <- generateInputs(inputSize)
  for (i in 1:popsize) {

    functionNodes <- generateFunctionNodes(startID = nrow(inputNodes) + 1,
                                           nrows = nrows,
                                           ncols = ncols,
                                           levelsBack = levelsBack)

    maxInputID <- tail(functionNodes, 1)$chromoID
    startOutputID <- maxInputID + 1
    outputNodes <- generateOutputs(startID = startOutputID,
                                   maxInputID = maxInputID,
                                   outputSize = outputSize)

    #Combine the three types of nodes into a list and store it as an
    #element of the population list
    population[[i]] <- list(inputNodes = inputNodes,
                            functionNodes = functionNodes,
                            outputNodes = outputNodes)
  }

  return(population)
}

#' generateInputs
#'
#' Generates a data frame containing a row for each input attribute and another
#' row used to store numeric constants. Each row isgiven a unique ID (chromoID)
#' which is unique within the entire chromosome. The value field of each row is
#' set as NA initially except for the numeric constant which is initialised.
#'
#' @param inputSize the number of input nodes required
#'
#' @return a data frame containing the input nodes
generateInputs <- function(inputSize) {

  inputSize <- 3
  inputSize <- inputSize + 1

  inputNodes <- data.frame(chromoID = 1:inputSize,
             value = rep(as.numeric(NA), inputSize))

  inputNodes[inputSize, 2] <- runif(1, min=-10, max = 10)


  return(inputNodes)
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
#' @param startID the starting chromoID
#' @param nrows the number of rows required in the chromosome
#' @param ncols the number of columns required in the chromosome
#' @param levelsBack the number of columns back that each column can access
#'
#' @return a data frame containing the function nodes
#'
#' @examples
#' generateFunctionNodes(2, 4, 2)
#' generateFunctionNodes(nrows = 3, ncols = 4, levelsBack = 3)
generateFunctionNodes <- function(startID, nrows, ncols, levelsBack) {

  functionNodes <- createFunctionNodesStructure(nrows * ncols)

  rowFunctionNodes <- 1

  #Create a matrix containing the chromoIDs of the nodes that can be used as
  #input for each node
  validInputIDs <- matrix(1:(startID-1),
                          nrow = levelsBack + 1,
                          ncol = max(startID-1, nrows),
                          byrow = TRUE)

  rowValidInputIDs <- 2

  for (i in seq(from = startID, to = ncols * nrows + startID - 1,
               by = nrows)) {

    #Reset the mostRecentLevel
    mostRecentLevel <- c()

    for (j in i:(i + nrows - 1)) {

      #Create a function node and store it in the appropriate position
      functionNodes[rowFunctionNodes, ] <-
        makeFunctionNode(chromoID = j, validInputs = c(validInputIDs))

      #Increment row counter
      rowFunctionNodes <- rowFunctionNodes + 1

      #Add chromaID of the newly created node into a vector
      mostRecentLevel <- c(mostRecentLevel, j)
    }

    #Replace the no longer valid row of chromoIDs with the most recent level
    validInputIDs <- updateValidInputs(row = rowValidInputIDs,
                      level = mostRecentLevel,
                      validInputs = validInputIDs)

    #If the row counter is not at the end of the matrix
    if (rowValidInputIDs != nrow(validInputIDs)) {
      #Increment row counter
      rowValidInputIDs <- rowValidInputIDs + 1
      } else {
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
#' @param maxInputID the maximum chromoID that can be accessed
#' @param outputSize the number of output nodes required
#'
#' @return a data frame containing the output nodes
generateOutputs <- function(startID, maxInputID, outputSize) {

  chromoID <- seq.int(from = startID, by = 1, length.out = outputSize)
  value <- rep(as.numeric(NA), outputSize)
  inputs <- sample(1:maxInputID, size = outputSize)

  return(data.frame(chromoID = chromoID, value = value, inputs = inputs))
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
  inputs <- sampleWithoutBiasOrNA(validInputs, size = arity, replace = TRUE)

  #Create a node and store the fields inside
  node <- createFunctionNodesStructure(rowsRequired = 1)
  node[1, ]$chromoID <- as.integer(chromoID)
  node[1, ]$value <- as.numeric(NA)
  node[1, ]$funcID <- funcID
  node[1, ]$inputs <- list(inputs)

  return(node)
}

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

#' updateValidInputs
#'
#' Replaces the no longer valid chromoIDs with the new level of
#' valid chromoIDs
#'
#' @param row the row to replace
#' @param level the new valid chromoIDs
#' @param validInputs a matrix containing the valid input chromoIDs
#'
#' @return a matrix containing the updated valid input chromoIDs

#' @examples
#' updateValidInputIDs(2, c(3, 4, 5), validInputs)
#' updateValidInputIDs(4, c(7, 8), validInputs)
updateValidInputs <- function(row, level, validInputs) {

  #Add NAs to level so it can fit in the matrix
  while(ncol(validInputs) > length(level)) {
    level <- c(level, NA)
  }

  #Replace the no longer valid chromoIDs with the
  #most recent valid chromoIDs
  validInputs[row, ] <- level

  return(validInputs)
}
