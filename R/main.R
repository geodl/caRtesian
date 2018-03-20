#' cgp
#'
#' Performs Cartesian Genetic Programming using the parameters provided.
#'
#' @param dataset the dataset to use for training
#' @param functionSet the functions that can be used in the function nodes
#' @param stopCondition the stopping criteria of the evolutionary process
#' @param inputSize the number of input nodes required. This should be the same as the number of input fields in the provided dataset
#' @param outputSize the number of output nodes required. This should be the same as the number of output fields in the provided dataset
#' @param rowsFuncNodes the number of rows to use in the function node structure
#' @param colsFuncNodes the number of columns to use in the function node structure
#' @param levelsBack the number of columns back that a function node can access
#'
#' @return a list containing the best solution found and the chosen functionSet
#' @export
cgp <- function(dataset, functionSet,
                stopCondition = timeCondition(5 * 60),
                inputSize, outputSize,
                rowsFuncNodes, colsFuncNodes, levelsBack) {

  dataset <- read.csv("./data/x_squared_minus_y.csv")

  functionSet <<- functionSet()

  popsize <- 5

  population <- initPopulation(popsize, inputSize, outputSize,
                               rowsFuncNodes, colsFuncNodes, levelsBack)

  #Return results to top level
  return(population)
}
