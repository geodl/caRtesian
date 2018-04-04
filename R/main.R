#' cgp
#'
#' Performs Cartesian Genetic Programming using the parameters provided.
#'
#' @param dataset the dataset to use for training
#' @param model a model specifying the output and input fields of the dataset
#' @param functionSet the functions that can be used in the function nodes
#' @param stopCondition the stopping criteria of the evolutionary process
#' @param rowsFuncNodes the number of rows to use in the function node structure
#' @param colsFuncNodes the number of columns to use in the function node structure
#' @param levelsBack the number of columns back that a function node can access
#' @param popSize the number of solutions to generate in each generation
#'
#' @return a list containing the best solution found and the chosen functionSet
#' @export
cgp <- function(dataset, model, functionSet,
                stopCondition = timeCondition(5 * 60),
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

  #Return results to top level
  return(population)
}
