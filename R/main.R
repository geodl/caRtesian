cgp <- function(dataset = NULL,
                functionSet,
                stopCondition = timeCondition(5 * 60)) {

  #Make sure dataset is provided
  if (is.null(dataset)) {
    stop("'dataset' parameter was NULL. Please provide a dataset to use")
  }

  dataset <- read.csv("./data/x_squared_minus_y.csv")

  ##Placeholders - these need to be determined either through headers or
  ##a scripted solution should be provided
  outputSize <<- 1
  inputSize <<- ncol(dataset) - outputSize

  popsize <- 5

  population <- initPopulation(popsize)

  #Return results to top level
  #return()
}
