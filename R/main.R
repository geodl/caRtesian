cgp <- function(dataset = NULL) {

  #Make sure dataset is provided
  if (is.null(dataset)) {
    stop("'dataset' parameter was NULL. Please provide a dataset to use")
  }

  dataset <- read.csv("./data/sin vs time.csv")
  dataset <- read.csv("./data/x_squared_minus_y.csv")
  outputSize <- 1
  inputSize <- ncol(dataset) - outputSize

  functionSet <- arithmeticSet()
  popsize <- 5

  population <- initPopulation(popsize, functionSet = functionSet)

}
