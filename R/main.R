cgp <- function(dataset,
                functionSet,
                stopCondition = timeCondition(5 * 60)) {

  dataset <- read.csv("./data/x_squared_minus_y.csv")

  functionSet <<- functionSet()

  ##Placeholders - these need to be determined either through headers or
  ##a scripted solution should be provided
  outputSize <<- calculateOutputSize(dataset)
  inputSize <<- calculateInputSize(dataset, outputSize)

  popsize <- 5

  population <- initPopulation(popsize)

  #Return results to top level
  return(population)
}
