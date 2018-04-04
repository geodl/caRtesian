#' calculatePopFitness
#'
#' Calculates the fitness of each element in the population
#'
#' @param population the list of elements to be evaluated
#' @param dataset the dataset used to evaluate the element against
#'
#' @return
#' @export
#'
#' @examples
calculatePopFitness <- function(population, dataset) {

  population <- pop
  dataset <- read.csv("./data/x_squared_minus_y.csv")

  for(i in population) {

    fitness = calculateFitness(i, dataset)

    population[[i]] <- c(population[[i]], fitness = fitness)
  }

  return(population)
}

calculateFitness <- function(solution, dataset) {


  fitness <- as.numeric(0)


  #get the first row in the dataset
  #pass this as input into the inputNodes
  #so I set the solution$inputNodes values from 1:inputSize as the
}
