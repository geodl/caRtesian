#' muLambdaStrategy
#'
#' Runs the Mu + Lambda Evolutionary Strategy. Mu is set as 1 and Lambda can be
#' changed through the lambda parameter. Mu is the number of parents and Lambda
#' is the number of offspring to generate.
#'
#' @param population the population to be evolved
#' @param lambda the number of offspring to generate
#'
#' @return a new population containing the parent used and the offspring
#'
muLambdaStrategy <- function(population, lambda) {

  #Extract the fittest individual
  parent <- population[[1]]

  offspring <- vector(mode = "list", length = lambda)

  for(i in 1:lambda) {

    offspring[i] <- mutate(fittest)

  }

  #Extract the fittest offspring
  bestOffspring <- offspring[[which.min(sapply(offspring, "[[", "fitness"))]]

  #If any offspring has an equal or better fitness than the parent
  #then set it as the new fittest
  if(bestOffspring$fitness <= fittest$fitness) {

    #Move the fittest individual to the start of the list
    offspring <- sortPopulation(offspring)
    #Add the parent to the end of the list
    return(c(offspring, list(parent)))

  } else {
    #Parent is still fittest so add the offspring to the end of the list
    return(c(list(parent), offspring))
  }
}
