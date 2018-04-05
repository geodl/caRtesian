#' tournamentSelection
#'
#' Performs Tournament Selection on the population by taking a sample of the
#' population (can be duplicates) and returning the individual with the best
#' fitness.
#'
#' @param population the population to sample
#' @param tournamentSize the number of samples to take
#'
#' @return the individual with the best fitness
tournamentSelection <- function(population, tournamentSize) {

  #Choose tournamentSize individuals from the population (can be duplicates)
  chosenIndividuals <- sample(population, size = tournamentSize, replace = TRUE)

  #Find the index of the lowest fitness value
  bestIndex <- which.min(sapply(chosenIndividuals, "[[", "fitness"))

  #Return the best indiviual found
  return(chosenIndividuals[bestIndex])
}
