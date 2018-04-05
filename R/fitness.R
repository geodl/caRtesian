#' calculatePopFitness
#'
#' Calculates the fitness of each element in the population
#'
#' @param population the list of elements to be evaluated
#' @param dataset the dataset used to evaluate the element against
#' @param fitnessFunction the fitness function to use
#'
#' @return
#' @export
#'
#' @examples
calculatePopFitness <- function(population, dataset, fitnessFunction) {

  for(i in population) {

    fitness = calculateFitness(i, dataset)

    population[[i]] <- c(population[[i]], fitness = fitness)
  }

  return(population)
}

calculateFitness <- function(solution, dataset, fitnessFunction) {


  fitness <- as.numeric(0)

#' rmse
#'
#' Calculates the Root Mean Squared Error on a set of predicted and actual
#' values. RMSE gives a large weight to large errors and should be used where
#' large errors are undesirable.
#'
#' @param data a dataset containing predicted and actual values
#'
#' @return the rmse value
#' @export
rmse <- function(data) {

  residuals <- vector(mode = "numeric", length = nrow(data))
  for(i in data) {
    #Substract actual value from predicted value and square result
    residuals[i] <- (data$predicted - data$actual)^2
  }

  #Square root the mean of the residuals
  return (sqrt(mean(residuals)))
}

#' mae
#'
#' Calculates the Mean Absolute Error on a set of predicted and actual values.
#' MAE produces a clear indicator of the average absolute difference between
#' predicted and actual values.
#'
#' @param data a dataset containing predicted and actual values
#'
#' @return the mae value
#' @export
mae <- function(data) {

  results <- vector(mode = "numeric", length = nrow(data))
  for(i in data) {
    #Subtract the predicted value from actual value and take the absolute value
    residual[i] <- abs(data$actual - data$predicted)
  }

  #Mean of the results
  return(mean(results))
}
