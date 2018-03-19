#' arithmeticSet
#'
#' Creates a function set containing the basic arithmetic operators (+, -, *, /)
#'
#' @return the function set created
#' @examples
#' arithmeticSet()
arithmeticSet <- function() {

  functionDefs <- c(c("add", 2),
                    c("subtract", 2),
                    c("multiply", 2),
                    c("divide", 2))

  #within this function
  #do.call(functionSet[3,1], list(2, 3))
  #outside this function
  #do.call(arithmeticSet()[3,1], list(2, 3))

  return(constructFuncSet(functionDefs))
}

add <- function(x, y) {
  return(x + y)
}

subtract <- function(x, y) {
  return(x - y)
}

multiply <- function(x, y) {
  return(x * y)
}

divide <- function(x, y) {
  return (x / y)
}

#' trigonometricSet
#'
#' Creates a function set containing the trigonometric operators (sin, cos, tan)
#'
#' @return the function set created
#' @examples
#' trigonometricSet()
trigonometricSet <- function() {

  functionDefs <- c(c("sin", 1),
                    c("cos", 1),
                    c("tan", 1))

  return(constructFuncSet(functionDefs))
}

#' complexSet
#'
#' Creates a function set containing the log, exp and sqrt functions
#'
#' @return the function set created
#' @examples
#' complexSet()
complexSet <- function() {

  functionDefs <- c(c("log", 1),
                    c("log", 2),
                    c("exp", 1),
                    c("sqrt", 1))

  return(constructFuncSet(functionDefs))
}

#' mathOpSet
#'
#' Creates a function set containing the functions from the other sets defined
#' in the package
#'
#' @return the function set created
#' @examples
#' mathOpSet()
mathOpSet <- function() {

  return(rbind(arithmeticSet(), trigonometricSet(), complexSet()))
}

#' constructFuncSet
#'
#' Constructs a function set using the provided function definitions.
#'
#' @param functionDefs the function definitions
#'
#' @return the function set created
#' @examples
#' constructFuncSet(c(c("add", 2), c("sqrt", 1)))
constructFuncSet <- function(functionDefs) {

  functionSet <- data.frame(matrix(data = functionDefs, ncol = 2, byrow = TRUE),
                            stringsAsFactors = FALSE)

  colnames(functionSet) <- c("funcName", "arity")

  #Restore the integer value of arity
  functionSet <- transform(functionSet, arity = as.integer(arity))

  return(functionSet)
}
