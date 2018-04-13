#' arithmeticSet
#'
#' Creates a function set containing the basic arithmetic operators (+, -, *, /)
#'
#' @return the function set created
#' @export
#' @examples
#' arithmeticSet()
arithmeticSet <- function() {

  #functionDefs <- c(c("+", 2),
  #                  c("-", 2),
  #                  c("*", 2),
  #                  c("/", 2))

  functionDefs <- c(c("+", 2),
                    c("-", 2),
                    c("*", 2))

  return(constructFuncSet(functionDefs))
}

#' trigonometricSet
#'
#' Creates a function set containing the trigonometric operators (sin, cos, tan)
#'
#' @return the function set created
#' @export
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
#' @export
#' @examples
#' complexSet()
complexSet <- function() {

  #functionDefs <- c(c("log", 1),
  #                  c("log", 2),
  #                  c("exp", 1),
  #                  c("sqrt", 1),
  #                  c("abs", 1),
  #                  c("floor", 1),
  #                  c("ceiling", 1))

  functionDefs <- c(c("exp", 1),
                    c("sqrt", 1),
                    c("abs", 1),
                    c("floor", 1),
                    c("ceiling", 1))

  return(constructFuncSet(functionDefs))
}

#' mathOpSet
#'
#' Creates a function set containing the functions from the other sets defined
#' in the package
#'
#' @return the function set created
#' @export
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
#' @export
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
