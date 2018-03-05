arithmeticSet <- function() {

  functionNames <- c("add", "subtract", "multiply", "divide")
  arity <- c(2, 2, 2, 2)
  functionSet.df <- data.frame(functionNames, arity, stringsAsFactors = FALSE)

  #do.call(functionSet.df[3,1], list(2, 3))

  return(functionSet.df)
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
