library(caRtesian)
context("population")

test_that("generateInputs returns a data frame of the correct structure", {

  maxColumns <- 2

  inputSize1 <- 5 #5 input variables
  constants <- 1 #1 constant
  inputs1 <- generateInputs(inputSize1)
  expect_equal(ncol(inputs1), maxColumns)
  expect_equal(nrow(inputs1), inputSize1 + constants)

  expect_is(inputs1, "data.frame")
  expect_is(inputs1$chromoID, "integer")
  expect_is(inputs1$value, "character")

  inputSize2 <- 10 #10 input variables

  inputs2 <- generateInputs(inputSize2)
  expect_equal(ncol(inputs2), maxColumns)
  expect_equal(nrow(inputs2), inputSize2 + constants)

  expect_is(inputs2, "data.frame")
})

test_that("generateFunctionNodes returns a data frame of the correct structure", {

  maxColumns <- 4

  functionNodes1 <- generateFunctionNodes(startID = 5,
                                         nrows = 5,
                                         ncols = 5,
                                         levelsBack = 2,
                                         functionSet = arithmeticSet())
  expect_equal(ncol(functionNodes1), maxColumns)
  expect_equal(nrow(functionNodes1), 5 * 5)

  expect_is(functionNodes1, "data.frame")
  expect_is(functionNodes1$chromoID, "integer")
  expect_is(functionNodes1$value, "numeric")
  expect_is(functionNodes1$funcID, "integer")
  expect_is(functionNodes1$inputs, "list")

  functionNodes2 <- generateFunctionNodes(startID = 2,
                                          nrows = 10,
                                          ncols = 8,
                                          levelsBack = 4,
                                          functionSet = mathOpSet())
  expect_equal(ncol(functionNodes2), maxColumns)
  expect_equal(nrow(functionNodes2), 10 * 8)
})

test_that("generateOutputs returns a data frame of the correct structure", {

  maxColumns <- 3

  outputSize1 <- 1
  outputNodes1 <- generateOutputs(startID = 10,
                                  maxInputID = 9,
                                  outputSize = outputSize1)
  expect_equal(ncol(outputNodes1), maxColumns)
  expect_equal(nrow(outputNodes1), outputSize1)

  expect_is(outputNodes1, "data.frame")
  expect_is(outputNodes1$chromoID, "integer")
  expect_is(outputNodes1$value, "numeric")
  expect_is(outputNodes1$inputs, "integer")

  outputSize2 <- 3
  outputNodes2 <- generateOutputs(startID = 100,
                                  maxInputID = 99,
                                  outputSize = outputSize2)
  expect_equal(ncol(outputNodes2), maxColumns)
  expect_equal(nrow(outputNodes2), outputSize2)
})

test_that("createFunctionNodeStructure returns a data frame with the correct
          structure", {

  maxColumns <- 4

  rowsReq1 <- 10
  structure1 <- createFunctionNodesStructure(rowsRequired = rowsReq1)
  expect_equal(ncol(structure1), maxColumns)
  expect_equal(nrow(structure1), rowsReq1)

  expect_is(structure1, "data.frame")
  expect_is(structure1$chromoID, "integer")
  expect_is(structure1$value, "numeric")
  expect_is(structure1$funcID, "integer")
  expect_is(structure1$inputs, "list")

  expect_true(all(is.na(structure1$chromoID)))
  expect_true(all(is.na(structure1$value)))
  expect_true(all(is.na(structure1$funcID)))
  expect_true(all(sapply(structure1$inputs, is.null)))

})

test_that("makeFunctionNode returns a data frame with the correct structure", {

  maxColumns <- 4
  maxRows <- 1

  node1 <- makeFunctionNode(chromoID = 7,
                            validInputs = c(1, 2, 3, 4, 5),
                            functionSet = arithmeticSet())
  expect_equal(ncol(node1), maxColumns)
  expect_equal(nrow(node1), maxRows)

  expect_is(node1, "data.frame")
  expect_is(node1$chromoID, "integer")
  expect_is(node1$value, "numeric")
  expect_is(node1$funcID, "integer")
  expect_is(node1$inputs, "list")

})

test_that("makeFunctionNode assigns the correct number of valid inputs", {

  functionSet <- arithmeticSet()

  node1 <- makeFunctionNode(chromoID = 10,
                            validInputs = c(1, 2, 6, 7, 8),
                            functionSet = functionSet)
  chosenFunc <- node1[1, ]$funcID
  arity <- functionSet[chosenFunc, ]$arity
  numInputsChosen <- sapply(node1$inputs, length)
  expect_equal(arity, numInputsChosen)

})

test_that("updateValidInputs updates the correct row with the correct values", {

  previousValidInputs <- matrix(c(1, 2, NA, NA,
                                  7, 8, 9, 10,
                                  11, 12, 13, 14),
                                nrow = 3,
                                ncol = 4,
                                byrow = TRUE)

  rowToRemove <- previousValidInputs[2, ]
  rowToAdd <- c(15, 16, 17, 18)

  newValidInputs <- updateValidInputs(row = 2,
                                      level = rowToAdd,
                                      validInputs = previousValidInputs)

  expect_true(all(is.element(rowToAdd, newValidInputs)))
  expect_false(all(is.element(rowToRemove, newValidInputs)))
})

test_that("updateValidInputs handles where validInputs has more columns
          than level has in length", {

  columns <- 5
  rowToAdd <- c(11, 12)

  previousValidInputs <- matrix(c(1, 2, 3, 4, 5,
                                6, 7, 8, 9, 10),
                                nrow = 2,
                                ncol = columns,
                                byrow = TRUE)

  newValidInputs <- updateValidInputs(row = 2,
                                      level = rowToAdd,
                                      validInputs = previousValidInputs)

  startPoint <- length(rowToAdd) + 1

  expect_true(all(is.na(newValidInputs[2, startPoint:columns])))
  expect_false(all(is.na(newValidInputs[2, 1:columns])))
})

test_that("initPopulation returns the correct number of solutions", {

  popsize1 = 5
  population1 <- initPopulation(popsize1, arithmeticSet(), inputSize = 2,
                                outputSize = 1, rowsFuncNodes = 2,
                                colsFuncNodes = 2, levelsBack = 2)

  expect_equal(length(population1), popsize1)

  popsize2 = 10
  population2 <- initPopulation(popsize2, mathOpSet(), inputSize = 5,
                                outputSize = 1, rowsFuncNodes = 6,
                                colsFuncNodes = 3, levelsBack = 3)

  expect_equal(length(population2), popsize2)
})

test_that("initPopulation returns a list with the correct structure", {

  population <- initPopulation(5, mathOpSet(), inputSize = 4,
                               outputSize = 1, rowsFuncNodes = 10,
                               colsFuncNodes = 3, levelsBack = 4)

  keys <- sapply(population, names)

  expect_true(all(sapply(keys[1,], function(x){ x == "inputNodes"})))
  expect_true(all(sapply(keys[2,], function(x){ x == "functionNodes"})))
  expect_true(all(sapply(keys[3,], function(x){ x == "outputNodes"})))

  expect_true(all(apply(keys, MARGIN = 2, FUN = function(x){
    return(x[1] == "inputNodes" &&
             x[2] == "functionNodes" &&
             x[3] == "outputNodes")
  })))
})
