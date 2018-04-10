library(caRtesian)
context("fitness")

test_that("mae returns the correct result when its input contains NaN", {

  actual <- 1:5
  predicted <- c(1:4, NaN)
  data <- data.frame(actual = actual, predicted = predicted)

  result <- mae(data)

  expect_true(is.nan(result))

  actual <- c(1:4, NaN)
  predicted <- 1:5
  data <- data.frame(actual = actual, predicted = predicted)

  result <- mae(data)

  expect_true(is.nan(result))
})

test_that("rmse returns the correct result when its input contains NaN", {

  actual <- 1:5
  predicted <- c(1:4, NaN)
  data <- data.frame(actual = actual, predicted = predicted)

  result <- rmse(data)

  expect_true(is.nan(result))

  actual <- c(1:4, NaN)
  predicted <- 1:5
  data <- data.frame(actual = actual, predicted = predicted)

  result <- rmse(data)

  expect_true(is.nan(result))
})

test_that("checkSolutionFound returns the correct result", {

  population <- initPopulation(5, mathOpSet(), inputSize = 2,
                               outputSize = 1, rowsFuncNodes = 5,
                               colsFuncNodes = 5, levelsBack = 2)

  population[[1]]$fitness <- 10.8
  population[[2]]$fitness <- 88.9
  population[[3]]$fitness <- 0
  population[[4]]$fitness <- 2401.7
  population[[5]]$fitness <- 298

  expect_true(checkSolutionFound(population))

  population[[3]]$fitness <- 90

  expect_false(checkSolutionFound(population))

})

test_that("nodesToProcess returns the correct nodes when a functionNode is used", {

  set.seed(2)
  solution <- initPopulation(1, mathOpSet(), inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 5,
                             colsFuncNodes = 3, levelsBack = 2)[[1]]

  rowsToProcess <- c(4, 6, 13)
  actualNodesToProcess <- solution$functionNodes[rowsToProcess,]

  expect_equal(nodesToProcess(solution), actualNodesToProcess)

})

test_that("nodesToProcess returns no nodes when no functionNodes are used", {

  set.seed(123)
  solution <- initPopulation(1, mathOpSet(), inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 5,
                             colsFuncNodes = 3, levelsBack = 2)[[1]]

  result <- nodesToProcess(solution)

  expect_equal(nrow(result), 0)

})

test_that("traverseFunctionNodes returns the correct results for different input", {

  set.seed(1)
  solution <- initPopulation(1, arithmeticSet(), inputSize = 3,
                             outputSize = 1, rowsFuncNodes = 4,
                             colsFuncNodes = 5, levelsBack = 2)[[1]]

  functionNodes <- solution$functionNodes

  chromoID <- 1
  nodesUsed <- vector(mode = "logical", length = nrow(functionNodes))
  result <- traverseFunctionNodes(functionNodes, nodesUsed, chromoID)

  expect_true(all(result == FALSE))

  chromoID <- 6
  nodesUsed <- vector(mode = "logical", length = nrow(functionNodes))
  result <- traverseFunctionNodes(functionNodes, nodesUsed, chromoID)
  index <- which(functionNodes$chromoID == chromoID)

  expect_true(result[index])
  expect_true(all(result[-index] == FALSE))

  chromoID <- 24
  nodesUsed <- vector(mode = "logical", length = nrow(functionNodes))
  result <- traverseFunctionNodes(functionNodes, nodesUsed, chromoID)
  indexes <- c(1, 2, 4, 6, 7, 10, 12, 20)

  expect_true(all(result[indexes] == TRUE))
  expect_true(all(result[-indexes] == FALSE))

  chromoID <- 25
  nodesUsed <- vector(mode = "logical", length = nrow(functionNodes))
  result <- traverseFunctionNodes(functionNodes, nodesUsed, chromoID)

  expect_true(all(result == FALSE))
})

test_that("calculateValue returns the correct value for a given node", {

  functionSet <- arithmeticSet()

  set.seed(1)
  solution <- initPopulation(1, functionSet, inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 3,
                             colsFuncNodes = 3, levelsBack = 1)[[1]]

  solution$inputNodes$value[1] <- 5
  solution$inputNodes$value[2] <- 10
  solution$inputNodes$value[3] <- -5

  currentNode <- solution$functionNodes[1, ]
  expectedValue <- solution$inputNodes$value[2] - solution$inputNodes$value[3]
  predictedValue <- calculateValue(currentNode, solution, functionSet)

  expect_identical(predictedValue, expectedValue)

  solution$functionNodes[1, ]$value <- predictedValue
  currentNode <- solution$functionNodes[4, ]
  expectedValue <- expectedValue + solution$inputNodes$value[3]
  predictedValue <- calculateValue(currentNode, solution, functionSet)

  expect_identical(predictedValue, expectedValue)

  solution$functionNodes[4, ]$value <- predictedValue
  currentNode <- solution$functionNodes[8, ]
  expectedValue <- solution$inputNodes$value[1] - expectedValue
  predictedValue <- calculateValue(currentNode, solution, functionSet)

  expect_identical(predictedValue, expectedValue)
})

test_that("calculateValueInSolution calculates the value of the correct nodes", {

  functionSet <- mathOpSet()

  set.seed(2)
  solution <- initPopulation(1, functionSet, inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 4,
                             colsFuncNodes = 4, levelsBack = 1)[[1]]

  solution$inputNodes$value[1] <- -20
  solution$inputNodes$value[2] <- 103
  solution$inputNodes$value[3] <- 1002

  functionNodesUsed <- nodesToProcess(solution)

  solution <- calculateValueInSolution(solution, functionNodesUsed, functionSet)

  rowsWithNA <- is.na(solution$functionNodes$value)
  rowsWithValues <- solution$functionNodes[!rowsWithNA,]

  incorrectRows <- setdiff(functionNodesUsed$chromoID, rowsWithValues$chromoID)

  expect_equal(length(incorrectRows), 0)

  expect_false(is.na(solution$outputNodes$value[1]))

})

test_that("calculateValueInSolution handles where functionNodes are not used", {

  functionSet <- mathOpSet()

  set.seed(6)
  solution <- initPopulation(1, functionSet, inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 3,
                             colsFuncNodes = 3, levelsBack = 1)[[1]]

  solution

  solution$inputNodes$value[1] <- 5
  solution$inputNodes$value[2] <- 25
  solution$inputNodes$value[3] <- -6.7

  functionNodesUsed <- nodesToProcess(solution)

  solution <- calculateValueInSolution(solution, functionNodesUsed, functionSet)

  expect_true(all(is.na(solution$functionNodes$value)))

  expect_false(is.na(solution$outputNodes$value[1]))

})

test_that("calculateValue2 returns the correct value for a given node", {

  functionSet <- arithmeticSet()

  set.seed(1)
  solution <- initPopulation(1, functionSet, inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 3,
                             colsFuncNodes = 3, levelsBack = 1)[[1]]

  solution$inputNodes$value[1] <- 5
  solution$inputNodes$value[2] <- 10
  solution$inputNodes$value[3] <- -5

  currentNode <- solution$functionNodes[8, ]
  expectedValue <- 5 - ((10 - -5) + -5)
  predictedValue <- calculateValue2(currentNode, solution, functionSet)

  expect_identical(predictedValue, expectedValue)
})
