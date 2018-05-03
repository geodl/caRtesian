library(caRtesian)
context("selection")

test_that("muLambdaStrategy returns the correct number of results", {

  population <- initPopulation(5, mathOpSet(), inputSize = 2,
                               outputSize = 1, rowsFuncNodes = 5,
                               colsFuncNodes = 5, levelsBack = 2)

  population[[1]]$fitness <- 5
  population[[2]]$fitness <- 6
  population[[3]]$fitness <- 90
  population[[4]]$fitness <- 22
  population[[5]]$fitness <- 150


  functionNodeStructure <- list(rows = 5,
                                cols = 5,
                                levelsBack = 2,
                                functionSet = mathOpSet())

  lambda <- 4

  newPopulation <- muLambdaStrategy(population, lambda, functionNodeStructure)

  expect_equal(length(newPopulation), 1 + lambda)

})

test_that("mutateFunction works correctly for nodes with no valid functionNode inputs", {

  set.seed(1)
  solution <- initPopulation(1, mathOpSet(), inputSize = 2,
                               outputSize = 1, rowsFuncNodes = 5,
                               colsFuncNodes = 5, levelsBack = 2)[[1]]

  chromoID <- 4

  functionNodeStructure <- list(rows = 5,
                                cols = 5,
                                levelsBack = 2,
                                functionSet = mathOpSet())

  set.seed(1)
  mutatedSolution <- mutateFunction(solution, chromoID, functionNodeStructure)

  numInputs <- length(unlist(mutatedSolution$functionNodes[1, ]$inputs))

  functionSet <- functionNodeStructure$functionSet
  arity <- functionSet[mutatedSolution$functionNodes[1, ]$funcID, ]$arity

  expect_equal(numInputs, arity)

  set.seed(4)
  mutatedSolution <- mutateFunction(solution, chromoID, functionNodeStructure)

  numInputs <- length(unlist(mutatedSolution$functionNodes[1, ]$inputs))

  functionSet <- functionNodeStructure$functionSet
  arity <- functionSet[mutatedSolution$functionNodes[1, ]$funcID, ]$arity

  expect_equal(numInputs, arity)
})

test_that("mutateInput works correctly for functionNodes with one input", {

  set.seed(5)
  solution <- initPopulation(1, mathOpSet(), inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 5,
                             colsFuncNodes = 5, levelsBack = 2)[[1]]

  functionNodeStructure <- list(rows = 5,
                                cols = 5,
                                levelsBack = 2,
                                functionSet = mathOpSet())

  chromoID <- 23

  mutation <- mutateInput(solution, chromoID, functionNodeStructure)

  expect_equal(length(mutation), 1)

})

test_that("mutateInput works correctly for functionNodes with two inputs", {

  set.seed(5)
  solution <- initPopulation(1, mathOpSet(), inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 5,
                             colsFuncNodes = 5, levelsBack = 2)[[1]]

  functionNodeStructure <- list(rows = 5,
                                cols = 5,
                                levelsBack = 2,
                                functionSet = mathOpSet())

  chromoID <- 28

  mutation <- mutateInput(solution, chromoID, functionNodeStructure)

  expect_equal(length(mutation), 2)

})

test_that("mutateInput works correctly for outputNodes", {

  solution <- initPopulation(1, mathOpSet(), inputSize = 3,
                             outputSize = 1, rowsFuncNodes = 4,
                             colsFuncNodes = 4, levelsBack = 2)[[1]]

  functionNodeStructure <- list(rows = 4,
                                cols = 4,
                                levelsBack = 2,
                                functionSet = mathOpSet())

  chromoID <- solution$outputNodes$chromoID[1]

  mutation <- mutateInput(solution, chromoID, functionNodeStructure)

  expect_equal(length(mutation), 1)

})
