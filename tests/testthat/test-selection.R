library(caRtesian)
context("selection")

test_that("muLambdaStrategy returns the correct number of results", {

  population <- initPopulation(5, mathOpSet(), inputSize = 2,
                               outputSize = 1, rowsFuncNodes = 5,
                               colsFuncNodes = 5, levelsBack = 2)




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
