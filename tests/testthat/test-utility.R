library(caRtesian)
context("utility")

test_that("sampleWithoutBiasOrNA handles NA values correctly", {

  expect_equal(length(sampleWithoutBiasOrNA(NA)), 0)

})

test_that("getValidInputs returns the correct inputs", {

  chromoID <- 26
  functionNodeRange <- 4:28
  functionNodeStructure <- list(rows = 5, cols = 5, levelsBack = 2,
                                functionSet = mathOpSet())

  correctValidInputs <- matrix(14:23, nrow = 5)

  validInputs <- getValidInputs(chromoID, functionNodeRange,
                                functionNodeStructure)

  expect_equal(validInputs, as.vector(correctValidInputs))

})

test_that("getValidInputs handles when chromoID is in the first column", {

  chromoID <- 7
  functionNodeRange <- 5:28
  functionNodeStructure <- list(rows = 3, cols = 8, levelsBack = 1,
                                functionSet = mathOpSet())

  dummy <- matrix(0, nrow = 3, ncol = 8)
  correctValidInputs <- dummy[, integer(0)]

  validInputs <- getValidInputs(chromoID, functionNodeRange,
                                functionNodeStructure)

  expect_equal(validInputs, as.vector(correctValidInputs))

})

test_that("is.functionNode returns the correct result for different input", {

  solution <- initPopulation(1, arithmeticSet(), inputSize = 3,
                             outputSize = 1, rowsFuncNodes = 4,
                             colsFuncNodes = 4, levelsBack = 2)[[1]]

  minID <- min(solution$functionNodes$chromoID)
  maxID <- max(solution$functionNodes$chromoID)

  chosenNode <- sample(minID:maxID, size = 1)

  expect_true(is.functionNode(solution, chosenNode))

  expect_false(is.functionNode(solution, solution$inputNodes$chromoID[2]))
  expect_false(is.functionNode(solution, solution$outputNodes$chromoID[1]))

})

test_that("is.outputNode returns the correct result for different input", {

  solution <- initPopulation(1, arithmeticSet(), inputSize = 3,
                             outputSize = 1, rowsFuncNodes = 4,
                             colsFuncNodes = 4, levelsBack = 2)[[1]]

  chosenNode <- min(solution$outputNodes$chromoID)

  expect_true(is.outputNode(solution, chosenNode))

  expect_false(is.outputNode(solution, solution$inputNodes$chromoID[2]))
  expect_false(is.outputNode(solution, solution$functionNodes$chromoID[3]))

})

test_that("findRow returns the correct row", {

  solution <- initPopulation(1, mathOpSet(), inputSize = 2,
                             outputSize = 1, rowsFuncNodes = 6,
                             colsFuncNodes = 3, levelsBack = 3)[[1]]

  chromoID <- 2
  rowReturned <- findRow(solution, chromoID)
  actualRow <- solution$inputNodes[2, ]

  expect_equal(rowReturned, actualRow)

  chromoID <- 8
  rowReturned <- findRow(solution, chromoID)
  actualRow <- solution$functionNodes[5, ]

  expect_equal(rowReturned, actualRow)

  chromoID <- 22
  rowReturned <- findRow(solution, chromoID)
  actualRow <- solution$outputNodes[1, ]

  expect_equal(rowReturned, actualRow)

})

test_that("sortPopulation orders the population correctly", {

  population <- initPopulation(5, arithmeticSet(), inputSize = 2,
                               outputSize = 1, rowsFuncNodes = 3,
                               colsFuncNodes = 3, levelsBack = 2)

  population[[1]]$fitness <- 10
  population[[2]]$fitness <- 8
  population[[3]]$fitness <- 7
  population[[4]]$fitness <- 5
  population[[5]]$fitness <- 2

  sorted <- sortPopulation(population)

  expect_equal(sorted, population[5:1])

})

test_that("sortPopulation correctly puts NA values last", {

  population <- initPopulation(5, arithmeticSet(), inputSize = 2,
                               outputSize = 1, rowsFuncNodes = 3,
                               colsFuncNodes = 3, levelsBack = 2)

  population[[1]]$fitness <- 1
  population[[2]]$fitness <- 2
  population[[3]]$fitness <- NA
  population[[4]]$fitness <- 5
  population[[5]]$fitness <- 10

  sorted <- sortPopulation(population)

  population <- list(population[[1]],
                     population[[2]],
                     population[[4]],
                     population[[5]],
                     population[[3]])

  expect_equal(sorted, population)
})

test_that("sortPopulation correctly puts Inf values last", {

  population <- initPopulation(5, arithmeticSet(), inputSize = 2,
                               outputSize = 1, rowsFuncNodes = 3,
                               colsFuncNodes = 3, levelsBack = 2)

  population[[1]]$fitness <- 1
  population[[2]]$fitness <- 2
  population[[3]]$fitness <- Inf
  population[[4]]$fitness <- Inf
  population[[5]]$fitness <- 10

  sorted <- sortPopulation(population)

  population <- list(population[[1]],
                     population[[2]],
                     population[[5]],
                     population[[3]],
                     population[[4]])

  expect_equal(sorted, population)
})
