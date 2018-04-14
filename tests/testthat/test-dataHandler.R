library(caRtesian)
context("dataHandler")

test_that("calculateInputSize returns the correct size", {

  model <- output ~ x + y

  expect_equal(calculateInputSize(model), 2)

  model <- output ~ x + y + z

  expect_equal(calculateInputSize(model), 3)

})

test_that("calculateOutputSize returns the correct size", {

  model <- output ~ x + y

  expect_equal(calculateOutputSize(model), 1)

  model <- class + output ~ x + y

  expect_equal(calculateOutputSize(model), 2)
})

test_that("validSelectionInput returns the correct logical value", {


  arguments <- list(func = muLambdaStrategy, args = c(population = NA, 9, 2))

  expect_true(validSelectionInput(arguments))

  arguments <- list(func = muLambdaStrategy, c(population = NA, 9, 2))

  expect_false(validSelectionInput(arguments))

  arguments <- list(func = muLambdaStrategy, args = c(NA, 9, 2))

  expect_false(validSelectionInput(arguments))

  arguments <- list(func = muLambdaStrategy, c(population = 1, 9, 2))

  expect_false(validSelectionInput(arguments))

})
