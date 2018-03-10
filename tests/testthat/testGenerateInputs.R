library(caRtesian)
context("Generate Inputs")

test_that("generateInputs returns a data frame of the correct dimensions", {
  inputSize <- 5
  inputs <- generateInputs(inputSize)
  expect_equal(2, ncol(inputs))
  expect_equal(inputSize, nrow(inputs))

  inputSize <- 10
  inputs <- generateInputs(inputSize)
  expect_equal(2, ncol(inputs))
  expect_equal(inputSize, nrow(inputs))
})
