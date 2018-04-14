library(caRtesian)
context("funcSet")

test_that("constructFuncSet returns a data frame of the correct structure", {

  dummyFunctions <- c(c("add", 2),
                      c("subtract", 2),
                      c("sqrt", 1))

  maxColumns <- 2

  funcSet <- constructFuncSet(dummyFunctions)
  expect_equal(ncol(funcSet), maxColumns)
  expect_equal(nrow(funcSet), length(dummyFunctions) / maxColumns)

  expect_is(funcSet, "data.frame")
  expect_is(funcSet$funcName, "character")
  expect_is(funcSet$arity, "integer")
})

test_that("mathOpSet returns a data frame with the correct number of rows", {

  set1 <- arithmeticSet()
  set2 <- trigonometricSet()
  set3 <- complexSet()
  mathSet <- mathOpSet()

  total <- nrow(set1) + nrow(set2) +  nrow(set3)

  expect_equal(nrow(mathSet), total)
})
