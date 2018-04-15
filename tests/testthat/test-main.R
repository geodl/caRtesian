library(caRtesian)
context("main")

test_that("cgp returns a list with the correct structure", {

  #Block output
  invisible(capture.output(
    result <- cgp(dataset = read.csv("../../data/sin_vs_time.csv"),
                  model = y ~ x,
                  maxGenerations = 1,
                  rowsFuncNodes = 3,
                  colsFuncNodes = 3,
                  levelsBack = 1,
                  updateFreq = 0)
  ))

  expect_is(result, "list")
  expect_is(result$bestSolution, "list")
  expect_is(result$textualFormat, "character")
  expect_is(result$functionSet, "data.frame")
  expect_is(result$plotData, "data.frame")

  expect_equal(names(result$plotData), c("generation",
                                         "bestFitness",
                                         "averageFitness"))

})
