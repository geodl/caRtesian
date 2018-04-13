library(caRtesian)
context("main")

test_that("cgp returns a list with the correct structure", {

  pop <- cgp(dataset = read.csv("./data/x_squared_minus_y.csv"),
             model = output ~ x + y,
             maxGenerations = 1,
             rowsFuncNodes = 5,
             colsFuncNodes = 5,
             levelsBack = 2,
             popSize = 5)

})
