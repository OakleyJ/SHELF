test_that("makeSurvivalTable works",{
  skip_on_cran()
  set.seed(123)
  expDf <- data.frame(time = c(rexp(10000, rate = 0.2),
                               rexp(10000, rate = 0.1)),
                      event = rep(2, 1000),
                      treatment = rep(c("drug", "placebo"), each = 10000))
                      
  t1 <- makeSurvivalTable(expDf, breakTime = 1, truncationTime = 10, 
                          timeUnit = "years")
  expect_equal(t1[, 2], 1-pexp(1:10, 0.2) , tolerance = 5e-3)
  expect_equal(t1[, 4], rep(0.2, 10) , tolerance = 5e-2)
  expect_equal(t1[, 5], 1-pexp(1:10, 0.1) , tolerance = 5e-3)
  expect_equal(t1[, 7], rep(0.1, 10) , tolerance = 5e-2)

})

test_that("ScenarioTest works",{
  set.seed(123)
  n <- 100
  expDf <- data.frame(time = rexp(100, rate = 0.2), event = rep(1, n),
                      treatment = "drug")
  sScen <- survivalScenario(0, 10, 4, 5, 10, expDf, showPlot = FALSE)
  expect_lt(sScen$interval[1], 1-pexp(10, 0.2))
  expect_gt(sScen$interval[2], 1-pexp(10, 0.2))
  
})