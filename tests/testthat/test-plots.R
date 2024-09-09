test_that("multiple expert plot works", {
  skip_on_cran()
  v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
  p <- c(0.25, 0.5, 0.75)
  myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
  p <- plotfit(myfit, showPlot = FALSE, returnPlot = TRUE)
  vdiffr::expect_doppelganger("multiple expert plot", p)
})

test_that("multiple expert linear pool plot works", {
  skip_on_cran()
  v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
  p <- c(0.25, 0.5, 0.75)
  myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
  p <- plotfit(myfit, d = "gamma",  lp = T, lpw = c(2,1), ql = 0.05, qu = 0.95, ind=FALSE,
               showPlot = FALSE, returnPlot = TRUE)
  vdiffr::expect_doppelganger("multiple expert linear pool plot", p)
})


test_that("single expert plot works", {
  skip_on_cran()
  v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
  p <- c(0.25, 0.5, 0.75)
  myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
  p <- plotfit(myfit, d = "beta", ql = 0.05, qu = 0.95, ex = 2, showPlot = FALSE,
               returnPlot = TRUE)
  vdiffr::expect_doppelganger("single expert plot", p)
})

test_that("exponential plot handling works", {
  skip_on_cran()
  v <- 1
  p <- 0.5
  myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
  p <- plotfit(myfit, d = "skewnormal", returnPlot = TRUE, showPlot = FALSE)
  vdiffr::expect_doppelganger("error message plot", p)
  p <- plotfit(myfit, d = "gamma", returnPlot = TRUE, showPlot = FALSE)
  vdiffr::expect_doppelganger("exponential distribution plot", p)
})

test_that("single expert plot works - histogram", {
  skip_on_cran()
  v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
  p <- c(0.25, 0.5, 0.75)
  myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
  p <- plotfit(myfit, d = "hist", ql = 0.05, qu = 0.95, ex = 2, showPlot = FALSE,
               returnPlot = TRUE)
  vdiffr::expect_doppelganger("single expert histogram plot", p)
})

test_that("CDF plot works", {
  skip_on_cran()
  vQuartiles <- c(30, 35, 45)
  pQuartiles<- c(0.25, 0.5, 0.75)
  myfit <- fitdist(vals = vQuartiles, probs = pQuartiles, lower = 0)
  p  <- makeCDFPlot(lower = 0, v = vQuartiles, p = pQuartiles,
              upper = 100, fit = myfit, dist = "lognormal",
              showFittedCDF = TRUE, showQuantiles = TRUE)
  
  vdiffr::expect_doppelganger("CDF plot", p)
})

test_that("quartile plot works", {
  skip_on_cran()
  l <- c(2, 1, 5, 1)
  u <- c(95, 90, 65, 40)
  v <- matrix(c(15, 25, 40,
                10, 20, 40,
                10, 15, 25,
                5, 10, 20),
              3, 4)
  p <- plotQuartiles(vals = v, lower = l, upper  = u)
  
  vdiffr::expect_doppelganger("quartile plot", p)
})

test_that("tertile plot works", {
  skip_on_cran()
  l <- c(-5, 0, 5, -10)
  u <- c(15, 35, 50, 35)
  v <- matrix(c(5, 8, 10,
                10, 15, 20,
                15, 18, 25,
                10, 20, 30),
              3, 4)
  p <- plotTertiles(vals = v, lower = l, upper  = u)
  vdiffr::expect_doppelganger("tertile plot", p)
})


test_that("distributions CDF plot works", {
  skip_on_cran()
  prfit <- fitprecision(interval = c(60, 70), propvals = c(0.2, 0.4), trans = "log",
                        pplot = FALSE)
  medianfit <- fitdist(vals = c(50, 60, 70), probs = c(0.05, 0.5,  0.95), lower = 0)
  p <- cdfplot(medianfit, prfit)
  vdiffr::expect_doppelganger("distributions CDF plot", p)
})

# test_that("compare group RIO plot works", {
#   skip_on_cran()
#   l <- c(2, 1, 5, 1)
#   u <- c(95, 90, 65, 40)
#   v <- matrix(c(15, 25, 40,
#                 10, 20, 40,
#                 10, 15, 25,
#                 5, 10, 20),
#               3, 4)
#   p <- c(0.25, 0.5, 0.75)
#   group <- fitdist(vals = v, probs = p, lower = l, upper = u)
#   rio <- fitdist(vals = c(12, 20, 25), probs = p, lower = 1, upper = 100)
#   p <- compareGroupRIO(groupFit = group, RIOFit = rio, dRIO = "skewnormal")
#   vdiffr::expect_doppelganger("group RIO CDF plot", p)
# })

test_that("compare interval plot works", {
  skip_on_cran()
  v <- matrix(c(30, 40, 50, 20, 25, 35, 40, 50, 60, 35, 40, 50), 3, 4)
  p <- c(0.25, 0.5, 0.75)
  myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
  p <- compareIntervals(myfit, interval = 0.5, showDist = FALSE)
  vdiffr::expect_doppelganger("compare interval plot", p)
})

test_that("survival model extrapolations works", {
  skip_on_cran()
  sdf <- survival::veteran[, c("time", "status", "trt")]
  colnames(sdf) <- c("time", "event", "treatment")
  sdf$treatment <- factor(sdf$treatment, labels = c("standard", "test"))
  
  p <- survivalModelExtrapolations(sdf, tEnd = 1000,
                              group = "test",
                              tTruncate = 100,
                              showPlot = FALSE)
  
  vdiffr::expect_doppelganger("survival extrapolations plot", p$KMplot)
})

test_that("survival scenario testing works", {
  skip_on_cran()
  sdf <- survival::veteran[, c("time", "status", "trt")]
  colnames(sdf) <- c("time", "event", "treatment")
  sdf$treatment <- factor(sdf$treatment, labels = c("standard", "test"))
  p <- survivalScenario(tLower = 0,tUpper = 150, expLower = 100, expUpper = 150,
                   tTarget = 250, survDf = sdf,
                   expGroup = "standard",
                   showPlot = FALSE)
  
  
  vdiffr::expect_doppelganger("survival scenario plot", p$KMplot)
})
