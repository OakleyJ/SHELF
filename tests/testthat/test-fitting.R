
test_that("normal distribution fitting and feedback works",{
  skip_on_cran()
  m <- 10
  s <- 20
  vals <- c(m - s, m , m + 2 * s)
  myfit <- fitdist(vals, pnorm(vals, m, s ))
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(m -0.5*s, m+s))
  norm.parameters <- unlist(myfit$Normal)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(norm.parameters) <- NULL
  expect_equal(norm.parameters, c(m, s))
  expect_equal(best.name, "Normal")
  expect_equal(fb$fitted.quantiles[, 1], 
               signif(qnorm(c(0.05, 0.95), m, s),3))
  expect_equal(fb$fitted.probabilities[, 1],
               signif(pnorm(c(m -0.5*s, m+s), m, s),3))
})

test_that("student-t distribution fitting and feedback works",{
  skip_on_cran()
  m <- 10
  s <- 20
  tdftest <- 4
  vals <- c(m - s, m , m + 2 * s)
  myfit <- fitdist(vals, pt((vals-m)/s, tdftest ), tdf = tdftest)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(m -0.5*s, m+s))
  t.parameters <- unlist(myfit$Student.t)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(t.parameters) <- NULL
  expect_equal(t.parameters, c(m, s, tdftest), tolerance = 0.001)
  expect_equal(best.name, "Student-t")
  expect_equal(fb$fitted.quantiles[, 2], 
               signif(m + s * qt(c(0.05, 0.95), tdftest),3))
  expect_equal(fb$fitted.probabilities[, 2],
               signif(pt(c( -0.5, 1), tdftest),3))
})


test_that("log-t distribution fitting and feedback works",{
  skip_on_cran()
  m <- log(30)
  s <- 0.5
  tdftest <- 5
  vals <- c(22, 30, 42)
  myfit <- fitdist(vals, pt((log(vals) - m) / s, tdftest ), lower = 0, tdf = tdftest)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(25, 55))
  lt.parameters <- unlist(myfit$Log.Student.t)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(lt.parameters) <- NULL
  expect_equal(lt.parameters, c(m, s, tdftest), tolerance = 0.001)
  expect_equal(best.name, "Log Student-t")
  expect_equal(fb$fitted.quantiles[, 5], 
               signif(exp(m + s * qt(c(0.05, 0.95), tdftest)), 3))
  expect_equal(fb$fitted.probabilities[, 5],
               signif(pt((log(c(25, 55)) - m )/s, tdftest), 3))
})


test_that("scaled beta distribution fitting and feedback works",{
  skip_on_cran()
  a <- 5
  b <- 20
  l <- 10
  u <- 60
  vals <- c(18, 20, 24)
  myfit <- fitdist(vals, pbeta((vals-l)/(u-l), a, b ), lower = l, upper = u)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(19, 29))
  beta.parameters <- unlist(myfit$Beta)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(beta.parameters) <- NULL
  expect_equal(beta.parameters, c(a, b), tolerance = 0.001)
  expect_equal(best.name, "Beta")
  expect_equal(fb$fitted.quantiles[, 6], 
               signif(l + (u-l) * qbeta(c(0.05, 0.95), a, b),3))
  expect_equal(fb$fitted.probabilities[, 6],
               signif(pbeta((c(19, 29)-l)/(u-l), a, b),3))
})

test_that("shifted lognormal distribution fitting and feedback works",{
  skip_on_cran()
  l <- -100
  m <- log(30)
  s <- 0.5
  vals <- c(22, 30, 42) + l
  myfit <- fitdist(vals, plnorm(vals - l, m, s ), lower = l)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(25, 55) + l)
  lnorm.parameters <- unlist(myfit$Log.normal)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(lnorm.parameters) <- NULL
  expect_equal(lnorm.parameters, c(m, s), tolerance = 0.001)
  expect_equal(best.name, "Log normal")
  expect_equal(fb$fitted.quantiles[, 4], 
               signif(l + qlnorm(c(0.05, 0.95), m, s),3))
  expect_equal(fb$fitted.probabilities[, 4],
               signif(plnorm(c(25, 55), m, s),3))
})

test_that("shifted lognormal distribution fitting and feedback works",{
  skip_on_cran()
  m <- 2
  s <- 0.5
  l <- -10
  vals <- c(-6, -2, 2, 6)
  myfit <- fitdist(vals, plnorm(vals - l, m, s ), lower = l)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(-4, 4))
  lnorm.parameters <- unlist(myfit$Log.normal)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(lnorm.parameters) <- NULL
  expect_equal(lnorm.parameters, c(m, s), tolerance = 0.001)
  expect_equal(best.name, "Log normal")
  expect_equal(fb$fitted.quantiles[, 4], 
               signif(qlnorm(c(0.05, 0.95), m, s)+l, 3) )
  expect_equal(fb$fitted.probabilities[, 4],
               signif(plnorm(c(-4, 4) - l, m, s),3))
})



test_that("shifted gamma distribution fitting and feedback works",{
  skip_on_cran()
  a <- 50
  b <- 2
  l <- 10
  vals <- c(32, 35, 37)
  myfit <- fitdist(vals, pgamma(vals-l, a, b ), lower = l)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(33, 40))
  gamma.parameters <- unlist(myfit$Gamma)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(gamma.parameters) <- NULL
  expect_equal(gamma.parameters, c(a, b), tolerance = 0.001)
  expect_equal(best.name, "Gamma")
  expect_equal(fb$fitted.quantiles[, 3], 
               signif(l + qgamma(c(0.05, 0.95), a, b),3))
  expect_equal(fb$fitted.probabilities[, 3],
               signif(pgamma(c(33, 40)-l, a, b),3))
})

test_that("precision fitting works - normal",{
  skip_on_cran()
  med <- 10
  k <- 1
  # sigma^-2 ~ gamma(a, b)
  a <- 3
  b <- 4
  sigmasq <- 1 / qgamma(c(0.05, 0.95), a, b)
  probs <- pnorm(rep(med + k, 2), med, sigmasq^0.5) - 0.5
  pfit <- fitprecision(c(med, med + k), probs, pplot = F)
  gamma.parameters <- unlist(pfit$Gamma)
  attributes(gamma.parameters) <- NULL
  expect_equal(gamma.parameters, c(a, b), tolerance = 1e-4)
})

test_that("precision fitting works - lognormal",{
  skip_on_cran()
  med <- 10
  k <- 5
  # sigma^-2 ~ gamma(a, b)
  a <- 3
  b <- 4
  sigmasq <- 1 / qgamma(c(0.05, 0.95), a, b)
  probs <- plnorm(rep(med + k, 2), log(med), sigmasq^0.5) - 0.5
  pfit <- fitprecision(interval = c(med, med + k), propvals = probs,
                       trans = "log", pplot = F)
  gamma.parameters <- unlist(pfit$Gamma)
  attributes(gamma.parameters) <- NULL
  expect_equal(gamma.parameters, c(a, b), tolerance = 1e-4)
})
