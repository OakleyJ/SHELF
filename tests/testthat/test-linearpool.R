test_that("linear pooling works",{
  skip_on_cran()
  #p1 <- c(runif(1, 0.1, 0.4), 0.5, runif(1, 0.6, 0.9))
  p1 <- c(0.25, 0.5, 0.75)
  a <- 10; b <- 4
  v1 <- qgamma(p1, a, b)
  mu <- 3 ; sigma <- 2
  v2 <- qnorm(p1, mu, sigma)
  v3 <- qlnorm(p1, log(mu), sigma)
  V <- matrix(c(v1, v2, v3), 3, 3)
  myfit <- fitdist(vals = V, probs = p1, lower = 0)
  
  w1 <- 1/6; w2 <- 2/6; w3 <- 3/6
  xtest <- 1.5
  qu <- 0.95
  
  qlp <- qlinearpool(myfit, qu, w = c(w1, w2, w3))
  qcheck <- w1 * pgamma(qlp, a, b) + 
    w2 * pnorm(qlp, mu, sigma) +
    w3 * plnorm(qlp, log(mu), sigma)
  expect_equal(qcheck, qu , tolerance = 1e-4)
  
  expect_equal(plinearpool(myfit, qlp, w = c(w1, w2, w3)),
               qu , tolerance = 1e-4)
  
  plp <- plinearpool(myfit, x = xtest, w = c(w1, w2, w3))
  pcheck <- w1 * pgamma(xtest, a, b) + 
    w2 * pnorm(xtest, mu, sigma) +
    w3 * plnorm(xtest, log(mu), sigma)
  expect_equal(plp, pcheck , tolerance = 1e-4)
})

test_that("linear pooling works - different lower limits",{
  skip_on_cran()
  llimits <- c(-2, 1, -4)
  p1 <- c(0.25, 0.5, 0.6, 0.75)
  a <- 10; b <- 4
  v1 <- llimits[1] + qgamma(p1, a, b)
  mu <- 3 ; sigma <- 2
  v2 <- llimits[2] + qlnorm(p1, log(mu), sigma)
  v3 <- llimits[3] + exp(1 + 2 * qt(p1, 3))
  V <- matrix(c(v1, v2, v3), length(p1), 3)
  myfit <- fitdist(vals = V, probs = p1, lower = llimits)
  
  w1 <- 1/6; w2 <- 2/6; w3 <- 3/6
  xtest <- 3
  qu <- 0.03
  
  qlp <- qlinearpool(myfit, qu, w = c(w1, w2, w3))
  qcheck <- w1 * pgamma(qlp - llimits[1], a, b) + 
    w2 * plnorm(qlp - llimits[2], log(mu), sigma) +
    w3 * pt((log(qlp - llimits[3]) - 1) / 2 , 3)
  expect_equal(qcheck, qu , tolerance = 1e-4)
  
  expect_equal(plinearpool(myfit, qlp, w = c(w1, w2, w3)),
               qu , tolerance = 1e-4)
  
  plp <- plinearpool(myfit, x = xtest, w = c(w1, w2, w3))
  pcheck <- w1 * pgamma(xtest - llimits[1], a, b) + 
    w2 * plnorm(xtest - llimits[2], log(mu), sigma) +
    w3 * pt((log(xtest - llimits[3]) - 1) / 2, 3)
  expect_equal(plp, pcheck , tolerance = 1e-4)
})

test_that("linear pool sampling ",{
  skip_on_cran()
  set.seed(123)
  m1 <- 10; m2 <- 20; m3 <- 40
  s1 <- 1; s2<- 2; s3 <- 3
  p <- c(0.25, 0.5, 0.75)
  N <- 1000
  v <- matrix(c(qnorm(p, m1, s1),
                qnorm(p, m2, s2),
                qnorm(p, m3, s3)),
              nrow = 3, ncol = 3)
  myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
  
  x <- rlinearpool(myfit, n = N)
  se <- sqrt(var(x) / N)
  expect_equal(mean(x), mean(c(m1, m2, m3)), tolerance = 4 * se)
  
  weights <- c(0.1, 0.1, 0.8)
  x <- rlinearpool(myfit, n = N, w = weights)
  se <- sqrt(var(x) / N)
  expect_equal(mean(x), 
               sum(weights * c(m1, m2, m3)),
               tolerance = 4 * se)
})

test_that("feedback for multiple experts works",{
  skip_on_cran()
  p1 <- c(0.33, 0.5, 0.66, 0.75)
  a <- 5; b <- 5
  v1 <- qgamma(p1, a, b)
  mu <- 10 ; sigma <- 2
  v2 <- qnorm(p1, mu, sigma)
  v3 <- qlnorm(p1, log(mu), sigma)
  V <- matrix(c(v1, v2, v3), 4, 3)
  myfit <- fitdist(vals = V, probs = p1, lower = 0)
  
  fb <- feedback(myfit, quantiles = c(0.1, 0.9),
                 values = c(1, 2), sf = 6)
  
  expect_equal(fb$fitted.quantiles[, 1], qgamma(c(0.1, 0.9), a, b),
               tolerance = 1e-3)
  expect_equal(fb$fitted.quantiles[, 2], qnorm(c(0.1, 0.9), mu, sigma),
               tolerance = 1e-3)
  expect_equal(fb$fitted.quantiles[, 3], qlnorm(c(0.1, 0.9), log(mu), sigma),
               tolerance = 1e-3)
  
  expect_equal(fb$fitted.probabilities[, 1], pgamma(1:2, a, b),
               tolerance = 1e-4)
  expect_equal(fb$fitted.probabilities[, 2], pnorm(1:2, mu, sigma),
               tolerance = 1e-4)
  expect_equal(fb$fitted.probabilities[, 3], plnorm(1:2, log(mu), sigma),
               tolerance = 1e-4)
})


test_that("feedback for multiple experts works",{
  skip_on_cran()
  p1 <- c(0.33, 0.5, 0.66, 0.75)
  a <- 5; b <- 5
  v1 <- qgamma(p1, a, b)
  mu <- 10 ; sigma <- 2
  v2 <- qnorm(p1, mu, sigma)
  v3 <- qlnorm(p1, log(mu), sigma)
  V <- matrix(c(v1, v2, v3), 4, 3)
  myfit <- fitdist(vals = V, probs = p1, lower = 0)
  
  fb <- feedback(myfit, quantiles = c(0.1, 0.9),
                 values = c(1, 2), sf = 6)
  
  expect_equal(fb$fitted.quantiles[, 1], qgamma(c(0.1, 0.9), a, b),
               tolerance = 1e-3)
  expect_equal(fb$fitted.quantiles[, 2], qnorm(c(0.1, 0.9), mu, sigma),
               tolerance = 1e-3)
  expect_equal(fb$fitted.quantiles[, 3], qlnorm(c(0.1, 0.9), log(mu), sigma),
               tolerance = 1e-3)
  
  expect_equal(fb$fitted.probabilities[, 1], pgamma(1:2, a, b),
               tolerance = 1e-4)
  expect_equal(fb$fitted.probabilities[, 2], pnorm(1:2, mu, sigma),
               tolerance = 1e-4)
  expect_equal(fb$fitted.probabilities[, 3], plnorm(1:2, log(mu), sigma),
               tolerance = 1e-4)
})


test_that("linear pool density works",{
  skip_on_cran()
  p1 <- c(0.1, 0.5, 0.66, 0.9)
  a <- 20; b <- 2
  v1 <- qgamma(p1, a, b)
  mu <- 9 ; sigma <- 3
  v2 <- qnorm(p1, mu, sigma)
  V <- matrix(c(v1, v2), 4, 2)
  myfit <- fitdist(vals = V, probs = p1, lower = 0)
  fx <- linearPoolDensity(myfit, xl = 0, xu = 20, lpw = c(2, 1))
  x <- seq(from = 0, to = 20, length = 200)
  expect_equal(fx$f, 1/3*dnorm(x, mu, sigma) + 2/3 * dgamma(x, a, b), tolerance = 1e-6)
})
