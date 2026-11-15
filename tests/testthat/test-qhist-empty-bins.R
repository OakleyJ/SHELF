test_that("histogram quantiles skip empty interior bins", {
  z <- c(0, 1, 2, 3)
  pz <- c(0, 0.5, 0.5, 1)
  expect_warning(x <- qhist(c(0.25, 0.5, 0.75), z, pz), NA)
  expect_equal(x, c(0.5, 1, 2.5))
  expect_equal(phist(x, z, pz), c(0.25, 0.5, 0.75))
})

test_that("histogram quantiles handle repeated endpoint probabilities", {
  z <- 0:6
  pz <- c(0, 0, 0, 0.5, 0.5, 1, 1)
  expect_equal(qhist(c(0, 0.25, 0.5, 0.75, 1), z, pz),
               c(0, 2.5, 3, 4.5, 5))
})

test_that("histogram quantiles retain ordinary interpolation and missing results", {
  z <- c(-2, 0, 3, 7)
  pz <- c(0, 0.2, 0.7, 1)
  q <- c(0, 0.1, 0.2, 0.5, 0.7, 0.9, 1)
  expect_equal(qhist(q, z, pz), approx(pz, z, q)$y)
  expect_equal(qhist(c(-0.1, 1.1, NA_real_, NaN, -Inf, Inf), z, pz),
               rep(NA_real_, 6))
  expect_identical(qhist(numeric(0), z, pz), numeric(0))
})

test_that("histogram inverse samples preserve the specified CDF", {
  z <- c(-3, -2, 0, 1, 4, 6)
  masses <- c(0.2, 0, 0, 0.5, 0.3)
  q <- seq(0.001, 0.999, length.out = 999)
  x <- qhist(q, z, c(0, cumsum(masses)))
  # Independently sum each uniform bin's CDF contribution.
  cdf <- vapply(x, function(value) {
    sum(masses * pmin(1, pmax(0, (value - head(z, -1)) / diff(z))))
  }, numeric(1))
  expect_equal(cdf, q, tolerance = 1e-12)
  expect_false(any(x > -2 & x < 1))
})


test_that("public histogram feedback and quantiles skip empty bins", {
  # The fit initializer also uses approx; its tie warning is unrelated here.
  fit <- suppressWarnings(fitdist(c(.5, 1, 2, 2.5), c(.25, .5, .5, .75),
                                 lower = 0, upper = 3))
  probabilities <- c(.45, .55)
  feedback <- suppressWarnings(feedback(fit, quantiles = probabilities, sf = 12))
  expect_equal(feedback$fitted.quantiles$hist, c(.9, 2.1))
  expect_equal(suppressWarnings(expertquantiles(fit, probabilities, d = "hist")),
               c(.9, 2.1))
})

test_that("public histogram sampling does not fill empty bins", {
  fit <- suppressWarnings(fitdist(c(.5, 1, 2, 2.5), c(.25, .5, .5, .75),
                                 lower = 0, upper = 3))
  set.seed(20261114)
  draws <- suppressWarnings(sampleFit(fit, 2000)[, "hist"])
  expect_false(any(draws > 1 & draws < 2))
})

test_that("public histogram quantiles preserve ordinary interpolation", {
  fit <- fitdist(c(.5, 1.5, 2.5), c(.25, .5, .75), lower = 0, upper = 3)
  probabilities <- seq(0, 1, length.out = 11)
  expected <- approx(c(0, .25, .5, .75, 1), c(0, .5, 1.5, 2.5, 3), probabilities)$y
  expect_equal(expertquantiles(fit, probabilities, d = "hist"), expected)
  expect_equal(feedback(fit, quantiles = probabilities, sf = 12)$fitted.quantiles$hist,
               expected, tolerance = 1e-10)
})
