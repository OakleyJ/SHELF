test_that("precision fitting uses the requested log-t degrees of freedom", {
  propvals <- c(0.2, 0.4)
  probs <- c(0.05, 0.95)
  fit <- fitprecision(c(0, 1), propvals, tdf = 9, pplot = FALSE)
  parameters <- unlist(fit$Log.Student.t, use.names = FALSE)
  expect_equal(parameters[3], 9)
  # For this interval, precision = qnorm(proportion + 0.5)^2.
  logprecision <- log(qnorm(propvals + 0.5)^2)
  expect_equal(pt((logprecision - parameters[1]) / parameters[2], df = 9),
               probs, tolerance = 1e-5)
  default <- fitprecision(c(0, 1), propvals, pplot = FALSE)
  expect_equal(default$Log.Student.t$df.log.X, 3)
})

test_that("precision best-fit label names the smallest error column", {
  fit <- fitprecision(c(0, 1), c(0.2, 0.4), pplot = FALSE)
  selected <- names(fit$ssq)[which.min(fit$ssq)]
  expect_equal(fit$best.fitting$best.fit, selected)
})

test_that("precision best-fit selection works in linear pool feedback", {
  fit <- fitprecision(c(0, 1), c(0.2, 0.4), pplot = FALSE)
  selected <- names(fit$ssq)[which.min(fit$ssq)]
  x <- c(0.1, 0.5, 1, 2)
  expect_equal(plinearpool(fit, x, d = "best"),
               plinearpool(fit, x, d = selected))
})

test_that("tail precision fits preserve asymmetric proportion judgements", {
  cases <- list(
    list(interval = c(-Inf, 0), med = 1, trans = "identity"),
    list(interval = c(2, Inf), med = 1, trans = "identity"),
    list(interval = c(-Inf, exp(-1)), med = 1, trans = "log"),
    list(interval = c(exp(1), Inf), med = 1, trans = "log"),
    list(interval = c(-Inf, plogis(-1)), med = 0.5, trans = "logit"),
    list(interval = c(plogis(1), Inf), med = 0.5, trans = "logit")
  )
  # Each threshold is one unit from the median on the population link scale.
  # The tail proportion is Phi(-sqrt(precision)), a decreasing transformation.
  proportions <- c(0.1, 0.3)
  for (case in cases) {
    fit <- do.call(fitprecision, c(case, list(
      propvals = proportions, propprobs = c(0.1, 0.8), pplot = FALSE)))
    implied <- pgamma(qnorm(proportions)^2, fit$Gamma$shape,
                      fit$Gamma$rate, lower.tail = FALSE)
    expect_equal(implied, c(0.1, 0.8), tolerance = 1e-4)
    expect_equal(as.numeric(fit$probs), c(0.2, 0.9))
  }
})

test_that("default symmetric tail judgements are unchanged", {
  for (interval in list(c(-Inf, 0), c(2, Inf))) {
    fit <- fitprecision(interval, c(0.1, 0.3), med = 1, pplot = FALSE)
    implied <- pgamma(qnorm(c(0.1, 0.3))^2, fit$Gamma$shape,
                      fit$Gamma$rate, lower.tail = FALSE)
    expect_equal(implied, c(0.05, 0.95), tolerance = 1e-4)
    expect_equal(as.numeric(fit$probs), c(0.05, 0.95))
  }
})

test_that("finite-interval judgements keep their increasing transformation", {
  fit <- fitprecision(c(0, 1), c(0.1, 0.3),
                      propprobs = c(0.1, 0.8), pplot = FALSE)
  implied <- pgamma(qnorm(c(0.6, 0.8))^2, fit$Gamma$shape, fit$Gamma$rate)
  expect_equal(implied, c(0.1, 0.8), tolerance = 1e-4)
  expect_equal(as.numeric(fit$probs), c(0.1, 0.8))
})

test_that("tail plots label the original proportion judgements", {
  plots <- NULL
  local_mocked_bindings(multiplot = function(...) plots <<- list(...),
                        .package = "SHELF")
  for (interval in list(c(-Inf, 0), c(2, Inf))) {
    fitprecision(interval, c(0.1, 0.3), propprobs = c(0.1, 0.8), med = 1)
    expect_equal(plots[[1]]$labels$title,
                 "lower (0.1 quantile) proportion = 0.1")
    expect_equal(plots[[2]]$labels$title,
                 "upper (0.8 quantile) proportion = 0.3")
  }
})


test_that("tail fits retain support for reversed proportion values", {
  for (interval in list(c(-Inf, 0), c(2, Inf))) {
    forward <- fitprecision(interval, c(0.1, 0.3),
                            propprobs = c(0.1, 0.8), med = 1, pplot = FALSE)
    reversed <- fitprecision(interval, c(0.3, 0.1),
                             propprobs = c(0.1, 0.8), med = 1, pplot = FALSE)
    expect_equal(reversed$Gamma, forward$Gamma)
    expect_equal(as.numeric(reversed$probs), c(0.2, 0.9))
  }
})

test_that("normal and lognormal tail judgements recover a known precision law", {
  sd <- 1 / sqrt(qgamma(c(0.05, 0.95), shape = 3, rate = 4))
  for (link in c("identity", "log")) {
    population_cdf <- if (link == "identity") {
      function(x, lower) pnorm(x, mean = 10, sd = sd, lower.tail = lower)
    } else {
      function(x, lower) plnorm(x, meanlog = log(10), sdlog = sd,
                               lower.tail = lower)
    }
    endpoints <- if (link == "identity") c(9, 11) else c(5, 15)
    for (lower_tail in c(TRUE, FALSE)) {
      endpoint <- endpoints[if (lower_tail) 1 else 2]
      interval <- if (lower_tail) c(-Inf, endpoint) else c(endpoint, Inf)
      # As in the original package tests, these proportion values descend.
      proportions <- population_cdf(endpoint, lower_tail)
      fit <- fitprecision(interval, proportions, med = 10,
                          trans = link, pplot = FALSE)
      expect_equal(unlist(fit$Gamma, use.names = FALSE), c(3, 4),
                   tolerance = 1e-4)
    }
  }
})

test_that("tail fitting still rejects descending probability levels", {
  for (interval in list(c(-Inf, 0), c(2, Inf))) {
    expect_error(fitprecision(interval, c(0.1, 0.3),
                              propprobs = c(0.8, 0.1), med = 1, pplot = FALSE),
                 "probabilities must be specified in ascending order")
  }
})
