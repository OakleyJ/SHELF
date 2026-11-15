test_that("reflected log-t feedback reaches one at its upper support", {
  fit <- fitdist(c(1, 2, 3), c(0.25, 0.5, 0.75), lower = 0, upper = 4)
  probabilities <- feedback(fit, values = c(3.9, 4, 5, Inf),
                            sf = 12)$fitted.probabilities$mirrorlogt
  expect_equal(probabilities[-1], c(1, 1, 1))
  expect_true(all(diff(probabilities) >= 0))
})

test_that("reflected log-t feedback agrees with the defining distribution", {
  for (upper in c(0, 4)) {
    fit <- fitdist(upper - c(3, 2, 1), c(0.25, 0.5, 0.75),
                   upper = upper, tdf = 9)
    values <- c(-Inf, upper - c(3, 1, 0.1), upper, upper + 1, Inf)
    parameters <- unlist(fit$mirrorlogt, use.names = FALSE)
    expected <- rep(1, length(values))
    below <- values < upper
    score <- (log(upper - values[below]) - parameters[1]) / parameters[2]
    expected[below] <- pt(score, parameters[3], lower.tail = FALSE)
    actual <- feedback(fit, values = values, sf = 12)$fitted.probabilities$mirrorlogt
    expect_equal(actual, expected, tolerance = 1e-10)
    expect_true(all(diff(actual) >= 0))
    expect_true(all(actual >= 0 & actual <= 1))
  }
})
