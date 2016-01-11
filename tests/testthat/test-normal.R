
test_that("distribution fitting works",{
  m <- 10
  s <- 20
  vals <- c(m - s, m , m + 2 * s)
  myfit <- fitdist(vals, pnorm(vals, m, s ))
  norm.parameters <- unlist(myfit$Normal)
  best.name <- unlist(myfit$best.fitting)
  attributes(best.name) <- attributes(norm.parameters) <- NULL
  expect_equal(norm.parameters, c(m, s))
  expect_equal(best.name, 1)
})