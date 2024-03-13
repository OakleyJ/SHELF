test_that("Dirichlet distribution fitting and feedback works",{
  skip_on_cran()
  a <- c(20, 10, 5)
  p1 <- c(0.25, 0.5, 0.75)
  v1 <- qbeta(p1, a[1], sum(a[2:3]))
  v2 <- qbeta(p1, a[2], sum(a[c(1, 3)]))
  v3 <- qbeta(p1, a[3], sum(a[1:2]))
  myfit1 <- fitdist(v1, p1, 0, 1)
  myfit2 <- fitdist(v2, p1, 0, 1)
  myfit3 <- fitdist(v3, p1, 0, 1)
  d <- fitDirichlet(myfit1, myfit2, myfit3,
                    categories = c("A","B","C"),
                    n.fitted = "opt", silent = TRUE, plotBeta = FALSE)
  expect_equal(a, as.numeric(round(d, 3)))
  fb <- feedbackDirichlet(d, 0.1, sf = 5)
  expect_equal(as.numeric(fb[, 2]), signif(qbeta(0.1, a, c(sum(a[2:3]),
                                                    sum(a[c(1, 3)]),
                                                    sum(a[1:2]))), 5))
})