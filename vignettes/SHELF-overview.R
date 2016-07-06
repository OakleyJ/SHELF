## ------------------------------------------------------------------------
v <- matrix(c(25, 30, 35, 30, 35, 50), nrow = 3, ncol = 2)

## ---- echo = F-----------------------------------------------------------
v

## ------------------------------------------------------------------------
p <- c(0.25, 0.5, 0.75)

## ------------------------------------------------------------------------
library(SHELF)
myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)

## ------------------------------------------------------------------------
names(myfit)

## ------------------------------------------------------------------------
myfit$Beta

## ------------------------------------------------------------------------
myfit$ssq

## ---- fig.height = 4, fig.width = 5, fig.pos="h", fig.align="center", fig.cap = "The two fitted distributions (normal for Expert 1 and log Student-$t_3$ for Expert 2) and an equal-weighted linear pool."----
plotfit(myfit, lp = TRUE)

## ------------------------------------------------------------------------
v <-c(25, 30, 40)
p <-c(0.25, 0.5, 0.75)
consensus <- fitdist(vals = v, probs = p, lower = 0, upper = 100)

## ---- fig.height = 4, fig.width = 5, fig.pos="h", fig.align="center", fig.cap = "The fitted consensus distribution, with the lower and upper 5\\% tail areas shown as feedback."----
plotfit(consensus, ql = 0.05, qu = 0.95, d = "beta")

## ------------------------------------------------------------------------
feedback(consensus, quantiles = c(0.05, 0.95))

