## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
p1 <- c(0.25, 0.5, 0.75)

## ------------------------------------------------------------------------
v.good <- c(0.5, 0.55, 0.6)
v.seql <- c(0.22, 0.3, 0.35)
v.dead <- c(0.11, 0.15, 0.2)

## ------------------------------------------------------------------------
library(SHELF)
fit.good <- fitdist(vals = v.good, probs = p1, lower = 0, upper = 1)
fit.seql <- fitdist(vals = v.seql, probs = p1, lower = 0, upper = 1)
fit.dead <- fitdist(vals = v.dead, probs = p1, lower = 0, upper = 1)

## ------------------------------------------------------------------------
fit.good$Beta

## ---- , fig.height = 4, fig.width = 5, fig.pos="h", fig.align="center"----
d.fit <- fitDirichlet(fit.good, fit.seql, fit.dead,
                  categories = c("Good outcome","Sequel","Dead"),
                  n.fitted = "opt")

## ------------------------------------------------------------------------
feedbackDirichlet(d.fit, quantiles = c(0.1, 0.5, 0.9))

## ---- fig.height = 4, fig.width = 5, fig.pos="h", fig.align="center"-----
d.fit <- fitDirichlet(fit.good, fit.seql, fit.dead,
                  categories = c("Good outcome","Sequel","Dead"),
                  n.fitted = "min")

