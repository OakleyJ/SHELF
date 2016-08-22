## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(SHELF)
p <- c(0.25, 0.5, 0.75)
v1 <- c(0.12, 0.15, 0.2)
v2 <- c(0.15, 0.2, 0.25)
myfit1 <- fitdist(vals = v1, probs = p, lower = 0, upper = 1)
myfit2 <- fitdist(vals = v2, probs = p, lower = 0, upper = 1)

## ---- fig.height = 3, fig.width = 3, fig.align="center", fig.cap = c("The fitted marginal distribution for $X_1$.", "The fitted marginal distribution for $X_2$." ), fig.keep = "all", fig.pos="h"----
plotfit(myfit1, d = "beta", xl = 0, xu = 0.5)
plotfit(myfit2, d = "beta", xl = 0, xu = 0.5)


## ------------------------------------------------------------------------
conc.prob <- matrix(0, 2, 2)
conc.prob[1, 2] <- 0.8
X <- copulaSample(myfit1, myfit2, cp = conc.prob, 
                  n = 1000, d = c("Beta", "Beta"))

## ------------------------------------------------------------------------
quantile(X[, 1], probs = c(0.25, 0.5, 0.75))
quantile(X[, 2], probs = c(0.25, 0.5, 0.75))
mean((X[, 1] > 0.15 & X[, 2] > 0.2) | (X[, 1] < 0.15 & X[, 2] < 0.2))

## ---- fig.height = 2.5, fig.width = 2.5, fig.align="center", fig.cap="A sample from the joint distribution of $X_1, X_2$. The horizontal and vertical lines indicated the elicited medians. Note that expert has judged a probability of 0.8 of $X_1$ and $X_2$ being either both above or both below their median values, and so approximately $80 \\% $ of the points are in the top right  and bototm left quadrants.", fig.keep = "all", fig.pos="h"----
library(ggplot2)
ggplot(data.frame(X), aes(x = X1, y = X2)) +
  geom_point(alpha = 0.1, colour = "red") +
  geom_hline(yintercept = 0.2) + 
  geom_vline(xintercept = 0.15) +
  labs(x=expression(X[1]), y = expression(X[2]))

## ---- eval = FALSE-------------------------------------------------------
#  elicitConcProb(myfit1, myfit2, m1 = 0.15, m2 = 0.2, d = c("Beta", "Beta"))

## ------------------------------------------------------------------------
v3 <- c(0.2, 0.25, 0.35)
myfit3 <- fitdist(vals = v3, probs = p, lower = 0, upper = 1)

## ------------------------------------------------------------------------
conc.prob <- matrix(0, 3, 3)
conc.prob[1, 2] <- 0.8
conc.prob[1, 3] <- 0.7
conc.prob[2, 3] <- 0.1

## ------------------------------------------------------------------------
conc.prob

## ------------------------------------------------------------------------
theta <- copulaSample(myfit1, myfit2, myfit3, 
                      cp = conc.prob, n = 1000, 
                      d = rep("Beta", 3))

## ------------------------------------------------------------------------
conc.prob[2, 3] <- 0.9
theta <- copulaSample(myfit1, myfit2, myfit3, 
                      cp = conc.prob, n = 1000, 
                      d = rep("Beta", 3))

## ------------------------------------------------------------------------
GGally::ggpairs(data.frame(theta),
        lower = list(continuous = GGally::wrap(GGally::ggally_points, 
                                       color = "red",
                                       alpha = 0.1)),
        columnLabels = c(expression(X[1]),
                                    expression(X[2]),
                                               expression(X[3])))
                                 

