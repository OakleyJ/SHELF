#' Generate correlated samples from elicited marginal distributions using a multivariate normal copula  
#' 
#' Takes elicited marginal distributions and elicited concordance probabilities: pairwise 
#' probabilities of two uncertain quantities being greater than their medians, and generates
#' a correlated sample, assuming the elicited marginal distributions and a multivariate 
#' normal copula
#' 
#' 
#' @param ... A list of elicitation fits produced from the \code{fitdist}
#' command, one per marginal distribution, separated by commas.
#' @param qp A matrix of pairwise quadrant probabilities, with element i,j the elicited probability
#' P(X_i > m_i, X_j > m_j), where m_i and m_j are the elicited medians of the uncertain quanties X_i and X_j.
#' Only the upper triangular elements in the matrix need to be specified; the remaining elements can be set at 0.
#' @param n The sample size to be generated
#' @param d A vector of distributions to be used for each elicited quantity: a string with elements chosen from
#' \code{"Normal", "Student-t", "Gamma", "Log normal" "Log Student-t", "Beta"}. The default is to use 
#' the best fitting distribution in each case.
#' 
#

#' @return A matrix of sampled values, one row per sample.
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' p1 <- c(0.25, 0.5, 0.75)
#' v1 <- c(0.5, 0.55, 0.6)
#' v2 <- c(0.22, 0.3, 0.35)
#' v3 <- c(0.11, 0.15, 0.2)
#' myfit1 <- fitdist(v1, p1, 0, 1)
#' myfit2 <- fitdist(v2, p1, 0, 1)
#' myfit3 <- fitdist(v3, p1, 0, 1)
#' quad.probs <- matrix(0, 3, 3)
#' quad.probs[1, 2] <- 0.4
#' quad.probs[1, 3] <- 0.4
#' quad.probs[2, 3] <- 0.3
#' copulaSample(myfit1, myfit2, myfit3, qp=quad.probs, n=100, d=NULL)
#' }

#' @importFrom MASS mvrnorm
#' @export

copulaSample <- function(..., qp, n, d = NULL){
  elicitation.fits <- list(...)
  n.vars <- length(elicitation.fits)
  
  if (is.null(d)) {
    d<-sapply(elicitation.fits, function(x){unlist(x$best.fitting)})
  }
  
  r <- sin(2 * pi * qp - pi/2)
  diag(r) <- 1
  r[lower.tri(r)] <- r[upper.tri(r)]
  
  r.check <- try(chol(r), silent = TRUE)
  if(class(r.check) == "try-error") stop("Elicited correlation matrix is not positive definite.")
  
  z <- MASS::mvrnorm(n, mu = rep(0, n.vars), r)
  p <- pnorm(z)
  
  theta <- matrix(0, n, n.vars)
  for(i in 1:n.vars){
    theta[ , i] <- feedback(elicitation.fits[[i]], 
                            quantiles=p[, i])$fitted.quantiles[d[i]][, 1]
  }
  theta
}


