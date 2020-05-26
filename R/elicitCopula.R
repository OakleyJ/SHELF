#' Generate correlated samples from elicited marginal distributions using a multivariate normal copula
#'
#' Takes elicited marginal distributions and elicited concordance probabilities: pairwise
#' probabilities of two uncertain quantities being greater than their medians, and generates
#' a correlated sample, assuming the elicited marginal distributions and a multivariate
#' normal copula
#'
#'
#' @param ... A list of objects of class \code{elicitation}.
#' command, one per marginal distribution, separated by commas.
#' @param cp A matrix of pairwise concordance probabilities, with element i,j the elicited probability
#' P(X_i > m_i, X_j > m_j or X_i < m_i, X_j < m_j), where m_i and m_j are the elicited medians of the uncertain quantities X_i and X_j.
#' Only the upper triangular elements in the matrix need to be specified; the remaining elements can be set at 0.
#' @param n The sample size to be generated
#' @param d A vector of distributions to be used for each elicited quantity: a string with elements chosen from
#' \code{"normal", "t", "gamma", "lognormal", "logt", "beta"}. The default is to use
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
#' copulaSample(myfit1, myfit2, myfit3, cp=quad.probs, n=100, d=NULL)
#' }

#' @importFrom MASS mvrnorm
#' @export

copulaSample <- function(..., cp, n, d = NULL) {
  elicitation.fits <- list(...)
  n.vars <- length(elicitation.fits)
  
  if (is.null(d)) {
    d <- sapply(elicitation.fits, function(x) {
      unlist(x$best.fitting)
    })
  }
  
  r <- sin(2 * pi * cp / 2 - pi / 2)
  diag(r) <- 1
  r[lower.tri(r)] <- r[upper.tri(r)]
  
  r.check <- try(chol(r), silent = TRUE)
  if (inherits(r.check, "try-error")) {
    cat("Elicited correlation matrix is not positive definite.")
    if (nrow(r) == 3) {
      cat(
        "\nConsider adjusting one of the concordance probabilities\nto be within the following limits.\n\n"
      )
      limits <- sapply(3:1, getConcordanceLimits, cor.mat = r)
      rownames(limits) <- c("lower", "upper")
      colnames(limits) <- c("  p_{1,2}", "  p_{1,3}", "  p_{2,3}")
      print(limits)
      return(NULL)
    }
  } else{
    z <- MASS::mvrnorm(n, mu = rep(0, n.vars), r)
    p <- pnorm(z)
    
    theta <- matrix(0, n, n.vars)
    for (i in 1:n.vars) {
   
        if(d[i] ==  "best"){
        d[i] <- as.character(elicitation.fits[[i]]$best.fitting[1, 1])
    
      }
    
      
      theta[, i] <- feedback(elicitation.fits[[i]],
                             quantiles = p[, i],
                             sf = 8)$fitted.quantiles[d[i]][, 1]
    }
    return(theta)
  }
}



getConcordanceLimits <- function(i, cor.mat) {
  index <- c(i, (1:3)[-i])
  r <- cor.mat[index, index]
  m <- solve(r[1:2, 1:2])
  a <- m[2, 2]
  b <- 2 * r[1, 3] * m[1, 2]
  k <- r[1, 3] ^ 2 * m[1, 1] - 1
  
  l1 <- (-b - sqrt(b ^ 2 - 4 * a * k)) / (2 * a)
  l2 <- (-b + sqrt(b ^ 2 - 4 * a * k)) / (2 * a)
  2 * (asin(c(l1, l2)) + pi / 2) / (2 * pi)
}
