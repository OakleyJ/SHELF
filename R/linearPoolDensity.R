#' Obtain points on the density function of a linear pool 
#' 
#' Takes an object of class \code{elicitation}, evaluates a (weighted) linear pool, 
#' and returns points on the density function at a sequence of values of the elicited 
#' parameter  
#'  
#' 
#' 
#' @param fit An object of class \code{elicitation}.
#' @param d The distribution fitted to each expert's probabilities. Options are
#' \code{"normal"}, \code{"t"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, \code{"hist"} (for a histogram fit), and
#' \code{"best"} (for best fitting)
#' @param xl The lower limit in the sequence of parameter values. The default is the 0.001 quantile
#' of the fitted distribution (or the 0.001 quantile of a fitted normal
#' distribution, if a histogram fit is chosen).
#' @param xu The upper limit in the sequence of parameter values. The default is the 0.999 quantile
#' of the fitted distribution (or the 0.999 quantile of a fitted normal
#' distribution, if a histogram fit is chosen).
#' @param lpw A vector of weights to be used in linear pool, if unequal
#' weighting is desired.
#' @param nx The number of points in the sequence from \code{xl} to \code{xu}.
#' @return A list, with elements
#' \item{x}{a sequence of values for the uncertain parameter}
#' \item{fx}{the density function of the linear pool, evaluated at each element in \code{x}.}
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' # Two experts
#' # Expert 1 states P(X<30)=0.25, P(X<40)=0.5, P(X<50)=0.75
#' # Expert 2 states P(X<20)=0.25, P(X<25)=0.5, P(X<35)=0.75
#' # Both experts state 0<X<100. 
#' 
#' v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
#' p <- c(0.25, 0.5, 0.75)
#' myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
#' linearPoolDensity(myfit)
#' }

#' @export
linearPoolDensity <- function(fit, xl = -Inf, xu = Inf, d = "best", lpw = 1, nx = 200){
  # get axes limits
  if(xl == -Inf & min(fit$limits[,1]) > -Inf){xl <- min(fit$limits[,1]) }
  if(xl == -Inf & min(fit$limits[,1]) == -Inf){
    f1 <- feedback(fit, quantiles=0.01, dist=d)
    xl <- min(f1$expert.quantiles)
  }
  
  if(xu == Inf & max(fit$limits[,2]) < Inf){xu <- max(fit$limits[,2]) }
  
  if(xu == Inf & max(fit$limits[,2]) == Inf){
    f2 <- feedback(fit, quantiles=0.99, dist=d)
    xu <- max(f2$expert.quantiles)
  }
  # end get axes limits
  
  n.experts <- nrow(fit$vals)
  x <- matrix(0, nx, n.experts)
  fx <- x
  if(min(lpw)<0 | max(lpw)<=0){stop("expert weights must be non-negative, and at least one weight must be greater than 0.")}
  
  if(length(lpw)==1){
    lpw <- rep(lpw, n.experts)
  }
  
  weight <- matrix(lpw/sum(lpw), nx, n.experts, byrow = T)
  
  for(i in 1:n.experts){
    densitydata <- expertdensity(fit, d, ex = i, xl, xu, nx)
    x[, i] <- densitydata$x
    fx[, i] <-densitydata$fx 
  }
  
  fx.lp <- apply(fx * weight, 1, sum)
  list(x = x[, 1], f = fx.lp)
}
