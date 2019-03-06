#' Fit a distribution to judgements about a population precision
#' 
#' Takes elicited probabilities about proportion of a population
#' lying in a specfied interval as inputs, converts the judgements into probability
#' judgements about the population precision, and fits gamma and lognormal distributions
#' to these judgements using the \link{fitdist} function. 
#' 
#' The expert provides a pair of probability judgements
#'  \deqn{P(\theta < \theta_1 ) = p_1,} and \deqn{P(\theta < \theta_2) = p_2,}
#'  where \eqn{\theta} is the proportion of the population that lies in the interval
#'  \eqn{[k_1, k_2]}, conditional on the population median taking some hypothetical value (\eqn{k_1}
#'  by default). \eqn{k_1} can be set to \code{-Inf}, or \eqn{k_2} can be set to \code{Inf};
#'  in either case, the hypothetical median value must be specified. If both \eqn{k_1}
#'  and \eqn{k_2} are finite, the hypothetical median must be one of the interval endpoints.
#'  Note that, unlike the \link{fitdist} command, a 'best fitting'
#'  distribution is not reported, as the distributions are fitted to two elicited
#'  probabilities only.    
#' 
#' @param interval A vector specifying the endpoints of an interval \eqn{[k_1, k_2]}.  
#' @param propvals A vector specifying two values \eqn{\theta_1, \theta_2} for the proportion.
#' @param propprobs A vector specifying two probabilities \eqn{p_1, p_2}.
#' @param med The hypothetical value of the population median.
#' @param trans A string variable taking the value \code{"identity"}, \code{"log"} or
#' \code{"logit"} corresponding to whether the population distribution is normal, lognormal
#' or logit-normal respectively.
#' @param pplot Plot the population distributions with median set at \eqn{k_1}
#' and precision fixed at the two elicited quantiles implied by \code{propvals} 
#' and \code{propprobs}.
#' @param tdf Degrees of freedom in the fitted log Student-t distribution.
#' @param fontsize Font size used in the plots.
#' 
#' @return 
#' \item{Gamma}{Parameters of the fitted gamma distribution. Note that E(precision) =
#' shape / rate.} 
#' \item{Log.normal}{Parameters of the fitted log normal
#' distribution: the mean and standard deviation of log precision.}
#' \item{Log.Student.t}{Parameters of the fitted log student t distributions.
#' Note that (log(X- \code{lower}) - location) / scale has a standard t distribution. The
#' degrees of freedom is not fitted: it is specified as an input argument.} 
#' \item{vals}{The elicited values \eqn{\theta_1, \theta_2}}
#' \item{probs}{The elicited probabilities \eqn{p_1, p_2}}
#' \item{limits}{The lower and upper limits specified by each expert (+/- Inf
#' if not specified).}
#' \item{transform}{Transformation used for a normal population distribution.}

#' 
#'    
#' @examples 
#' \dontrun{
#' fitprecision(interval=c(60, 70), propvals=c(0.2, 0.4), trans = "log")
#'   }
#' @export

fitprecision <- function(interval, propvals, 
                         propprobs = c(0.05, 0.95),
                         med = interval[1],
                         trans = "identity", pplot = TRUE,
                         tdf = 3,
                         fontsize = 12){
  
  if (!all(is.finite(interval)) & med == interval[1] ){
    stop('argument med must be specified if using tail proportions.')
  }
  
  if (all(is.finite(interval)) & !is.element(med, interval)){
    stop('for finite interval, med must be one of the interval endpoints.')
  }
  
  if (max(propvals >=0.5) | min(propvals <=0)){
    stop('propvals must be between 0 and 0.5')
  }
  
  if (trans != "identity" & trans != "log" & trans != "logit"){
    stop('argument trans must be one of "identity", "log" or "logit"')}
  
  # Make copies for use in the density plots
  intervalPlot <- interval
  propvalsPlot <- propvals
  
  
  # Convert interval and proportions to the case
  # P(X in [k1, k2] | mu = k1), if necessary
  if (interval[1] == -Inf){
    interval <- c(interval[2], med)
    propvals <- sort(0.5 - propvals)
  }
  
  if (interval[2] == Inf){
    interval <- c(med, interval[1])
    propvals <- sort(0.5 - propvals)
  }
  
  if (trans == "identity"){
    precisionvalues <- (qnorm(propvals + 0.5) /
                          (interval[2] - interval[1]))^2
    dens <- dnorm
    quan <- qnorm
    m <- med
  }
  
  if (trans == "log"){
    precisionvalues <- (qnorm(propvals + 0.5) / (log(interval[2]) - log(interval[1])))^2
    dens <- dlnorm
    quan <- qlnorm
    m <- log(med)
  }
  
  if (trans == "logit"){
    precisionvalues <- (qnorm(propvals + 0.5) / (logit(interval[2]) - logit(interval[1])))^2
    dens <- dlogit
    quan <- qlogit
    m <- logit(med)
  }
  
  precisionfit <- fitdist(vals = precisionvalues, probs = propprobs,
                          lower = 0)
  
  precisionfit$interval <- intervalPlot 
  precisionfit$transform <- trans
  
  if(pplot == TRUE){
    
    s <- sort(1 / sqrt(precisionvalues))
    xl<-quan(0.001, m, s[2])
    xu<-quan(0.999, m, s[2])
    x <- seq(from = xl, to  = xu, length = 200)
    d1 <- dens(x, m, s[2])
    
    tailInterval <- FALSE
    
    if(intervalPlot[1]== -Inf){
      intervalPlot[1] <- xl
      tailInterval <- TRUE
    }
    if(intervalPlot[2]== Inf){
      intervalPlot[2] <- xu
      tailInterval <- TRUE
    }
    
    
    xint <- seq(from = intervalPlot[1], to = intervalPlot[2], length = 200)
    dint1 <- dens(xint, m, s[2])
    d2 <- dens(x, m, s[1])
    dint2 <- dens(xint, m, s[1])
    
    # lower proportion will give larger variance for a finite interval
    # but smaller variance if tail interval is used
    if(tailInterval){
      df<-data.frame(x=x, d1=d2, d2=d1, xint=xint, dint1=dint2, dint2=dint1)
    }else{
      df<-data.frame(x=x, d1=d1, d2=d2, xint=xint, dint1=dint1, dint2=dint2)
    }
   
    theme_set(theme_grey(base_size = fontsize))
    
    pcore <- ggplot(df, aes(x=x, y=d1)) + expand_limits(y = c(0, max(d2))) +
      labs(y = "")
    p1 <- pcore + geom_line() + 
      geom_area(data = df, aes(x=xint, y = dint1), fill="red", alpha=0.5) +
      labs(title = paste("lower (",propprobs[1],
                         " quantile) proportion = ", propvalsPlot[1], sep=""))
    p2 <- pcore + geom_line(aes(x=x, y=d2)) + 
      geom_area(data = df, aes(x=xint, y = dint2), fill="red", alpha=0.5) +
      labs(title = paste("upper (",propprobs[2],
                         " quantile) proportion = ", propvalsPlot[2], sep =""))
    multiplot(p1, p2)
  }
  
  
  precisionfit$Normal[1, ] <- precisionfit$Student.t[1, ] <- NA
  precisionfit$ssq[1:2] <- NA
  
  distnames <- c("Normal", "Student-t", "Gamma", "Log normal", "Log Student-t", "Beta")
   
  index <- which.min(precisionfit$ssq)
  precisionfit$best.fitting <- data.frame(best.fit=distnames[index])
  
  class(precisionfit) <- "elicitation"
  precisionfit
}