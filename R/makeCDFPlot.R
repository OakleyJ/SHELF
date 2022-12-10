#' Plot the elicited cumulative probabilities 
#' 
#' Plots the elicited cumulative probabilities and, optionally,
#' a fitted CDF. Elicited are shown as filled circles, and
#' limits are shown as clear circles.
#'
#' @param lower lower limit for the uncertain quantity
#' @param v vector of values, for each value x in Pr(X<=x) = p
#' in the set of elicited probabilities
#' @param p vector of probabilities, for each value p in Pr(X<=x) = p
#' in the set of elicited probabilities
#' @param upper upper limit for the uncertain quantity
#' @param fontsize font size to be used in the plot
#' @param fit object of class \code{elicitation}
#' @param dist the fitted distribution to be plotted. Options are
#' \code{"normal"}, \code{"t"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, \code{"mirrorgamma"},
#' \code{"mirrorlognormal"}, \code{"mirrorlogt"} \code{"hist"} (for a histogram fit)
#' @param showFittedCDF logical. Should a fitted distribution function
#' be displayed?
#' @param showQuantiles logical. Should quantiles from the fitted distribution function
#' be displayed?
#' @param ql a lower quantile to be displayed.
#' @param qu an upper quantile to be displayed.
#' @param ex if the object \code{fit} contains judgements from multiple experts,
#' which (single) expert's judgements to show.
#' @param sf number of significant figures to be displayed.
#' @param xaxisLower lower limit for the x-axis.
#' @param xaxisUpper upper limit for the x-axis.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' 
#' @examples
#' 
#' \dontrun{
#' vQuartiles <- c(30, 35, 45)
#' pQuartiles<- c(0.25, 0.5, 0.75)
#' myfit <- fitdist(vals = vQuartiles, probs = pQuartiles, lower = 0)
#' makeCDFPlot(lower = 0, v = vQuartiles, p = pQuartiles,
#'  upper = 100, fit = myfit, dist = "gamma",
#'  showFittedCDF = TRUE, showQuantiles = TRUE)
#' 
#' 
#' }
#'
#' @export

makeCDFPlot <- function(lower, v, p, upper, fontsize = 12,
                        fit = NULL, 
                        dist = NULL,
                        showFittedCDF = FALSE,
                        showQuantiles = FALSE,
                        ql = 0.05, 
                        qu = 0.95,
                        ex = 1,
                        sf = 3,
                        xaxisLower = lower,
                        xaxisUpper = upper,
                        xlab = "x",
                        ylab = expression(P(X<=x))){
  
  # Hack to avoid CRAN check NOTE
  
  x <- NULL
  
  
  
  p1 <- ggplot(data.frame(x = c(xaxisLower, xaxisUpper)), aes(x = x)) +
    annotate("point", x = v, y = p, size = 5) + 
    annotate("point", x = c(lower, upper), y = c(0, 1), size = 5, shape = 1)+
    labs(y = ylab, x = xlab) +
    scale_x_continuous(breaks = c(xaxisLower, xaxisUpper, v), 
                       minor_breaks = NULL,
                       limits = c(xaxisLower, xaxisUpper)) +
    scale_y_continuous(breaks = c(0, 1, p),
                       minor_breaks = NULL) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = fontsize))
  
  
  # Add in CDF
  
  if(showFittedCDF){
    if(dist == "hist"){
      dist.title <- "Histogram fit"
      p1 <- p1 + annotate("segment", x = c(lower, v),
                            y = c(0, p),
                            xend = c(v, upper),
                            yend = c(p, 1)) 
      if(showQuantiles){
        xl <- qhist(ql, c(lower, v, upper), c(0, p, 1))
        xu <- qhist(qu, c(lower, v, upper), c(0, p, 1))
          p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
                                  
    }
    if(dist == "normal"){
      if(is.na(fit$ssq[ex, "normal"])){
        dist.title <- "Normal distribution not fitted"
      }else{
      dist.title <- paste("Normal (mean = ",
                          signif(fit$Normal[ex,1], sf),
                          ", sd = ",
                          signif(fit$Normal[ex,2], sf), ")",
                          sep="")
      
      p1 <- p1 + stat_function(fun = pnorm, 
                             args = list(mean = fit$Normal[1, 1],
                                         sd = fit$Normal[1, 2])
                               )
      if(showQuantiles){
        xl <- qnorm(ql, mean = fit$Normal[1, 1], sd = fit$Normal[1, 2])
        xu <- qnorm(qu, mean = fit$Normal[1, 1], sd = fit$Normal[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
        }
      }
    }
    if(dist == "t"){
      if(is.na(fit$ssq[ex, "t"])){
        dist.title <- "Student-t distribution not fitted"
      }else{
      dist.title=paste("Student-t(",
                       signif(fit$Student.t[ex,1], sf),
                       ", ",
                       signif(fit$Student.t[ex,2], sf),
                       ")",
                       sep="")
      
      tcdf <- function(x){pt((x - fit$Student.t[1, 1]) /
                               fit$Student.t[1, 2], fit$Student.t[1, 3])}
      p1 <- p1 + stat_function(fun = tcdf)
      
      if(showQuantiles){
        xl <- fit$Student.t[1, 1] + 
          fit$Student.t[1, 2] * qt(ql, fit$Student.t[1, 3])
        xu <- fit$Student.t[1, 1] + 
          fit$Student.t[1, 2] * qt(qu, fit$Student.t[1, 3])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
    }
    if(dist == "lognormal"){
      if(is.na(fit$ssq[ex, "lognormal"])){
        dist.title <- "Log normal distribution not fitted"
      }else{
      dist.title = paste("Log normal(",
                         signif(fit$Log.normal[ex,1], sf),
                         ", ",
                         signif(fit$Log.normal[ex,2], sf), ")",
                         sep="")
      
      lncdf <- function(x){
        plnorm(x - lower, 
               meanlog = fit$Log.normal[1, 1],
               sdlog = fit$Log.normal[1, 2])
        
      }
      p1 <- p1 + stat_function(fun = lncdf)
      
      if(showQuantiles){
        xl <- lower + qlnorm(ql, meanlog = fit$Log.normal[1, 1],
                             sdlog = fit$Log.normal[1, 2])
        xu <- lower + qlnorm(qu, meanlog = fit$Log.normal[1, 1],
                             sdlog = fit$Log.normal[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
      
    }
    
    if(dist == "mirrorlognormal"){
      if(is.na(fit$ssq[ex, "mirrorlognormal"])){
        dist.title <- "Mirror log normal distribution not fitted"
      }else{
      dist.title = paste("Mirror log normal(",
                         signif(fit$mirrorlognormal[ex,1], sf),
                         ", ",
                         signif(fit$mirrorlognormal[ex,2], sf), ")",
                         sep="")
      
      mirrorlncdf <- function(x){
        1- plnorm(upper - x, 
               meanlog = fit$mirrorlognormal[1, 1],
               sdlog = fit$mirrorlognormal[1, 2])
        
      }
      p1 <- p1 + stat_function(fun = mirrorlncdf)
      
      if(showQuantiles){
        xl <- upper - qlnorm(1 - ql, meanlog = fit$mirrorlognormal[1, 1],
                             sdlog = fit$mirrorlognormal[1, 2])
        xu <- upper -  qlnorm(1 - qu, meanlog = fit$mirrorlognormal[1, 1],
                             sdlog = fit$mirrorlognormal[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
      
    }
    
    if(dist == "gamma"){
      if(is.na(fit$ssq[ex, "gamma"])){
        dist.title <- "Gamma distribution not fitted"
      }else{
      dist.title = paste("Gamma(",
                         signif(fit$Gamma[ex,1], sf),
                         ", ",
                         signif(fit$Gamma[ex,2], sf),
                         ")", sep="")
      
      gcdf <- function(x){pgamma(x - lower, 
                                 shape = fit$Gamma[1, 1],
                                 rate = fit$Gamma[1, 2])}
      p1 <- p1 + stat_function(fun = gcdf)
      
      if(showQuantiles){
        xl <- lower + qgamma(ql,  
                             shape = fit$Gamma[1, 1],
                             rate = fit$Gamma[1, 2])
        xu <- lower + qgamma(qu,  
                             shape = fit$Gamma[1, 1],
                             rate = fit$Gamma[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
    }
    
    if(dist == "mirrorgamma"){
      if(is.na(fit$ssq[ex, "mirrorgamma"])){
        dist.title <- "Mirror gamma distribution not fitted"
      }else{
      dist.title = paste("Mirror gamma(",
                         signif(fit$mirrorgamma[ex,1], sf),
                         ", ",
                         signif(fit$mirrorgamma[ex,2], sf),
                         ")", sep="")
      
      mirrorgcdf <- function(x){1 - pgamma(upper - x, 
                                 shape = fit$mirrorgamma[1, 1],
                                 rate = fit$mirrorgamma[1, 2])}
      p1 <- p1 + stat_function(fun = mirrorgcdf)
      
      if(showQuantiles){
        xl <- upper - qgamma(1 - ql,  
                             shape = fit$mirrorgamma[1, 1],
                             rate = fit$mirrorgamma[1, 2])
        xu <- upper - qgamma(1 - qu,  
                             shape = fit$mirrorgamma[1, 1],
                             rate = fit$mirrorgamma[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
    }
    
    if(dist == "logt"){
      if(is.na(fit$ssq[ex, "logt"])){
        dist.title <- "Log Student-t distribution not fitted"
      }else{
      dist.title = paste("Log T(",
                         signif(fit$Log.Student.t[ex,1], sf),
                         ", ",
                         signif(fit$Log.Student.t[ex,2], sf),
                         ")", sep="")
      
      lntcdf <- function(x){
        # Need to handle case of x < lower
        
        p <- pt((log(abs(x - lower)) - fit$Log.Student.t[1, 1]) /
                  fit$Log.Student.t[1, 2], 
                fit$Log.Student.t[1, 3])
        p[x <= lower] <- 0
        p
      }
      p1 <- p1 + stat_function(fun = lntcdf)
      
      if(showQuantiles){
        xl <- lower + exp(fit$Log.Student.t[1, 1] + 
                            fit$Log.Student.t[1, 2] * 
                            qt(ql, fit$Log.Student.t[1, 3]))
        xu <- lower + exp(fit$Log.Student.t[1, 1] + 
                            fit$Log.Student.t[1, 2] * 
                            qt(qu, fit$Log.Student.t[1, 3]))
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      
    }
    }
    if(dist == "mirrorlogt"){
      if(is.na(fit$ssq[ex, "mirrorlogt"])){
        dist.title <- "Mirror log Student-t distribution not fitted"
      }else{
      dist.title = paste("Mirror log T(",
                         signif(fit$mirrorlogt[ex,1], sf),
                         ", ",
                         signif(fit$mirrorlogt[ex,2], sf),
                         ")", sep="")
      
      mirrorlntcdf <- function(x){
        # Need to handle case of x > upper
        
        p <- 1 - pt((log(abs(upper - x)) - fit$mirrorlogt[1, 1]) /
                  fit$mirrorlogt[1, 2], 
                fit$mirrorlogt[1, 3])
        p[x >= upper] <- 1
        p
      }
      p1 <- p1 + stat_function(fun = mirrorlntcdf)
      
      if(showQuantiles){
        xl <- upper -  exp(fit$mirrorlogt[1, 1] + 
                            fit$mirrorlogt[1, 2] * 
                            qt(1-ql, fit$mirrorlogt[1, 3]))
        xu <- upper -  exp(fit$mirrorlogt[1, 1] + 
                            fit$mirrorlogt[1, 2] * 
                            qt(1 - qu, fit$mirrorlogt[1, 3]))
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      
    }
    }
      
    if(dist == "beta"){
      
      if(is.na(fit$ssq[ex, "beta"])){
        dist.title <- "Beta distribution not fitted"
      }else{
      
      dist.title =paste("Beta(",
                        signif(fit$Beta[ex,1], sf),
                        ", ", signif(fit$Beta[ex,2], sf),
                        ")", sep="")
      
     
      bcdf <- function(x){pbeta((x - lower) / (upper - lower), 
                                 shape1 = fit$Beta[1, 1],
                                 shape2 = fit$Beta[1, 2])}
      p1 <- p1 + stat_function(fun = bcdf)
      
      if(showQuantiles){
        xl <- lower + (upper - lower) * qbeta(ql, 
                                              shape1 = fit$Beta[1, 1],
                                              shape2 = fit$Beta[1, 2])
        xu <- lower + (upper - lower) * qbeta(qu, 
                                              shape1 = fit$Beta[1, 1],
                                              shape2 = fit$Beta[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      
      }
    }
  p1 <- p1 + labs(title = dist.title)  
  }
  
  p1
  
 }
