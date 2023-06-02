#' Plot the fitted density function for one or more experts
#' 
#' Plots the fitted density function for one or more experts. Can also plot a
#' fitted linear pool if more than one expert. If plotting the density function
#' of one expert, or the linear pool only, can also indicated desired lower and
#' upper fitted quantiles.
#' 
#' 
#' @param fit An object of class \code{elicitation}.
#' @param d The distribution fitted to each expert's probabilities. Options are
#' \code{"normal"}, \code{"t"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, \code{"mirrorgamma"},
#' \code{"mirrorlognormal"}, \code{"mirrorlogt"} \code{"hist"} (for a histogram fit), and
#' \code{"best"} (for best fitting)
#' @param xl The lower limit for the x-axis. The default is the 0.001 quantile
#' of the fitted distribution (or the 0.001 quantile of a fitted normal
#' distribution, if a histogram fit is chosen).
#' @param xu The upper limit for the x-axis. The default is the 0.999 quantile
#' of the fitted distribution (or the 0.999 quantile of a fitted normal
#' distribution, if a histogram fit is chosen).
#' @param ql A lower quantile to be indicated on the density function plot.
#' Only displayed when plotting the density function for a single expert.
#' @param qu An upper quantile to be indicated on the density function plot.
#' Only displayed when plotting the density function for a single expert.
#' @param lp For multiple experts, set \code{lp = TRUE} to plot a linear pool.
#' @param ex If judgements have been elicited from multiple experts, but a
#' density plot for one expert only is required, the expert to be used in the
#' plot.
#' @param sf The number of significant figures to be displayed for the
#' parameter values.
#' @param ind If plotting a linear pool, set \code{ind = FALSE} to suppress
#' plotting of the individual density functions.
#' @param lpw A vector of weights to be used in linear pool, if unequal
#' weighting is desired.
#' @param fs The font size used in the plot.
#' @param lwd The line width used in the plot.
#' @param xlab A string or expression giving the x-axis label.
#' @param ylab A string or expression giving the y-axis label.
#' @param legend_full If plotting a linear pool, set \code{ind = TRUE} for each expert
#' to be plotted with a different colour, and \code{ind = FALSE} for each expert to be 
#' plotted with the same colour, reducing the legend size.
#' @param percentages Set to \code{TRUE} to use percentages on the x-axis.
#' @param returnPlot Set to \code{TRUE} to return the plot as a ggplot object.
#' @param showPlot Set to \code{FALSE} to suppress displaying the plot.
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
#' 
#' # Plot both fitted densities, using the best fitted distribution
#' plotfit(myfit)
#' 
#' # Plot a fitted beta distribution for expert 2, and show 5th and 95th percentiles
#' plotfit(myfit, d = "beta", ql = 0.05, qu = 0.95, ex = 2)
#' 
#' 
#' # Plot a linear pool, giving double weight to expert 1
#' plotfit(myfit,  lp = T, lpw = c(2,1))
#' 
#' 
#' # Plot a linear pool, giving double weight to expert 1, 
#' # show 5th and 95th percentiles, surpress plotting of individual distributions, 
#' # and force use of Beta distributions
#' plotfit(myfit, d = "beta",  lp = T, lpw = c(2,1), ql = 0.05, qu = 0.95, ind=FALSE )
#' }
#' @import graphics
#' @export
plotfit <- function(fit, 
                    d = "best",
                    xl = -Inf, 
                    xu = Inf, 
                    ql = NA, 
                    qu = NA, 
                    lp = FALSE, 
                    ex = NA, 
                    sf = 3, 
                    ind = TRUE, 
                    lpw = 1,
                    fs = 12,
                    lwd = 1,
                    xlab = "x",
                    ylab = expression(f[X](x)),
                    legend_full = TRUE,
                    percentages = FALSE,
                    returnPlot = FALSE,
                    showPlot = TRUE){
  
  # Error handling if fitted distribution is not available ----
  
  errorDist <- "Distribution has not been fitted. Requirements are"
  errorL <- "- finite lower limit"
  errorU <- "- finite upper limit"
  errorP <- "- smallest elicited probability < 0.4\n- largest elicited probability > 0.6"
  errorO <- "- at least one elicited probability, greater than 0 and less than 1"
  

  distributions <- c("histogram", "normal", "Student-t", "gamma",
                     "log normal", "log Student-t", "beta",
                     "mirror gamma", "mirror log normal",
                     "mirror log Student-t")
  
  if(is.na(ex)){
    index <- !is.na(c(0, fit$ssq[1, ]))}else{
      index <- !is.na(c(0, fit$ssq[ex, ]))
    }
  
  
  errorPlotBeta <- paste(errorDist, errorL, errorU, errorP,
                         "Available fitted distributions are:",
                         paste(distributions[index],
                               collapse = ", "),sep = "\n")
  errorPlotGamma <- paste(errorDist, errorL, errorO,
                          "Available fitted distributions are:",
                          paste(distributions[index],
                                collapse = ", "), sep = "\n")
  errorPlotLogNormal <- paste(errorDist, errorL, errorP,
                              "Available fitted distributions are:",
                              paste(distributions[index],
                                    collapse = ", "), sep = "\n")
  errorPlotMirrorGamma <- paste(errorDist, errorU, errorO,
                                "Available fitted distributions are:",
                                paste(distributions[index],
                                      collapse = ", "), sep = "\n")
  errorPlotMirrorLogNormal <-paste(errorDist, errorU, errorP,
                                   "Available fitted distributions are:",
                                   paste(distributions[index],
                                         collapse = ", "), sep = "\n")
  errorPlotNormal <- paste(errorDist, errorP,
                           "Available fitted distributions are:",
                           paste(distributions[index],
                                 collapse = ", "), sep = "\n")
  
  # If single expert chosen, check whether selected distributions fitted for that expert
  # If no expert specified, just check for any expert with missing selected distribution
  
  emptyPlot <- ggplot() +
    theme_void(base_size = fs) +
    xlim(0, 10)
  
  # If best fit chosen, find out if any parametric fits are available
  # either for all experts, or for selected expert
  # For histogram plot, need finite lower and upper limits
  
  if(is.na(ex)){
    noBestFit <- sum(!is.na(fit$ssq)) == 0
    noHistFit <- any(is.infinite(unlist(fit$limits)))
  }else{
    noBestFit <- sum(!is.na(fit$ssq[ex, ])) == 0
    noHistFit <- any(is.infinite(unlist(fit$limits[ex, ])))
  }
  
  # If distribution specified, check to see if it is available
  
  noFit <- TRUE
  
  if(d %in% colnames(fit$ssq)){
    if(is.na(ex)){
      noFit <- anyNA(fit$ssq[, d])
    }else{
      noFit <- anyNA(fit$ssq[ex, d])
    }
      
  }
  
  if(d == "hist" & noHistFit){
    return(emptyPlot + 
             annotate("text",0,0,
                      label="Histogram not available.\nFinite lower and upper limits required.",
                      hjust = 0, size = fs /2))
  }
  

  
  if(d=="best" & noBestFit){
    return(emptyPlot + 
             annotate("text",0,0,
                      label="No distributions fitted",
                      hjust = 0, size = fs /2))
    
  }
  
  
  
  if(d=="beta" & noFit ){
    return(emptyPlot + 
             annotate("text",0,0,
                           label=errorPlotBeta, hjust = 0, size = fs /2))
  }
  if((d=="normal" | d == "t") & noFit ){
    return(emptyPlot + 
             annotate("text",0,0,
                           label=errorPlotNormal, hjust = 0, size = fs /2))
  }
  if(d=="gamma" & noFit ){
    return(emptyPlot + 
             annotate("text",0,0,
                           label=errorPlotGamma, hjust = 0, size = fs / 2))
  }
  if((d=="lognormal" | d == "logt") & noFit ){
    return(emptyPlot + 
             annotate("text",0,0,
                           label=errorPlotLogNormal, hjust = 0, size = fs /2))
  }
  if(d=="mirrorgamma" & noFit ){
    return(emptyPlot + 
             annotate("text",0,0,
                           label=errorPlotMirrorGamma, hjust = 0, size = fs /2))
  }
  if((d=="mirrorlognormal" | d == "mirrorlogt") & noFit ){
    return(emptyPlot + 
             annotate("text",0,0,
                           label=errorPlotMirrorLogNormal, hjust = 0, size = fs /2))
  }
    


  # if(d=="beta" & (min(fit$limits) == -Inf | max(fit$limits) == Inf )){stop("Parameter limits must be finite to fit a beta distribution")}
  # if(d=="gamma" & min(fit$limits) == -Inf ){stop("Lower parameter limit must be finite to fit a (shifted) gamma distribution")}
  # if(d=="lognormal" & min(fit$limits) == -Inf ){stop("Lower parameter limit must be finite to fit a (shifted) log normal distribution")}
  # if(d=="logt" & min(fit$limits) == -Inf ){stop("Lower parameter limit must be finite to fit a (shifted) log t distribution")}
  # if(is.na(ql)==F & (ql <0 | ql>1 )){stop("Lower feedback quantile must be between 0 and 1")}
  # if(is.na(qu)==F & (qu <0 | qu>1 )){stop("Upper feedback quantile must be between 0 and 1")}
  
  theme_set(theme_grey(base_size = fs))
  theme_update(plot.title = element_text(hjust = 0.5))

  if(nrow(fit$vals)>1 & is.na(ex)==T & lp==F){
    if(xl == -Inf & min(fit$limits[,1]) > -Inf){xl <- min(fit$limits[,1]) }
    if(xu == Inf & max(fit$limits[,2]) < Inf){xu <- max(fit$limits[,2]) }
      p1 <- suppressWarnings(makeGroupPlot(fit, xl, xu, d, lwd, xlab, ylab,
                                           expertnames = rownames(fit$Normal)))
      if(showPlot){print(p1)}
      
      if(returnPlot){
        return(p1)
      }
  }
  
  if(nrow(fit$vals)>1 & lp==T){
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
    p1 <- makeLinearPoolPlot(fit, xl, xu,  d , lpw,
                                              lwd, xlab, ylab, legend_full,
                                              expertnames = rownames(fit$Normal)
                                              )
    if(showPlot){print(p1)}
    if(returnPlot){
      return(p1)
    }
    
  }
  
  if(nrow(fit$vals)>1 & is.na(ex)==F){
    if(xl == -Inf & fit$limits[ex,1] > -Inf){xl <- fit$limits[ex,1] }
    if(xu == Inf & fit$limits[ex,2] < Inf){xu <- fit$limits[ex,2] }
    p1 <- suppressWarnings(makeSingleExpertPlot(fit, d, 
                                                                 xl, xu, 
                                                                 ql, qu, 
                                                                 sf, ex = ex, 
                                                                 lwd, xlab, 
                                                                 ylab,
                                                                 percentages)
                                            
                                      )
    if(showPlot){print(p1)}
    if(returnPlot){
      return(p1)
    }
      
    
  }
  
 
  if(nrow(fit$vals)==1){
    
      p1 <- suppressWarnings(makeSingleExpertPlot(fit, d, xl, 
                                                  xu, ql, qu, sf, ex = 1,
                                                  lwd, xlab, ylab,
                                                  percentages))
      if(showPlot){print(p1)}
      
      if(returnPlot){
        return(p1)
      }
      
      
  }

}
