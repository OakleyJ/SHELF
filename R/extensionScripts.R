

makeConditionalMedianFunction <- function(yCP,
                                          xMed,
                                          link = "identity") {
  if(link == "identity"){
    myfunc <- function(yCondition) {
      Hmisc::approxExtrap(x = yCP, y = xMed, xout = yCondition)$y
    }
  }
  
  if(link == "log"){
    myfunc <- function(yCondition) {
      exp(Hmisc::approxExtrap(x = yCP,
                              y = log(xMed),
                              xout = yCondition)$y)
    }
  }
  
  if(link == "logit"){
    myfunc <- function(yCondition) {
      logodds <- Hmisc::approxExtrap(x = yCP,
                                     y = log(xMed / (1 - xMed)),
                                     xout = yCondition)$y
      exp(logodds) / (1 + exp(logodds))
    }
  }
  attr(myfunc, "link") <- link
  myfunc
}

#' Plot the conditional median function
#' 
#' Produces a plot of the conditional median function, given a set of 
#' conditioning points for the extension variable, a set of corresponding
#' medians of the target variable, given the extension variable, and a choice
#' of link. The identity link is the default, a log link can be used for 
#' non-negative target variables, and a logit link can be used for target 
#' variables constrained to lie between 0 and 1.
#' 
#' @param yCP vector of conditioning points for the extension variable.
#' @param xMed vector of medians of the target variable, corresponding to
#' each value of the extension variable in \code{yCP}.
#' @param yLimits limits for the extension variable, used to set the axis limits
#' in the plot
#' @param link link in the median function. One of \code{"identity"},
#'  \code{"log"} or \code{"logit"}.
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param fs font size
#' @param ybreaks tick marks on the y-axis
#' @param xbreaks tick marks on the axis
#'
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' plotConditionalMedianFunction(yCP = c(3, 5, 7, 9.5, 13.5),
#'  xMed = c(2, 6.5, 9, 13, 20),
#'  yLimits = c(0, 20),
#'  link = "log")
#'  
#' plotConditionalMedianFunction(yCP = c(2, 4, 6, 8, 10),
#'  xMed = c(0.1, 0.3, 0.5, 0.7, 0.9),
#'  yLimits = c(0, 15),
#'  link = "logit")
#'  
#' }
#'
#' @export
plotConditionalMedianFunction <- 
  function(yCP,
           xMed,
           yLimits = NULL,
           link = "identity",
           xlab = "Y",
           ylab = "median of X given Y",
           fs = 12,
           ybreaks = NULL,
           xbreaks = NULL){
    
    x <- y <- NULL
    
    
    medianFunction <- makeConditionalMedianFunction(yCP = yCP,
                                                   xMed = xMed,
                                                   link = link)
    
    df1 <- data.frame(x = yCP, y = xMed)
    
    ySeqMiddle <- seq(from = min(yCP), to = max(yCP), length = 100)
    
    
    
    p1 <- ggplot(df1, aes(x = x, y = y)) +
      geom_point(size = 2) +
      annotate("path", x = ySeqMiddle, 
               y = medianFunction(ySeqMiddle),
               size = 1, alpha = 0.5) +
      theme_grey(base_size = fs)
    
    if(is.null(ybreaks)){
      p1 <- p1 + scale_y_continuous(breaks = xMed, name = ylab,
                         minor_breaks = NULL) 
    }else{
      p1 <- p1 + scale_y_continuous(breaks = ybreaks, name = ylab,
                                    minor_breaks = NULL) 
    }
    
    if(is.null(xbreaks)){
      p1 <- p1 + scale_x_continuous(breaks = yCP, name = xlab,
                                    minor_breaks = NULL) 
    }else{
      p1 <- p1 + scale_x_continuous(breaks = xbreaks, name = xlab,
                                    minor_breaks = NULL)  
    }
    
    if(!is.null(yLimits)){
      if(length (yLimits) == 2 &
         min(yLimits) < min(yCP) &
         max(yLimits) > max(yCP)){
        
        ySeqUpper <- seq(from = max(yCP), to = max(yLimits), length = 100)
        ySeqLower <- seq(from = min(yLimits), to = min(yCP), length = 100)
        
        p1 <- p1 + annotate("path", x = ySeqLower, 
                            y = medianFunction(ySeqLower),
                            linetype = "dashed",
                            size = 1, alpha = 0.5) +
          annotate("path", x = ySeqUpper, 
                   y = medianFunction(ySeqUpper),
                   linetype = "dashed",
                   size = 1, alpha = 0.5) 
      }
      
    }
    
    p1
    
  }

#' Plot density of the target variable, conditional on the extension variable
#' 
#' Plots kernel density estimates of the target variable, conditional on 
#' each of a set of specified values of the extension variable. The plot
#' makes use of the function \code{ggridges::geom_density_ridges()}, and so
#' uses kernel density estimates rather than the exact conditional density 
#' function.
#' 
#' @param y vector of values for the extension variable
#' at which to condition on.
#' @param fitX an object of class \code{elicitation} specifying the
#' c-distribution: the distribution of the target variable, conditional on the
#' extension variable taking its median value.
#'@param yCP vector of conditioning points for the extension variable.
#' @param xMed vector of medians of the target variable, corresponding to
#' each value of the extension variable in \code{yCP}.
#' @param medianY the median value of the extension variable.
#' @param link link in the median function. One of \code{"identity"},
#' \code{"log"} or \code{"logit"}
#' @param dist choice of parametric distribution for the c-distribution. Options are
#' \code{"normal"}, \code{"t"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, \code{"hist"} (for a histogram fit), and
#' \code{"best"} (for best fitting).
#' @param N sample size used in the kernel density estimate
#' @param xLimits x-axis limits
#' @param fs font size
#' @examples
#' \dontrun{
#' 
#' myfitX <- fitdist(vals = c(5.5, 9, 14),
#'  probs = c(0.25, 0.5, 0.75),
#'  lower = 0)
#' 
#' plotConditionalDensities(y = c(2, 6, 10),
#'  fitX = myfitX,
#'  yCP = c(3, 5, 7, 9.5, 13.5),
#'  xMed = c(2, 6.5, 9, 13, 20),
#'  medianY = 7,
#'  link = "log",
#'  dist = "lognormal",
#'  xLimits = c(0, 60))
#' 
#'   
#' # Example with the logit link
#' 
#' myfitXlogit <- fitdist(vals = c(0.2, 0.25, 0.3),
#'  probs = c(0.25, 0.5, 0.75),
#'  lower = 0, 
#'  upper = 1)
#'  
#'  plotConditionalDensities(y = c(2, 6, 10),
#'   fitX = myfitXlogit, 
#'   yCP = c(2, 4, 6, 8, 10),
#'   xMed = c(0.1, 0.3, 0.5, 0.7, 0.9),
#'   medianY = 6,
#'   link = "logit",
#'   dist = "beta")
#'  
#' }
#'
#' @export
plotConditionalDensities <- function(y,
                                     fitX,
                                     yCP,
                                     xMed,
                                     medianY,
                                     link = "identity",
                                     dist = "best",
                                     N = 100000,
                                     xLimits = NULL,
                                     fs = 12){
  
  medianFunction <- makeConditionalMedianFunction(yCP = yCP,
                                                  xMed = xMed,
                                                  link = link)
  
  if(dist == "best"){
    dist <- as.character(fitX$best.fitting[1, ])
  }

  
  x <- rep(sampleFit(fitX, n = N)[, dist], length(y))
  
  if(link == "identity"){
    locshift <- rep(medianFunction(y),
                    each = N) - 
      rep(medianFunction(medianY), N * length(y))
    x <- x + locshift
  }
  if(link == "log"){
    scaleshift <- rep(medianFunction(y),
                      each = N) / 
      rep(medianFunction(medianY), N * length(y))
    x <- x * scaleshift
  }
  
  if(link == "logit"){
    if(min(x)<0 | max(x)>1){
      warning('sampled x values are outside the interval [0, 1]. Try setting dist = "beta".')
    }
    m <- rep(medianFunction(y), each = N)
    m3 <- rep(medianFunction(medianY), N * length(y))
    oddsRatio <- (1 - m) / m * m3 / (1 - m3)
    x <- x / (oddsRatio - (oddsRatio - 1) * x)
  }
  
  
  yh <- NULL
  
  dfDensity <- data.frame(x = x, yh = rep(y, each = N))
  p1 <- ggplot(dfDensity, aes(x = x, y = yh))+
    ggridges::geom_density_ridges(aes(group = yh), alpha = 0.5,
                                  fill = "steelblue")+
    labs(y = "Y", x = "X | Y") +
    scale_y_continuous(breaks = y, minor_breaks = NULL) +
    theme_grey(base_size = fs)
  
  if(!is.null(xLimits)){
    p1 <- p1 + xlim(xLimits)
  }
  
  p1
}


#' Sample from the marginal distribution of the target variable
#' 
#' As part of the Extension Method, this function will generate a random 
#' sample from the marginal distribution of the target variable, using
#' a sample from the marginal distribution of the extension variable, 
#' the specified c-distribution, and the appropriate judgements used to
#' construct the median model. 
#' 
#' @param fitX an object of class \code{elicitation} specifying the
#' c-distribution: the distribution of the target variable, conditional on the
#' extension variable taking its median value.
#' @param sampleY a sample from the marginal distribution of the extension 
#' variable.
#' @param yCP vector of conditioning points for the extension variable.
#' @param xMed vector of medians of the target variable, corresponding to
#' each value of the extension variable in \code{yCP}.
#' @param medianY the median value of the extension variable.
#' @param dist choice of parametric distribution for the c-distribution. Options are
#' \code{"normal"}, \code{"t"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, \code{"hist"} (for a histogram fit), and
#' \code{"best"} (for best fitting).
#' @param link link in the median function. One of \code{"identity"},
#' \code{"log"} or \code{"logit"}
#' 
#' @return a vector containing a sample from the marginal distribution of 
#' the target variable.
#' 
#' @examples
#' \dontrun{
#' 
#' myfitX <- fitdist(vals = c(5.5, 9, 14),
#'  probs = c(0.25, 0.5, 0.75),
#'  lower = 0)
#' ry <- rgamma(10, 5.19, 0.694)
#' sampleMarginalFit(fitX = myfitX, 
#'  sampleY = ry,
#'  medianY = 7,
#'  yCP = c(3, 5, 7, 9.5, 13.5),
#'  xMed = c(2, 6.5, 9, 13, 20),
#'  dist = "lognormal",
#'  link = "log")
#'  }
#' @export
sampleMarginalFit <- function(fitX, 
                              sampleY,
                              medianY,
                              yCP,
                              xMed,
                              dist = "best",
                              link = "identity"){
  
  medianFunction <- makeConditionalMedianFunction(yCP = yCP,
                                                  xMed = xMed,
                                                  link = link)
  
  if(dist == "best"){
    dist <- as.character(fitX$best.fitting[1, 1])
  }
  xSample <- sampleFit(fitX, n = length(sampleY))[, dist]
  
  if(link == "identity"){
    locshift <- medianFunction(sampleY) -
      medianFunction(medianY)
    xSample <- xSample + locshift
  }
  
  if(link == "log"){
    scaleshift <- medianFunction(sampleY) /
      medianFunction(medianY)
    xSample <- xSample * scaleshift
  }
  
  if(link == "logit"){
    m <- medianFunction(sampleY)
    m3 <- medianFunction(medianY)
    oddsRatio <- (1 - m) / m * m3 / (1 - m3)
    xSample <- xSample / (oddsRatio - (oddsRatio - 1) * xSample)
  }
  
  
  xSample
}
