

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

#' @export
plotConditionalMedianFunction <- 
  function(yCP,
           xMed,
           yLimits = NULL,
           link = "identity",
           medianFunction = makeConditionalMedianFunction(yCP = yCP,
                                                          xMed = xMed,
                                                          link = link),
           xlab = "Y",
           ylab = "median of X given Y",
           fs = 12,
           ybreaks = NULL,
           xbreaks = NULL){
    
    x <- y <- NULL
    
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

  
  x <- sampleFit(fitX, n = N)[, dist]
  
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
  
  yh <- NULL
  
  dfDensity <- data.frame(x = x, yh = rep(y, each = N))
  p1 <- ggplot(dfDensity, aes(x = x, y = yh))+
    ggridges::geom_density_ridges(aes(group = yh), alpha = 0.5,
                                  fill = "steelblue")+
    labs(y = "Y", x = "X | Y") +
    scale_y_continuous(breaks = y) +
    theme_grey(base_size = fs)
  
  if(!is.null(xLimits)){
    p1 <- p1 + xlim(xLimits)
  }
  
  p1
}

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
