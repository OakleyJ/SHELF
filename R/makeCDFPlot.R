makeCDFPlot <- function(lower, v, p, upper, fontsize = 12,
                        fit = NULL, 
                        dist = NULL,
                        showFittedCDF = FALSE,
                        showQuantiles = FALSE,
                        ql = 0.05, 
                        qu = 0.95,
                        ex = 1,
                        sf = 3){
  
  # Hack to avoid CRAN check NOTE
  
  x <- NULL
  
  p1 <- ggplot(data.frame(x = c(lower, upper)), aes(x = x)) +
    annotate("point", x = v, y = p, size = 5) + 
    annotate("point", x = c(lower, upper), y = c(0, 1), size = 5, shape = 1)+
    labs(y = "P(X<=x)", x = "x") +
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
          addQuantileCDF(lower, xl, ql, upper) + 
          addQuantileCDF(lower, xu, qu, upper) 
      }
                                  
    }
    if(dist == "normal"){
      
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
          addQuantileCDF(lower, xl, ql, upper) + 
          addQuantileCDF(lower, xu, qu, upper) 
        }
      
    }
    if(dist == "t"){
      
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
          addQuantileCDF(lower, xl, ql, upper) + 
          addQuantileCDF(lower, xu, qu, upper) 
      }
      
    }
    if(dist == "lognormal"){
      
      dist.title = paste("Log normal(",
                         signif(fit$Log.normal[ex,1], sf),
                         ", ",
                         signif(fit$Log.normal[ex,2], sf), ")",
                         sep="")
      
      lncdf <- function(x){pnorm(log(x - lower), 
                                 mean = fit$Log.normal[1, 1],
                                 sd = fit$Log.normal[1, 2])}
      p1 <- p1 + stat_function(fun = lncdf)
      
      if(showQuantiles){
        xl <- lower + qlnorm(ql, meanlog = fit$Log.normal[1, 1],
                             sdlog = fit$Log.normal[1, 2])
        xu <- lower + qlnorm(qu, meanlog = fit$Log.normal[1, 1],
                             sdlog = fit$Log.normal[1, 2])
        p1 <- p1 + 
          addQuantileCDF(lower, xl, ql, upper) + 
          addQuantileCDF(lower, xu, qu, upper) 
      }
      
      
    }
    if(dist == "gamma"){
      
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
          addQuantileCDF(lower, xl, ql, upper) + 
          addQuantileCDF(lower, xu, qu, upper) 
      }
      
    }
    if(dist == "logt"){
      
      dist.title = paste("Log T(",
                         signif(fit$Log.Student.t[ex,1], sf),
                         ", ",
                         signif(fit$Log.Student.t[ex,2], sf),
                         ")", sep="")
      
      lntcdf <- function(x){pt((log(x - lower) - fit$Log.Student.t[1, 1]) /
                                 fit$Log.Student.t[1, 2], 
                               fit$Log.Student.t[1, 3])}
      p1 <- p1 + stat_function(fun = lntcdf)
      
      if(showQuantiles){
        xl <- lower + exp(fit$Log.Student.t[1, 1] + 
                            fit$Log.Student.t[1, 2] * 
                            qt(ql, fit$Log.Student.t[1, 3]))
        xu <- lower + exp(fit$Log.Student.t[1, 1] + 
                            fit$Log.Student.t[1, 2] * 
                            qt(qu, fit$Log.Student.t[1, 3]))
        p1 <- p1 + 
          addQuantileCDF(lower, xl, ql, upper) + 
          addQuantileCDF(lower, xu, qu, upper) 
      }
      
    }
    if(dist == "beta"){
      
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
          addQuantileCDF(lower, xl, ql, upper) + 
          addQuantileCDF(lower, xu, qu, upper) 
      }
      
    }
  p1 <- p1 + labs(title = dist.title)  
  }
  
  p1
  
 }
