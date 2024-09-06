#' Scenario Testing for Survival Extrapolation
#' 
#' Provides a plot and approximate 95% credible interval for an 
#' extrapolated surival time, based on a assumption of constant hazard 
#' after some specified time. Intended to be used as part of the SHELF protocol
#' for elicitation for survival extrapolation.
#' 
#' @param tLower lower limit for x-axis.
#' @param tUpper upper limit for x-axis.
#' @param expLower start time at which constant hazard is assumed.
#' @param expUpper end time for using data to estimate constant hazard;
#' data after this time will be censored.
#' @param tTarget target extrapolation time.
#' @param survDf data frame with individual patient data. Require to bea .csv file with
#' three columns: "time", "event" and "treatment" (in that order).
#' Values in the "event" column should be 0 for a censored observation, and 1 otherwise.
#' The"treatment" column should be included even if there is only one treatment group.'
#' @param groups character vector of names of the treatment group. Extracted from survDF by default.
#' @param expGroup selected treatment group for extrapolating
#' @param xl x-axis label
#' @param fontsize plot fontsize
#' @param showPlot whether to display the plot
#' 
#' @return A list containing the elements
#' \item{KMplot}{a ggplot2 plot object;}
#' \item{interval}{an approximate 95% credible interval for the survival proportion
#' at the target extrapolation time.} 
#' 
#' @examples
#' \dontrun{
#' sdf <- survival::veteran[, c("time", "status", "trt")]
#' colnames(sdf) <- c("time", "event", "treatment")
#' sdf$treatment <- factor(sdf$treatment, labels = c("standard", "test"))
#' survivalScenario(tLower = 0,tUpper = 150, expLower = 100, expUpper = 150,
#' tTarget = 250, survDf = sdf,
#' expGroup = "standard")
#' }
#' @import survival
#' @importFrom survminer ggsurvplot
#' @export

survivalScenario <- function(tLower = 0,
                          tUpper,
                          expLower,
                          expUpper,
                          tTarget,
                          survDf,
                          groups = levels(survDf$treatment),
                          expGroup = levels(survDf$treatment)[1],
                          xl = "Time",
                          fontsize = 12,
                          showPlot = TRUE){
  
  x <- ymin <- ymax <- NULL # hack to avoid R CMD check NOTE
  
  # Truncate data frame for plotting
  index <- survDf$time > tUpper
  truncatedDf <- survDf
  truncatedDf$event[index] <- 0
  truncatedDf$time[index] <- tUpper
  
  # Prepare KM plot
  fit <- survival::survfit(survival::Surv(time, event) ~ treatment, data = truncatedDf)
  myplot <- survminer::ggsurvplot(fit, data = truncatedDf, censor = FALSE,
                                  legend = "right",
                                  legend.title = "",
                                  legend.labs = groups,
                                  xlim = c(tLower, tTarget),
                                  conf.int = TRUE, conf.type = "plain",
                                  breaks = c(tLower, expLower, expUpper, tTarget),
                                  name = xl)
  
  myplot$plot <- myplot$plot + geom_vline(xintercept = tTarget, linetype="dotted") +
    theme_bw(base_size = fontsize) 
    
 
    suppressMessages( myplot$plot <-  myplot$plot + 
                        scale_x_continuous(breaks = c(tLower, expLower, expUpper, tTarget)))
  
    
  # extract S(t) and Var(S(t)) for t = expLower (beginning of constant hazard period)
  dfExp <- truncatedDf[truncatedDf$treatment == expGroup, ]
  fitExp <- survival::survfit(survival::Surv(time, event) ~ 1, data = dfExp)
  Plower <- summary(fitExp, times = expLower)$surv
  mP <- summary(fitExp, times = expLower)$surv
  sP <- summary(fitExp, times = expLower)$std.err
  
    
  # Modify dfExp data frame for exponential fitting: discard observations less than expLower
  # and censor all observations above expUpper
  
  expUpper <- min(expUpper, max(dfExp$time))
    
  index <- dfExp$time > expLower # only want observations for time > expLower for exponential fitting
  eventTruncated <- dfExp$event
  eventTruncated[dfExp$time > expUpper] <- 0 # treat all observations > expUpper as censored
  eventTruncated <- eventTruncated[index]
  
  timeTruncated <- dfExp$time[index]
  timeTruncated[timeTruncated > expUpper] <- expUpper
    
  expFit <- survival::survreg(survival::Surv(time = timeTruncated-expLower,
                                             event = eventTruncated) ~ 1, dist = "exponential")
    
    
    
  lambda <- exp(-expFit$coefficients)
  mLogLambda <- summary(expFit)$table[1]
  sLogLambda <- summary(expFit)$table[2]
    
  myfun <- function(x) exp(-lambda*(x-expLower))*mP
    
  # Draw lines to indicate exponential fitting (solid) and extrapolation (dashed)  
    
    myplot$plot <- myplot$plot + 
      stat_function(fun = myfun, xlim = c(expLower, expUpper), lwd = 1.5, alpha = 0.9) +
      stat_function(fun = myfun, xlim = c(expUpper, tTarget), linetype = "dashed")
    
    tVals <- seq(from = expLower, to = tTarget, length = 100)
    
    
    
    # Sample random S(T) values for target time T
    
    expMatrix <- matrix(0, 10000, 100)
    randomP <- exp(rnorm(10000, log(mP), sP/mP))
   
    
    randomLambda <- exp(-rnorm(10000, mLogLambda, sLogLambda ))
    for(i in 1:10000){
      expMatrix[i, ] <- exp(-randomLambda[i]*(tVals-expLower))*randomP[i]
    }
    
    # discard samples out of bounds
    expMatrix <- expMatrix[(randomP >= 0 ) & (randomP <= 1), ]
    
      
      ribbon_data <- data.frame(x =tVals,
                                ymin = apply(expMatrix, 2, quantile, probs = 0.025),
                                ymax = apply(expMatrix, 2, quantile, probs = 0.975),
                                surv =tVals)
      interval <- as.numeric(ribbon_data[100, 2:3])
      names(interval) <- c("2.5%", "97.5%")
      
      myplot$plot <- myplot$plot +
        geom_ribbon(data = ribbon_data,
                    aes(x = x, ymin = ymin, ymax = ymax),
                    fill = "gray", alpha = 0.5)
  if(showPlot){
   print(myplot$plot)
  }
      list(KMplot = myplot$plot, 
           interval = interval )
      
}
