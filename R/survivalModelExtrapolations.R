#' Compare Multiple Fitted Models for Survival Extrapolation
#' 
#' Fits seven parametric models to an individual patient survival data set, 
#' displays extrapolations, and report the time point at which there is the
#' widest range in estimated extrapolated survival probabilities. This is intended
#' to support elicitation for survival extrapolation, in particular, to inform 
#' the choice extrapolation time.
#' 

#' @param survDf data frame with individual patient data. Require to be a .csv file with
#' three columns: "time", "event" and "treatment" (in that order).
#' Values in the "event" column should be 0 for a censored observation, and 1 otherwise.
#' The"treatment" column should be included even if there is only one treatment group.'
#' @param tEnd the maximum time point for extrapolation
#' @param group character variable to select treatment group: one of the levels in the
#' factor variable survDf$treatment
#' @param tTruncate optional argument: time point at which to censor all observations
#' @param showPlot whether to display the plot 
#' 
#' @return A list containing the elements
#' \item{KMplot}{a ggplot2 plot object;}
#' \item{tMaxRange}{the time point at which there is the greatest difference between the largest
#' and smallest extrapolated survival probability.} 
#' 
#' @examples
#' \dontrun{
#' 
#' # Make a data frame using the survival::veteran data frame
#' sdf <- survival::veteran[, c("time", "status", "trt")]
#' colnames(sdf) <- c("time", "event", "treatment")
#' sdf$treatment <- factor(sdf$treatment, labels = c("standard", "test"))
#' 
#' survivalModelExtrapolations(sdf, tEnd = 1000, group = "test", tTruncate = 100)
#' }
#' @import survival
#' @importFrom flexsurv flexsurvreg
#'@importFrom survminer ggsurvplot
#' @export



survivalModelExtrapolations <- function(survDf, tEnd, group, tTruncate = NULL,
                                showPlot = TRUE){
  
  if(!is.null(tTruncate)){
    index <- survDf$time > tTruncate
    survDf$event[index] <- 0
    survDf$time[index] <- tTruncate
    
  }
  
  tExtrapolate <- seq(from = 0, to = tEnd, length = 100 )
  
  sExtrapolate <- matrix(0, 100, 7 )
  dList <- c("exp", "weibull", "gamma", "gompertz", "llogis", "lnorm",
             "gengamma")
  for(i in 1:7){
    mf <- flexsurv::flexsurvreg(survival::Surv(time, event)~1,
                      data=survDf[survDf$treatment==group,], dist=dList[i])
    sExtrapolate[, i] <- summary(mf, t = tExtrapolate)[[1]][, "est"]
  }
  
  fit <- survival::survfit(survival::Surv(time, event) ~ 1,
                           data=survDf[survDf$treatment==group, ])
  myplot <- survminer::ggsurvplot(fit, data=sdf[survDf$treatment=="standard",],
                                  censor = FALSE,
                                  legend = "right",
                                  legend.title = "",
                                  xlim = c(0, tEnd),
                                  break.time.by = tEnd/4)
  
  for(i in 1:7){
    myplot$plot <- myplot$plot + annotate("line", x=tExtrapolate,
                                          y = sExtrapolate[, i], alpha = 0.5)
  }
  
  index <- which.max(diff(apply(sExtrapolate, 1, range)))
  
  myplot$plot <- myplot$plot + 
    geom_vline(xintercept = tExtrapolate[index], linetype = "dashed") +
    guides(colour = "none", fill = "none")
  
  
  if(showPlot){
    print(myplot$plot)
  }
  
  list(KMplot = myplot$plot, tMaxRange  = tExtrapolate[index])
  
}