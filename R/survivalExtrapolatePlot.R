#' Plot survival data and elicited extrapolated intervals
#'
#' Show Kaplan-Meier plot of available data, and credible
#' interval for extrapolated survivor function value S(T)
#'
#'
#' @param survDf data frame with individual patient data. Needs three columns with names
#' "time", "event" and "treatment" (in that order). For weighted observations (e.g. 
#' using propensity scores), include a fourth column "weights".
#' Values in the "event" column should be 0 for a censored observation, and 1 otherwise.
#' The "treatment" column should be included even if there is only one treatment group, and defined
#' as a factor variable. 
#' @param myfit1 object of class \code{elicitation}, obtained from \code{fitdist} function with
#' elicited judgements for the first treatment group. 
#' @param myfit2 object of class \code{elicitation}, obtained from \code{fitdist} function with
#' elicited judgements for the second treatment group, if there is on. 
#' @param fqDist1 fitted distribution family for first treatment group. Options are
#' \code{"normal"}, \code{"t"}, \code{"skewnormal"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, \code{"mirrorgamma"},
#' \code{"mirrorlognormal"}, \code{"mirrorlogt"} \code{"hist"} (for a histogram fit), and
#' \code{"best"} (for best fitting)
#' @param fqDist2 fitted distribution family for second treatment group if there is one. Options are
#' \code{"normal"}, \code{"t"}, \code{"skewnormal"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, \code{"mirrorgamma"},
#' \code{"mirrorlognormal"}, \code{"mirrorlogt"} \code{"hist"} (for a histogram fit), and
#' \code{"best"} (for best fitting)
#' @param tTruncate Optional argument to censor all observations at this time point.
#' @param tTarget Target time for extrapolation: judgements are elicited at S(t = tTarget)
#' @param alpha Size of probability interval to plot (100*\code{alpha}\% interval).
#' @param useWeights set to TRUE if survDf includes column of weights, as described in specification 
#' of survDf. This column is passed on to survival::survfit() as the case weights.
#' @param groups Vector of strings for the group labels. Will be extracted from factor levels of
#' \code{treatment} column in \code{survDf}.
#' @param xl x-axis label in plot
#' @param fontsize font size in plot
#' @param breakTime Optional argument to specify tick mark spacing on x-axis
#' @param showPlot set to TRUE to display the plot
#' @param returnPlot set to TRUE to return the plot as a ggplot object.
#'
#' @returns a ggplot object, if returnPlot is TRUE
#' @export
#' @examples
#' \dontrun{
#' sdf <- survival::veteran[, c("time", "status", "trt")]
#' colnames(sdf) <- c("time", "event", "treatment")
#' sdf$treatment <- factor(sdf$treatment, labels = c("standard", "test"))
#' 
#' sdf <- survival::veteran[, c("time", "status", "trt")]
#' colnames(sdf) <- c("time", "event", "treatment")
#' sdf$treatment <- factor(sdf$treatment, labels = c("standard", "test"))
#' groupStandardElicitation <- fitdist(vals = c(0.15, 0.2, 0.25),
#'                                     probs = c(0.25, 0.5, 0.75),
#'                                     lower = 0,
#'                                     upper = 1)
#' 
#' groupTestElicitation <- fitdist(vals = c(0.1, 0.15, 0.2),
#'                                 probs = c(0.25, 0.5, 0.75),
#'                                 lower = 0,
#'                                 upper = 1)
#' 
#' survivalExtrapolatePlot(sdf,
#'                         myfit1 = groupStandardElicitation,
#'                         myfit2 = groupTestElicitation,
#'                         fqDist1 = "beta",
#'                         fqDist2 = "beta",
#'                         tTruncate = 150,
#'                         tTarget=200,
#'                         alpha = 0.95)
#' }

survivalExtrapolatePlot <- function(survDf,
                                    myfit1,
                                    myfit2 = NULL,
                                    fqDist1 = "best",
                                    fqDist2 = NULL,
                                    tTruncate = max(survDf$time),
                                    tTarget,
                                    alpha = 0.95,
                                    useWeights = FALSE,
                                    groups = levels(survDf$treatment),
                                    xl = "Time (t)",
                                    fontsize = 12,
                                    breakTime = NULL,
                                    showPlot = TRUE,
                                    returnPlot = FALSE){
  # Truncate data frame for plotting
  index <- survDf$time > tTruncate
  truncatedDf <- survDf
  truncatedDf$event[index] <- 0
  truncatedDf$time[index] <- tTruncate
  
  # Prepare KM plot
  
  if(useWeights == TRUE){
    fit <- survival::survfit(survival::Surv(time, event) ~ treatment,
                             weights = weights,
                             data = truncatedDf)
    
  }else{
    fit <- survival::survfit(survival::Surv(time, event) ~ treatment,
                             data = truncatedDf)
  }
  
  myplot <- survminer::ggsurvplot(fit, data = truncatedDf, censor = FALSE,
                                  break.time.by = breakTime,
                                  legend = "right",
                                  legend.title = "",
                                  legend.labs = groups,
                                  xlim = c(0, tTarget),
                                  xlab = xl,
                                  conf.int = TRUE,
                                  break.y.by = 0.2)
  myplot$plot <- myplot$plot + geom_vline(xintercept = tTarget, linetype="dotted") +
    theme_bw(base_size = fontsize) + labs(y = "S(t)")
  
  # Extract feedback quantiles
  
  if(fqDist1 == "best"){
    fqDist1 <- myfit1$best.fitting[1, 1]
  }
  
  fq1<- feedback(myfit1,
                 quantiles=c((1-alpha)/2,
                             0.5,
                             1-(1-alpha)/2))$fitted.quantiles[, fqDist1]
  
  if(length(levels(survDf$treatment)) == 1){
    jitter <- 1
  }else{
    jitter <- 1.005
  }
  
  
  
  myplot$plot <- myplot$plot + annotate("errorbar", x = jitter*tTarget,
                          y = fq1[2],
                          ymin = fq1[1],
                          ymax = fq1[3],
                          colour = "#F8766D",
                          linewidth = 1.1,
                          width = 0.02 * tTarget) 
  
  if(length(levels(survDf$treatment)) > 1){
    
    if(fqDist2 == "best"){
      fqDist2 <- myfit2$best.fitting[1, 1]
    }
    
    fq2<- feedback(myfit2,
                   quantiles=c((1-alpha)/2,
                               0.5,
                               1-(1-alpha)/2))$fitted.quantiles[, fqDist2]
    myplot$plot <- myplot$plot +
      annotate("errorbar", x = 0.995 * tTarget,
               y = fq2[2],
               ymin = fq2[1],
               ymax = fq2[3],
               colour = "#00BFC4",
               linewidth = 1.1,
               width = 0.02 * tTarget)
  }
  
  if(showPlot){
    print(myplot$plot)
  }
  
  if(returnPlot){
    return(myplot$plot)
  }
  
  
}