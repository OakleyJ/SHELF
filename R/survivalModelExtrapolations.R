#' Compare Multiple Fitted Models for Survival Extrapolation
#' 
#' Fits seven parametric models to an individual patient survival data set (using the \code{flexsurv}
#' package), 
#' displays extrapolations, and report the time point at which there is the
#' widest range in estimated extrapolated survival probabilities. This function is intended to be used
#' only as an informal exploratory tool to support elicitation for survival extrapolation,
#' specifically, to inform the choice of target extrapolation time. The fitted models 
#' are exponential, weibull, gamma, gompertz, log logistic, log normal and geneneralised gamma.
 

#' @param survDf data frame with individual patient data. Require to be a .csv file with
#' three columns: "time", "event" and "treatment" (in that order).
#' Values in the "event" column should be 0 for a censored observation, and 1 otherwise.
#' The"treatment" column should be included even if there is only one treatment group.'
#' @param tOffset discard observations with time less than this value, and fit survival 
#' distributions to \code{survDf$time - tOffset}.  
#' @param tEnd the maximum time point for extrapolation
#' @param group character variable to select treatment group: one of the levels in the
#' factor variable survDf$treatment
#' @param tTruncate optional argument: time point at which to censor all observations
#' @param dists character vector of distributions to fit. Default is \code{c("exp", "weibull",
#' "gamma", "gompertz", "llogis", "lnorm", "gengamma")} corresponding to the distributions listed
#' above; can choose a subset of this.
#' @param nModels how many fitted models to plot, up to a maximum of 7, chosen by lowest AIC
#' value. Default is \code{length(dists)}.
#' @param showPlot whether to display the plot 
#' @return A list containing the elements
#' \item{KMplot}{a ggplot2 plot object;}
#' \item{tMaxRange}{the time point at which there is the greatest difference between the largest
#' and smallest extrapolated survival probability (if more than one distribution fitted);}
#' \item{modelAIC}{the AIC for each fitted model.} 
#' \item{lclExtrapolate; uclExtrapolate}{pointwise 95 percent confidence interval for extrapolated survivor functions}
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



survivalModelExtrapolations <- function(survDf, tOffset = 0,
                                        tEnd, group, tTruncate = NULL,
                                        dists = c("exp", "weibull",
                                                  "gamma", "gompertz",
                                                  "llogis", "lnorm",
                                                  "gengamma"),
                                        nModels = length(dists),
                                showPlot = TRUE){
  
  sdf <- NULL # hack to avoid R CMD check NOTE
  
  if(!is.null(tTruncate)){
    index <- survDf$time > tTruncate
    survDf$event[index] <- 0
    survDf$time[index] <- tTruncate
    
  }
  
  # Models are only fit to times >= tOffset
  index <- survDf$time >= tOffset
  survDfReduced <- survDf[index, ]
  
  tExtrapolate <- seq(from = 0, to = tEnd, length = 100 )
  
  sExtrapolate <- matrix(0, 100, length(dists) )
  allAIC <- rep(0, length(dists))
  
  lclExtrapolate <- uclExtrapolate <- matrix(0, 100, length(dists))
  
  colnames(lclExtrapolate) <- colnames(uclExtrapolate) <- 
    colnames(sExtrapolate) <- dists
  
  for(i in 1:length(dists)){
    mf <- flexsurv::flexsurvreg(survival::Surv(time - tOffset, event)~1,
                      data=survDfReduced[survDfReduced$treatment==group,],
                      dist=dists[i])
    exSummary <- summary(mf, t = tExtrapolate - tOffset)[[1]]
    sExtrapolate[, i] <- exSummary[, "est"]
    lclExtrapolate[, i] <- exSummary[, "lcl"]
    uclExtrapolate[, i] <- exSummary[, "ucl"]
    allAIC[i] <- mf$AIC
  }
  
  # Extract Kaplan-Meier curve, including estimate at time t = tOffset
  
 
  
  fit <- survival::survfit(survival::Surv(time, event) ~ 1,
                           data=survDf[survDf$treatment==group, ])
  mP <- summary(fit, times = tOffset)$surv
  myplot <- survminer::ggsurvplot(fit, data=survDf[survDf$treatment=="standard",],
                                  censor = FALSE,
                                  legend = "right",
                                  legend.title = "",
                                  xlim = c(0, tEnd),
                                  break.time.by = tEnd/4)
  
  index <- tExtrapolate >= tOffset
  AICrank <- order(allAIC)
  
  for(i in 1:min(nModels, length(dists))){
    myplot$plot <- myplot$plot + annotate("line", x=tExtrapolate[index],
                                          y = mP*sExtrapolate[index, AICrank[i]], alpha = 0.5)
  }
  
  index <- which.max(diff(apply(sExtrapolate, 1, range)))
  tMaxRange  <- tExtrapolate[index]
  if(length(dists == 1)){
    tMaxRange <- NULL
  }
  
  if(length(dists) >1){
    myplot$plot <- myplot$plot + 
      geom_vline(xintercept = tExtrapolate[index], linetype = "dashed")
  }
  
  myplot$plot <- myplot$plot + 
    guides(colour = "none", fill = "none")
  
  
  if(showPlot){
    print(myplot$plot)
  }
  
  modelAIC  <- allAIC[AICrank]
  names(modelAIC) <- dists[AICrank]
  
  list(KMplot = myplot$plot, tMaxRange  = tExtrapolate[index],
       modelAIC = modelAIC,
       tExtrapolate = tExtrapolate,
       mExtrapolate = sExtrapolate,
       lclExtrapolate = lclExtrapolate,
       uclExtrapolate = uclExtrapolate
       )
  
}