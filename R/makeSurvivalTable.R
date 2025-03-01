
#' Tabulate Summary Data for Survival Extrapolation
#'
#' Tabulates the Kaplan Meier survivor function and within interval hazard at discrete equally spaced time points t_1,...,t_n
#' "Within interval hazard" is defined as (1-S(t_[n+1])) / S_(t_n), using the Kaplan Meier estimate of S().
#' The table is intended to be included on a summary sheet provided to experts when eliciting judgements about
#' extrapolated survival probabilities.
#' 
#' @param survDf data frame with individual patient data. Needs three columns with names
#' "time", "event" and "treatment" (in that order). For weighted observations (e.g. 
#' using propensity scores), include a fourth column "weights".
#' Values in the "event" column should be 0 for a censored observation, and 1 otherwise.
#' The "treatment" column should be included even if there is only one treatment group.
#' @param breakTime duration of each time interval
#' @param truncationTime time point for the end of the last interval 
#' @param timeUnit string variable to give unit of time
#' @param dp number of decimal places to display
#' @param useWeights set to TRUE if survDf includes column of weights, as described in specification 
#' of survDf. This column is passed on to survival::survfit() as the case weights.
#'
#' @returns a data frame with survivor function estimates, 95% confidence intervals,
#' and within interval hazard estimates for each time interval.
#' 
#' @import survival
#' @export
#'
#' @examples
#' \dontrun{
#' sdf <- survival::veteran[, c("time", "status", "trt")]
#' colnames(sdf) <- c("time", "event", "treatment")
#' sdf$treatment <- factor(sdf$treatment, labels = c("standard", "test"))
#' makeSurvivalTable(sdf, breakTime = 50, truncationTime = 250, timeUnit = "months")
#' }
makeSurvivalTable <- function(survDf, breakTime, truncationTime, timeUnit, useWeights = FALSE, dp = 2){
  
  survDf$treatment <- as.factor(survDf$treatment)
  
  if(useWeights == TRUE){
    sv <- survival::survfit(survival::Surv(time, event) ~ treatment, weights  = weights,
                            data = survDf)
  }else{
    sv <- survival::survfit(survival::Surv(time, event) ~ treatment,
                            data = survDf)
  }
  
 
  truncationTime <- min(truncationTime, min(tapply(survDf$time, survDf$treatment, max)))
  sTimes <- seq(from = breakTime, to = truncationTime, by = breakTime)
  nTimes <- length(sTimes)
  nTreatments <- length(levels(survDf$treatment))
  tNames <- levels(survDf$treatment)
  
  pt <- matrix(round(summary(sv, times = sTimes)$surv , dp),
               nrow = nTimes, ncol = nTreatments)
  wt  <- pt
  wt[1, ] <- 1 - wt[1, ]
  if(nrow(wt) > 1){
    wt[-1, ] <- 1 - round(pt[2:nTimes, ]/pt[1:(nTimes - 1), ], dp)
  }
  
  ciLower <- matrix(round(summary(sv, times = sTimes)$lower , dp),
                    nrow = nTimes, ncol = nTreatments)
  ciUpper <- matrix(round(summary(sv, times = sTimes)$upper , dp),
                    nrow = nTimes, ncol = nTreatments)
  ci95 <-  matrix (paste0("(", ciLower, ", ", ciUpper,")"),
                   nrow = nTimes, ncol = nTreatments)
  sTable <- data.frame(paste0("[", c(0, sTimes[1:(nTimes -1)]), ",", sTimes, ")"),
                       pt[, 1], ci95[, 1],  wt[, 1])
  colnames(sTable) <- c(paste0("time interval (", timeUnit,")"),
                        paste0("survivor (", tNames[1], ")"),
                        paste0("survivor 95% CI (", tNames[1], ")"),
                        paste0("hazard (", tNames[1], ")"))
  
  if(nTreatments > 1){
    for(i in 2:nTreatments){
      dfTemp <-  data.frame(pt[, i], ci95[, i],  wt[, i])
      colnames(dfTemp) <- c(paste0("survivor (", tNames[i], ")"),
                            paste0("survivor 95% CI (", tNames[i], ")"),
                            paste0("hazard (", tNames[i], ")"))
      sTable <- cbind(sTable, dfTemp)
    }
  }
  
  sTable
}