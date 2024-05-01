checkJudgementsValid <- function(probs, vals, tdf, lower, upper, silent = TRUE){
  valid <- TRUE
  error <- NULL
  
    if (length(probs) < 1){
      valid <- FALSE
      error <- "need at least one elicited probability"
      }
    if (min(probs) < 0 | max(probs) > 1 ){
      valid <- FALSE
      error <- "probabilities must be between 0 and 1"
      }
    if (min(vals) < lower){
      valid <- FALSE
      error <- "elicited parameter values cannot be smaller than lower parameter limit"
      }
    if (max(vals) > upper){
      valid <- FALSE
      error <- "elicited parameter values cannot be greater than upper parameter limit"}
    if (tdf <= 0 ){
      valid <- FALSE
      error <- "Student-t degrees of freedom must be greater than 0"
      }
    if (any(diff(probs) <= 0)){
      valid <- FALSE
      error <- "probabilities must be specified in ascending order"
    }
  if (any(diff(vals) <= 0)){
    valid <- FALSE
    error <- "values must be specified in ascending order"
  }
  if (length(probs) != length(vals)){
    valid <- FALSE
    error <- "number of vals must equal number of probs"
  }
  
 list(valid = valid, error = error)   
  
}