checkJudgementsValid <- function(probs, vals, tdf, lower, upper, silent = TRUE,
                                 excludeExponential = FALSE){
  valid <- TRUE
  error <- NULL
  
  if(any(is.na(probs)) | any(is.na(vals)) ){
    valid <- FALSE
    error <- "missing values in probs/vals"
  }else{
  
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
  if (setequal(unique(probs), c(0, 1))){
    valid <- FALSE
    error <- "Cannot fit with only elicited probabilities of 0 or 1"
  }
  if (excludeExponential == TRUE){
    # only use this check if want to exclude option to fit exponential only
    if(min(probs[probs>0]) > 0.4 | max(probs[probs <1]) < 0.6){
      valid <- FALSE
      error <- "smallest elicited probability must be < 0.4; largest must be > 0.6\n
      (Exclude probabilities equalling 0 or 1)."
    }
    
  }
  }
 list(valid = valid, error = error)   
  
}