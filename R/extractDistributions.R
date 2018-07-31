extractDistributions <- function(fit, d, weight = 1){
  

  
  df <- NULL
  
  if(d == 1){
    df$distribution <- "Normal"
    df$parameters <- fit$Normal
  }
  
  if(d == 2){
    df$distribution <- "Student t"
    df$parameters <- fit$Student.t
  }
  
  if(d == 3){
    df$distribution <- "Gamma"
    df$parameters <- fit$Gamma
  }
  
  if(d == 4){
    df$distribution <- "Log normal"
    df$parameters <- fit$Log.normal
  }
  
  if(d == 5){
    df$distribution <- "Log Student t"
    df$parameters <- fit$Log.Student.t
  }
  
  if(d == 6){
    df$distribution <- "Beta"
    df$parameters <- fit$Beta
  }
  
  if((d != 7) & length(unique(weight)) > 1){
    df$parameters <- cbind(df$parameters, weight = weight)
  }
  
  if(d == 7){
    index <- apply(fit$ssq, 1, which.min)
    df <- NULL
    df$distribution <- fit$best.fitting
    if(is.element(1, index)){
      df$Normal <- fit$Normal[index == 1, ]}else{
       # df$Normal <- NULL
      }
    if(is.element(2, index)){
      df$Student.t <- fit$Student.t[index == 2, ]}else{
       # df$Student.t <- NULL
      }
    if(is.element(3, index)){
      df$Gamma <- fit$Gamma[index == 3, ]}else{
       # df$Gamma <- NULL
      }
    if(is.element(4, index)){
      df$Log.normal <- fit$Log.normal[index == 4, ]}else{
       # df$Log.normal <- NULL
      }
    if(is.element(5, index)){
      df$Log.Student.t <- fit$Log.Student.t[index == 5, ]}else{
      #  df$Log.Student.t <- NULL
      }
    if(is.element(6, index)){
      df$Beta <- fit$Beta[index == 6, ]}else{
      #  df$Beta <- NULL
      }
    #df$ssq <- df$vals <- df$probs <- df$limits <- df$best.fitting <- NULL
    if(length(unique(weight)) > 1){
      df$weight <- cbind(df$distribution, weight = weight)
    }
  }
  #class(df) <- NULL
  
  df
}