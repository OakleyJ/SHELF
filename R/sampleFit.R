#' Sample from the elicited distributions 
#' 
#' Generates a random sample from all distributions specified
#' within an object of class \code{elicitation}
#' 

#' @param fit An object of class elicitation
#'
#' @param n The required sample size for each elicitation
#' @param expert Specify which expert's distributions to sample 
#' from, if multiple experts' judgements have been elicited.
#' 
#' @return A matrix of sampled values, one column per distribution.
#' Column names are given to label the distributions.
#' 
#' @examples
#' \dontrun{
#' v <- c(20,30,50)
#' p <- c(0.25,0.5,0.75)
#' myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
#' sampleFit(myfit, n = 10)
#' }
#'
#' @export

sampleFit <- function(fit, n, expert = 1){
  x <- matrix(NA, nrow = n, ncol = 11)
  colnames(x) <- c("normal", "t", "skewnormal",
                   "gamma", "lognormal", "logt", "beta", "hist",
                   "mirrorgamma", "mirrorlognormal", "mirrorlogt")
  
  if(all(is.finite(unlist(fit$limits[expert, ])))){
    u <- runif(n)
    x[, "hist"] <- qhist(u, c(fit$limits[expert, 1],
                         fit$vals[expert,],
                         fit$limits[expert, 2]),
                    c(0, fit$probs[expert, ], 1))
    
    
  }
  
  if(!is.na(fit$ssq[expert, "normal"])){
  x[, "normal"] <- rnorm(n, fit$Normal[expert, 1], fit$Normal[expert, 2])
  }
  
  if(!is.na(fit$ssq[expert, "t"])){
  x[, "t"] <- fit$Student.t[expert, 1] +
    fit$Student.t[expert, 2] * rt(n, fit$Student.t[expert, 3])
  }
  
  if(!is.na(fit$ssq[expert, "skewnormal"])){
    x[, "skewnormal"] <- sn::rsn(n, xi = fit$Skewnormal[expert, 1],
                                 omega = fit$Skewnormal[expert, 2],
                                 alpha = fit$Skewnormal[expert, 3])
  }
  
  if(!is.na(fit$ssq[expert, "beta"])){
    x[, "beta"] <- fit$limits[expert, 1] + (fit$limits[expert, 2] - fit$limits[expert, 1]) *
      rbeta(n, fit$Beta[expert, 1], fit$Beta[expert, 2])
  }
  
  if(!is.na(fit$ssq[expert, "lognormal"])){
    x[, "lognormal"] <- fit$limits[expert, 1] + 
      rlnorm(n, fit$Log.normal[expert, 1], fit$Log.normal[expert, 2])
  }
  
  if(!is.na(fit$ssq[expert, "gamma"])){    
    x[, "gamma"] <- fit$limits[expert, 1] +
      rgamma(n, fit$Gamma[expert, 1], fit$Gamma[expert, 2])
  }
  
  if(!is.na(fit$ssq[expert, "logt"])){
    x[, "logt"] <- fit$limits[expert, 1] + 
      exp(fit$Log.Student.t[expert, 1] +
            fit$Log.Student.t[expert, 2] * rt(n, fit$Log.Student.t[expert, 3]))
    }
  
  if(!is.na(fit$ssq[expert, "mirrorlognormal"])){
    x[, "mirrorlognormal"] <- fit$limits[expert, 2] - 
      rlnorm(n, fit$mirrorlognormal[expert, 1],
             fit$mirrorlognormal[expert, 2])
  }
  
  if(!is.na(fit$ssq[expert, "mirrorgamma"])){
    x[, "mirrorgamma"] <- fit$limits[expert, 2] -
      rgamma(n, fit$mirrorgamma[expert, 1], fit$mirrorgamma[expert, 2])
  }
  
  if(!is.na(fit$ssq[expert, "mirrorlogt"])){
    x[, "mirrorlogt"] <- fit$limits[expert, 2] - 
      exp(fit$mirrorlogt[expert, 1] +
            fit$mirrorlogt[expert, 2] * rt(n, fit$mirrorlogt[expert, 3]))
    
    
  }
  
  
  x
  
}