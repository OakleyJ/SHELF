#' @export

sampleFit <- function(fit, n, expert = 1){
  x <- matrix(NA, nrow = n, ncol = 7)
  colnames(x) <- c("histogram", "normal", "Student-t",
                   "log normal", "gamma", "log Student-t", "beta")
  
  if(all(is.finite(unlist(fit$limits[expert, ])))){
    u <- runif(n)
    x[, 1] <- qhist(u, c(fit$limits[expert, 1],
                         fit$vals[expert,],
                         fit$limits[expert, 2]),
                    c(0, fit$probs[expert, ], 1))
    
    x[, 7] <- fit$limits[expert, 1] + (fit$limits[expert, 2] - fit$limits[expert, 1]) *
      rbeta(n, fit$Beta[expert, 1], fit$Beta[expert, 2])
  }
  
  x[, 2] <- rnorm(n, fit$Normal[expert, 1], fit$Normal[expert, 2])
  
  x[, 3] <- fit$Student.t[expert, 1] +
    fit$Student.t[expert, 2] * rt(n, fit$Student.t[expert, 3])
  
  if(is.finite(fit$limits[expert, 1])){
    x[, 4] <- fit$limits[expert, 1] + 
      rlnorm(n, fit$Log.normal[expert, 1], fit$Log.normal[expert, 2])
      
    x[, 5] <- fit$limits[expert, 1] +
      rgamma(n, fit$Gamma[expert, 1], fit$Gamma[expert, 2])
    
    x[, 6] <- fit$limits[expert, 1] + 
      exp(fit$Log.Student.t[expert, 1] +
            fit$Log.Student.t[expert, 2] * rt(n, fit$Log.Student.t[expert, 3]))
      
        
  }
  x
  
}