feedbacksingle <-
function(fit, quantiles =  NA, values = NA, sf = 3, ex = 1){
	
	n.distributions <- 11
	distribution.names <- c("normal", "t", "skewnormal", "gamma", "lognormal",
	                        "logt", "beta", "hist",
	                        "mirrorgamma", "mirrorlognormal",
	                        "mirrorlogt")
	
	# Fitted quantiles ----
	
	if(is.na(quantiles[1]) == F ){
		report.elicited.q <- F
	}else{
		quantiles <- fit$probs[ex,]		
		report.elicited.q <- T	
	}
	
	Mq <- matrix(NA, length(quantiles), n.distributions) 
	
	colnames(Mq) <- distribution.names
	if(!is.na(fit$ssq[ex, "normal"])){
	  Mq[, "normal"] <- qnorm(quantiles, fit$Normal[ex,1], fit$Normal[ex,2])
	}
	if(!is.na(fit$ssq[ex, "t"])){
	  Mq[, "t"] <- qt(quantiles, fit$Student.t[ex,3]) * fit$Student.t[ex,2] +
	  fit$Student.t[ex,1]
	}
	if(!is.na(fit$ssq[ex, "skewnormal"])){
	Mq[, "skewnormal"] <- sn::qsn(quantiles, xi = fit$Skewnormal[ex,1],
	                              omega = fit$Skewnormal[ex,2],
	                              alpha = fit$Skewnormal[ex,3])
	}
	

	  if(!is.na(fit$ssq[ex, "gamma"])){
	  Mq[, "gamma"] <- fit$limits[ex,1] + 
		  qgamma(quantiles, fit$Gamma[ex,1], fit$Gamma[ex,2])
	  }
	  if(!is.na(fit$ssq[ex, "lognormal"])){
		Mq[, "lognormal"] <- fit$limits[ex,1] + 
		  qlnorm(quantiles, fit$Log.normal[ex,1], fit$Log.normal[ex,2])
	  }
	  if(!is.na(fit$ssq[ex, "logt"])){
		Mq[, "logt"] <- fit$limits[ex,1] +
		  exp( qt(quantiles, fit$Log.Student.t[ex,3]) * fit$Log.Student.t[ex,2] +
		         fit$Log.Student.t[ex, 1])}
	  
	if(!is.na(fit$ssq[ex, "beta"])){
			Mq[, "beta"] <- fit$limits[ex,1] + 
			  (fit$limits[ex,2] - fit$limits[ex,1]) * 
			  qbeta(quantiles, fit$Beta[ex,1], fit$Beta[ex,2] )
	}
			if(fit$limits[ex,1] > - Inf & fit$limits[ex,2] < Inf) {
			Mq[, "hist"] <- qhist(quantiles,
			                 c(fit$limits[ex, "lower"], fit$vals[ex, ], 
			                   fit$limits[ex, "upper"]),
			                 c(0, fit$probs[ex, ], 1 )
			                 )
		}
	
	if(!is.na(fit$ssq[ex, "mirrorgamma"])){
	  Mq[, "mirrorgamma"] <- fit$limits[ex,2] - 
	    qgamma(1 - quantiles, fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])
	}
	if(!is.na(fit$ssq[ex, "mirrorlognormal"])){
	  Mq[, "mirrorlognormal"] <- fit$limits[ex,2] - 
	    qlnorm(1 - quantiles, fit$mirrorlognormal[ex,1],
	           fit$mirrorlognormal[ex,2])
	}
	if(!is.na(fit$ssq[ex, "mirrorlogt"])){
	  Mq[, "mirrorlogt"] <- fit$limits[ex,2] -
	    exp( qt(1 - quantiles, fit$mirrorlogt[ex,3]) * fit$mirrorlogt[ex,2] +
	           fit$mirrorlogt[ex, 1])
	}
	
	# Fitted probabilities ----
	if(is.na(values[1]) == F ){
		valuesMatrix <- matrix(values,
		                       nrow = length(values),
		                       ncol = n.distributions)
		report.elicited.p <- F
		}else{
		  valuesMatrix <- matrix(fit$vals[ex,],
		                         nrow = length(fit$vals[ex,]),
		                         ncol = n.distributions)
		  values <- fit$vals[ex, ]
		report.elicited.p <- T
		}
	
	colnames(valuesMatrix) <- distribution.names
	
	Mp <- matrix(NA, nrow(valuesMatrix), ncol(valuesMatrix))
	colnames(Mp) <- distribution.names
	
	if(!is.na(fit$ssq[ex, "t"])){		
	valuesMatrix[, "t"] <- (valuesMatrix[, "t"] - fit$Student.t[ex,1]) / 
	  fit$Student.t[ex,2]
	
	Mp[,"t"] <- pt(valuesMatrix[, "t"], fit$Student.t[ex,3])
	}
	
	if(!is.na(fit$ssq[ex, "normal"])){
	  Mp[, "normal"] <- pnorm(valuesMatrix[, "normal"], 
	                          fit$Normal[ex,1], 
	                          fit$Normal[ex,2])

	}
	
	if(!is.na(fit$ssq[ex, "skewnormal"])){
	  Mp[, "skewnormal"] <- sn::psn(valuesMatrix[, "skewnormal"], 
	                                xi = fit$Skewnormal[ex,1],
	                                omega = fit$Skewnormal[ex,2],
	                                alpha = fit$Skewnormal[ex,3])
	}
	
	if(!is.na(fit$ssq[ex, "gamma"])){
	  valuesMatrix[, "gamma"] <- 
	    valuesMatrix[, "gamma"] - fit$limits[ex,1]
	  Mp[, "gamma"] <- pgamma(valuesMatrix[, "gamma"],
	                          fit$Gamma[ex,1], fit$Gamma[ex,2])
	}
	
	if(!is.na(fit$ssq[ex, "lognormal"])){
	  valuesMatrix[, "lognormal"] <- 
	    valuesMatrix[, "lognormal"] - fit$limits[ex,1]
	  Mp[, "lognormal"] <- plnorm(valuesMatrix[, "lognormal"], 
	                              fit$Log.normal[ex,1], fit$Log.normal[ex,2])
	}
	
	if(!is.na(fit$ssq[ex, "logt"])){
	  valuesMatrix[, "logt"] <- (log(abs(valuesMatrix[, "logt"] - fit$limits[ex,1])) - 
	                               fit$Log.Student.t[ex,1]) / fit$Log.Student.t[ex,2]
	  # avoid log of negative values. Set probability to 0 if X below lower limit
	  Mp[, "logt"] <- pt(valuesMatrix[, "logt"], fit$Log.Student.t[ex,3])
	  Mp[values <= fit$limits[ex, 1],  "logt"] <- 0  
	}
	
	if(!is.na(fit$ssq[ex, "beta"])){
	  valuesMatrix[, "beta"] <- (valuesMatrix[, "beta"] - fit$limits[ex,1]) / 
	    (fit$limits[ex,2] - fit$limits[ex,1])
	  Mp[, "beta"] <- pbeta(valuesMatrix[, "beta"], fit$Beta[ex,1], fit$Beta[ex,2])
	}
	
	if(!is.na(fit$ssq[ex, "mirrorgamma"])){
	  valuesMatrix[, "mirrorgamma"] <- fit$limits[ex,2] -
	    valuesMatrix[, "mirrorgamma"]
	  Mp[, "mirrorgamma"] <- 1 - pgamma(valuesMatrix[, "mirrorgamma"],
	                                    fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])
	}
	
	if(!is.na(fit$ssq[ex, "mirrorlognormal"])){
	  valuesMatrix[, "mirrorlognormal"] <- fit$limits[ex,2] -
	    valuesMatrix[, "mirrorlognormal"]
	  Mp[, "mirrorlognormal"] <- 1 - plnorm(valuesMatrix[, "mirrorlognormal"], 
	                                        fit$mirrorlognormal[ex,1], fit$mirrorlognormal[ex,2])
	}
	
	if(!is.na(fit$ssq[ex, "mirrorlogt"])){
	  valuesMatrix[, "mirrorlogt"] <- (log(abs(fit$limits[ex,2] - valuesMatrix[, "mirrorlogt"])) - 
	                                     fit$mirrorlogt[ex,1]) / fit$mirrorlogt[ex,2]
	  Mp[, "mirrorlogt"] <- 1 - pt(valuesMatrix[, "mirrorlogt"],
	                               fit$mirrorlogt[ex,3])
	  
	  # set to 0 for log-T, if x is above upper limit
	  Mp[values >= fit$limits[ex, 2],  "mirrorlogt"] <- 0 
	}
	
	if(fit$limits[ex,1] > - Inf & fit$limits[ex,2] <  Inf){
	  
	    Mp[, "hist"] <- phist(valuesMatrix[, "hist"],
	                          c(fit$limits[ex, "lower"], fit$vals[ex, ], fit$limits[ex, "upper"]),
	                          c(0, fit$probs[ex, ], 1 ))
	  }
	
	

	
		
	if(report.elicited.p == F){
		Mp <- data.frame(Mp, row.names = values)}else{
		Mp <- data.frame(matrix(fit$probs[ex,], ncol=1), Mp, row.names = values)
		names(Mp) <- c("elicited", distribution.names)
	}
	
	if(report.elicited.q == F){
		Mq <- data.frame(Mq, row.names = quantiles)
		}else{
		Mq <- data.frame(fit$vals[ex,], Mq, row.names = quantiles)
		names(Mq) <- c("elicited", distribution.names)
	}
	

	list(fitted.quantiles = signif(Mq, sf), 
	     fitted.probabilities = signif(Mp, sf))
}
