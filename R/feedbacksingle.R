feedbacksingle <-
function(fit, quantiles =  NA, values = NA, sf = 3, ex = 1){
	
	n.distributions <- 10
	distribution.names <- c("normal", "t", "gamma", "lognormal",
	                        "logt", "beta", "hist",
	                        "mirrorgamma", "mirrorlognormal",
	                        "mirrorlogt")
	
	if(is.na(quantiles[1]) == F ){
		report.elicited.q <- F
	}else{
		quantiles <- fit$probs[ex,]		
		report.elicited.q <- T	
	}
	
	Mq <- matrix(0, length(quantiles), n.distributions) 
	
	colnames(Mq) <- distribution.names
	
	Mq[, "normal"] <- qnorm(quantiles, fit$Normal[ex,1], fit$Normal[ex,2])
	Mq[, "t"] <- qt(quantiles, fit$Student.t[ex,3]) * fit$Student.t[ex,2] +
	  fit$Student.t[ex,1] 
	if(fit$limits[ex,1] > - Inf){

		Mq[, "gamma"] <- fit$limits[ex,1] + 
		  qgamma(quantiles, fit$Gamma[ex,1], fit$Gamma[ex,2])
		Mq[, "lognormal"] <- fit$limits[ex,1] + 
		  qlnorm(quantiles, fit$Log.normal[ex,1], fit$Log.normal[ex,2])
		Mq[, "logt"] <- fit$limits[ex,1] +
		  exp( qt(quantiles, fit$Log.Student.t[ex,3]) * fit$Log.Student.t[ex,2] +
		         fit$Log.Student.t[ex, 1])
		if(fit$limits[ex,2] < Inf){

			Mq[, "beta"] <- fit$limits[ex,1] + 
			  (fit$limits[ex,2] - fit$limits[ex,1]) * 
			  qbeta(quantiles, fit$Beta[ex,1], fit$Beta[ex,2] )
			Mq[, "hist"] <- qhist(quantiles,
			                 c(fit$limits[ex, "lower"], fit$vals[ex, ], 
			                   fit$limits[ex, "upper"]),
			                 c(0, fit$probs[ex, ], 1 )
			                 )
		}
	}
	if(fit$limits[ex, 2] < Inf){
	  Mq[, "mirrorgamma"] <- fit$limits[ex,2] - 
	    qgamma(1 - quantiles, fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])
	  Mq[, "mirrorlognormal"] <- fit$limits[ex,2] - 
	    qlnorm(1 - quantiles, fit$mirrorlognormal[ex,1],
	           fit$mirrorlognormal[ex,2])
	  Mq[, "mirrorlogt"] <- fit$limits[ex,2] -
	    exp( qt(1 - quantiles, fit$mirrorlogt[ex,3]) * fit$mirrorlogt[ex,2] +
	           fit$mirrorlogt[ex, 1])
	}
		
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
			
	valuesMatrix[, "t"] <- (valuesMatrix[, "t"] - fit$Student.t[ex,1]) / 
	  fit$Student.t[ex,2]
		
	if(fit$limits[ex,1] > - Inf){
		valuesMatrix[, c("gamma", "lognormal")] <- 
		  valuesMatrix[, c("gamma", "lognormal")] - fit$limits[ex,1]
		valuesMatrix[, "logt"] <- (log(abs(valuesMatrix[, "logt"] - fit$limits[ex,1])) - 
		                 fit$Log.Student.t[ex,1]) / fit$Log.Student.t[ex,2]
		# avoid log of negative values. Set probability to 0 in line 91
	}
	
	if(fit$limits[ex,2] < Inf){
	  valuesMatrix[, c("mirrorgamma", "mirrorlognormal")] <- fit$limits[ex,2] -
	    valuesMatrix[, c("mirrorgamma", "mirrorlognormal")]  
	  valuesMatrix[, "mirrorlogt"] <- (log(abs(fit$limits[ex,2] - valuesMatrix[, "mirrorlogt"])) - 
	                               fit$mirrorlogt[ex,1]) / fit$mirrorlogt[ex,2]
	  # avoid log of negative values. Set probability to 0 in line 91
	}
		
	if((fit$limits[ex,1] > - Inf) & (fit$limits[ex,2] < Inf)){
		valuesMatrix[, "beta"] <- (valuesMatrix[, "beta"] - fit$limits[ex,1]) / 
		  (fit$limits[ex,2] - fit$limits[ex,1])
	}
		
	Mp <- matrix(0, nrow(valuesMatrix), ncol(valuesMatrix))
	colnames(Mp) <- distribution.names
		
	Mp[, "normal"] <- pnorm(valuesMatrix[, "normal"], 
	                        fit$Normal[ex,1], 
	                        fit$Normal[ex,2])
	Mp[,"t"] <- pt(valuesMatrix[, "t"], fit$Student.t[ex,3])
	if(fit$limits[ex,1] > - Inf){
		Mp[, "gamma"] <- pgamma(valuesMatrix[, "gamma"],
		                        fit$Gamma[ex,1], fit$Gamma[ex,2])
		Mp[, "lognormal"] <- plnorm(valuesMatrix[, "lognormal"], 
		                            fit$Log.normal[ex,1], fit$Log.normal[ex,2])
		Mp[, "logt"] <- pt(valuesMatrix[, "logt"], fit$Log.Student.t[ex,3])
	
		# set to 0 for log-T, if x is below lower limit
		Mp[values <= fit$limits[ex, 1],  "logt"] <- 0  
		
		if(fit$limits[ex,2] <  Inf){
			Mp[, "beta"] <- pbeta(valuesMatrix[, "beta"], fit$Beta[ex,1], fit$Beta[ex,2])
			Mp[, "hist"] <- phist(valuesMatrix[, "hist"],
			                 c(fit$limits[ex, "lower"], fit$vals[ex, ], fit$limits[ex, "upper"]),
			                 c(0, fit$probs[ex, ], 1 ))
		}
	}
	
	if(fit$limits[ex,2] < Inf){
	  Mp[, "mirrorgamma"] <- 1 - pgamma(valuesMatrix[, "mirrorgamma"],
	                          fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])
	  Mp[, "mirrorlognormal"] <- 1 - plnorm(valuesMatrix[, "mirrorlognormal"], 
	                              fit$mirrorlognormal[ex,1], fit$mirrorlognormal[ex,2])
	  Mp[, "mirrorlogt"] <- 1 - pt(valuesMatrix[, "mirrorlogt"],
	                               fit$mirrorlogt[ex,3])
	  
	  # set to 0 for log-T, if x is below lower limit
	  Mp[values >= fit$limits[ex, 2],  "mirrorlogt"] <- 0  }
	  
	
		
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
	
	Mp <- signif(Mp, sf)
	Mq <- signif(Mq, sf)
	
	if(fit$limits[ex, 1] == -Inf){
	  Mp[, c("gamma", "lognormal", "logt", "beta")] <- NA
	  Mq[, c("gamma", "lognormal", "logt", "beta")] <- NA
	}
	
	if(fit$limits[ex, 2] == Inf){
	  Mp[, c("mirrorgamma", "mirrorlognormal",
	         "mirrorlogt", "beta")] <- NA
	  Mq[, c("mirrorgamma", "mirrorlognormal", "mirrorlogt", "beta")] <- NA
	  
	}
	
	

	list(fitted.quantiles = Mq, 
	     fitted.probabilities = Mp)
}
