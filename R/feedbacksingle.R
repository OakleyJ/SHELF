feedbacksingle <-
function(fit, quantiles =  NA, values = NA, sf = 3, ex = 1){
	
	n.distributions <- 7
	distribution.names <- c("Normal", "Student-t", "Gamma", "Log normal",
	                        "Log Student-t", "Beta", "Histogram")
	d.index<-c(1:2)
	
	if(is.na(quantiles[1]) == F ){
		report.elicited.q <- F
	}else{
		quantiles <- fit$probs[ex,]		
		report.elicited.q <- T	
	}
	
	Mq <- matrix(0, length(quantiles), n.distributions) 		
	Mq[,1] <- qnorm(quantiles, fit$Normal[ex,1], fit$Normal[ex,2])
	Mq[,2] <- qt(quantiles, fit$Student.t[ex,3]) * fit$Student.t[ex,2] + fit$Student.t[ex,1] 
	if(fit$limits[ex,1] > - Inf){
		d.index<-c(1:5)
		Mq[,3] <- fit$limits[ex,1] + qgamma(quantiles, fit$Gamma[ex,1], fit$Gamma[ex,2])
		Mq[,4] <- fit$limits[ex,1] + qlnorm(quantiles, fit$Log.normal[ex,1], fit$Log.normal[ex,2])
		Mq[,5] <- fit$limits[ex,1] + exp( qt(quantiles, fit$Log.Student.t[ex,3]) * fit$Log.Student.t[ex,2] + fit$Log.Student.t[ex,1])
		if(fit$limits[ex,2] < Inf){
			d.index<-c(1:7)
			Mq[,6] <- fit$limits[ex,1] + 
			  (fit$limits[ex,2] - fit$limits[ex,1]) * qbeta(quantiles, fit$Beta[ex,1], fit$Beta[ex,2] )
			Mq[, 7] <- qhist(quantiles,
			                 c(fit$limits[ex, "lower"], fit$vals[ex, ], fit$limits[ex, "upper"]),
			                 c(0, fit$probs[ex, ], 1 )
			                 )
		}
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
	
			
	valuesMatrix[,2] <- (valuesMatrix[,2] - fit$Student.t[ex,1]) / fit$Student.t[ex,2]
		
	if(fit$limits[ex,1] > - Inf){
		valuesMatrix[,3:4] <- valuesMatrix[,3:4] - fit$limits[ex,1]
		valuesMatrix[,5] <- (log(abs(valuesMatrix[,5] - fit$limits[ex,1])) - 
		                 fit$Log.Student.t[ex,1]) / fit$Log.Student.t[ex,2]
		# avoid log of negative values. Set probability to 0 in line 66
	}
		
	if((fit$limits[ex,1] > - Inf) & (fit$limits[ex,2] < Inf)){
		valuesMatrix[,6] <- (valuesMatrix[,6] - fit$limits[ex,1]) / (fit$limits[ex,2] - fit$limits[ex,1])
	}
		
	Mp <- matrix(0, nrow(valuesMatrix), ncol(valuesMatrix))
		
	Mp[,1] <- pnorm(valuesMatrix[,1], fit$Normal[ex,1], fit$Normal[ex,2])
	Mp[,2] <- pt(valuesMatrix[,2], fit$Student.t[ex,3])
	if(fit$limits[ex,1] > - Inf){
		Mp[,3] <- pgamma(valuesMatrix[,3], fit$Gamma[ex,1], fit$Gamma[ex,2])
		Mp[,4] <- plnorm(valuesMatrix[,4], fit$Log.normal[ex,1], fit$Log.normal[ex,2])
		Mp[,5] <- pt(valuesMatrix[,5], fit$Log.Student.t[ex,3])
	
		# set to 0 for log-T, if x is below lower limit
		Mp[values <= fit$limits[ex, 1], 5] <- 0  
		
		if(fit$limits[ex,2] <  Inf){
			Mp[,6] <- pbeta(valuesMatrix[,6], fit$Beta[ex,1], fit$Beta[ex,2])
			Mp[, 7] <- phist(valuesMatrix[, 1],
			                 c(fit$limits[ex, "lower"], fit$vals[ex, ], fit$limits[ex, "upper"]),
			                 c(0, fit$probs[ex, ], 1 ))
		}
	}	
		
	if(report.elicited.p == F){
		Mp <- data.frame(Mp, row.names = values)
		names(Mp) <- distribution.names
    dp.index<-d.index }else{
		Mp <- data.frame(matrix(fit$probs[ex,], ncol=1), Mp, row.names = values)
		names(Mp) <- c("elicited", distribution.names)
    dp.index<-c(1,d.index+1)
	}
	
	if(report.elicited.q == F){
		Mq <- data.frame(Mq, row.names = quantiles)
		names(Mq) <- c(distribution.names)
		dq.index<-d.index }else{
		Mq <- data.frame(fit$vals[ex,], Mq, row.names = quantiles)
		names(Mq) <- c("elicited", distribution.names)
		dq.index<-c(1,d.index+1)
	}
	


	list(fitted.quantiles = signif(Mq[, dq.index],
	                               sf), 
	     fitted.probabilities = signif(Mp[, dp.index],
	                                   sf))
}
