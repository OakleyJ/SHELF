feedbackgroup <-
function(fit, quantiles =  NA, values = NA, dist = "best", sfg = 3){
	
	
	n.experts <- nrow(fit$limits)
	expertnames <- paste("expert.", LETTERS[1:n.experts], sep="")
	
	distributions <- data.frame(matrix(0, nrow = 1, ncol = n.experts))
	names(distributions) <- expertnames
	distribution.names <- c("normal", "t", "gamma", "lognormal",
	                        "logt", "beta",
	                        "hist", "mirrorgamma", "mirrorlognormal",
	                        "mirrorlogt")
	

	
	if(is.na(quantiles[1]) == T ){
		quantiles <- fit$probs[1,]		
	}
	expert.quantiles <- data.frame(matrix(0, nrow = length(quantiles),
	                                      ncol = n.experts), 
	                               row.names = quantiles)
	names(expert.quantiles) <- expertnames
	
	if(is.na(values[1]) == T ){
		values <- fit$vals[1,]
	}
	expert.probs <- data.frame(matrix(0, nrow = length(values),
	                                  ncol = n.experts), 
	                           row.names = values)
	names(expert.probs) <- expertnames
	
	for(i in 1:n.experts){
		d.index<- c(1:2)
		if(fit$limits[i,1]>-Inf){
			d.index <- c(1:5)
			if(fit$limits[i,2]<Inf){
			d.index <- c(1:6)
			}
		}
		
		if(dist == "best"){
		  expertDist <- fit$best.fitting[i, 1]
		}else{
		  expertDist <- dist
		}
		
		
		distributions[1, i] <- expertDist
		
		temp <- feedbacksingle(fit, quantiles, values, ex = i)
		expert.quantiles[, i] <- temp$fitted.quantiles[, expertDist]
		expert.probs[, i] <- temp$fitted.probabilities[, expertDist]
	}
	
	
	list(expert.quantiles = signif(expert.quantiles, sfg), expert.probs = signif(expert.probs, sfg), distributions = distributions)
}
