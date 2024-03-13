feedbackgroup <-
function(fit, quantiles =  NA, values = NA, dist = "best", sfg ){
	
	
	n.experts <- nrow(fit$limits)
	expertnames <- paste("expert.", LETTERS[1:n.experts], sep="")
	
	distributions <- data.frame(matrix(0, nrow = 1, ncol = n.experts))
	names(distributions) <- expertnames
	
	
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
		
		if(dist == "best"){
		  expertDist <- fit$best.fitting[i, 1]
		}else{
		  expertDist <- dist
		}
		
		
		distributions[1, i] <- expertDist
		
		temp <- feedbacksingle(fit, quantiles, values, ex = i, sf = sfg)
		expert.quantiles[, i] <- temp$fitted.quantiles[, expertDist]
		expert.probs[, i] <- temp$fitted.probabilities[, expertDist]
	}
	
	
	# 18/1/23: have just spotted that outputs are "expert.quantiles" and "expert.probs"
	# if multiple experts, but "fitted.quantiles" and "fitted.probabilities"
	# if a single expert. I need the naming to be consistent - will use the
	# single expert case.
	
	# list(expert.quantiles = signif(expert.quantiles, sfg), expert.probs = signif(expert.probs, sfg), distributions = distributions)
	list(fitted.quantiles = expert.quantiles,
	     fitted.probabilities = expert.probs,
	     distributions = distributions)
	
}
