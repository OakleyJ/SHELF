
#' @export
rlinearpool <-
function(fit, n, d = "best", w = 1){
	
  if(min(w)<0 | max(w)<=0){stop("expert weights must be non-negative, and at least one weight must be greater than 0.")}
  
	n.experts <- nrow(fit$vals)
	
	if(length(d) == 1){
	  d <- rep(d, n.experts)
	}
	
  if(length(w) == 1){
    w <- rep(w, n.experts)
  }
  
	x <- rep(0, n * n.experts)
	
	# Sample n values per expert, then make a stack of the n samples
	
	for(i in 1:n.experts){
		xExpert <- sampleFit(fit, n, expert = i)
		if(d[i] == "best"){
		  d[i] <- fit$best.fitting[i, "best.fit"]
		}
		x[(1 + (i - 1) * n):(i * n)] <- xExpert[, d[i]]
	}
	
	# for i=1,...,n, sample which expert's sample of values 
	# provides the i-th value in the linear pool sample
	
	index <- n * sample(0:(n.experts-1), size = n,
	                replace = TRUE,
	                prob = w / sum(w))
	x[index + 1:n]
	
}
