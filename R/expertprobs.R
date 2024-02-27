expertprobs <-
function(fit, x, d = "best", ex = 1){
	
  if(d == "best"){
    d <- fit$best.fitting[ex, 1]
  }

	
		
	if(d == "normal"){
		px <- pnorm(x, fit$Normal[ex,1], fit$Normal[ex,2]) 		
	}
  
  if(d == "skewnormal"){
    px <- sn::psn(x, xi = fit$Skewnormal[ex,1],
                  omega = fit$Skewnormal[ex,2],
                  alpha = fit$Skewnormal[ex,3]) 		
  }
	
	if(d == "t"){
		px <- pt((x - fit$Student.t[ex,1])/fit$Student.t[ex,2], fit$Student.t[ex,3])	
	}
	
	if(d == "gamma"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		px <- pgamma(x - xl, fit$Gamma[ex,1], fit$Gamma[ex,2])  
	}
	
	if(d == "mirrorgamma"){
	  xu <- fit$limits[ex, 2]
	  px <- 1 - pgamma(xu - x, fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])  
	}
	
	if(d == "lognormal"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		px <- plnorm(x - xl, fit$Log.normal[ex,1], fit$Log.normal[ex,2]) 
	}
	
	if(d == "mirrorlognormal"){
	  xu <- fit$limits[ex, 2]
	  px <- 1 - plnorm(xu - x, fit$mirrorlognormal[ex,1], fit$mirrorlognormal[ex,2]) 
	}	
	
	if(d == "logt"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		# Avoid NaN
		px <- pt( (log(abs(x - xl)) - fit$Log.Student.t[ex,1]) 
		          / fit$Log.Student.t[ex,2], fit$Log.Student.t[ex,3])
		px[x <= xl] <- 0 # Set to 0 for x < lower limit
	}
	
	if(d == "mirrorlogt"){
	  xu <- fit$limits[ex, 2]
	  # Avoid NaN
	  px <- 1 - pt( (log(abs(xu - x)) - fit$mirrorlogt[ex,1]) 
	            / fit$mirrorlogt[ex,2], fit$mirrorlogt[ex,3])
	  px[x >= xu] <- 1 # Set to 1 for x > upper limit
	}
		
	if(d == "beta"){
		xl <- fit$limits[ex,1]
		xu <- fit$limits[ex,2]
		if(xl == -Inf){xl <- 0}
		if(xu == Inf){xu <- 1}
		px <-  pbeta( (x - xl) / (xu - xl), fit$Beta[ex,1], fit$Beta[ex,2])
	}
	
	if(d == "hist"){
	  px <- approx(c(fit$limits[ex, 1],
	                 fit$vals[ex, ],
	                 fit$limits[ex, 2]),
	               c(0, fit$probs[ex, ], 1),
	               xout = x,
	               yleft = 0, 
	               yright = 1)$y
	}
  
px	
	
}
