expertdensity <-
function(fit, d = "best", ex = 1, pl, pu, ql = NULL, qu = NULL, nx = 200){
	
  if(pl == -Inf){pl <- qnorm(0.001, fit$Normal[ex,1], fit$Normal[ex,2])}
  if(pu == Inf){pu <- qnorm(0.999, fit$Normal[ex,1], fit$Normal[ex,2])}
  
	x <- unique(sort(c(seq(from = pl, to = pu, length = nx), ql, qu)))
	

	if(d == "best"){
	  d <- fit$best.fitting[ex, 1]
	}

	if(d == "normal"){
		fx <- dnorm(x, fit$Normal[ex,1], fit$Normal[ex,2]) 
	}
	
	if(d == "t"){
		fx <- dt((x - fit$Student.t[ex,1])/fit$Student.t[ex,2], fit$Student.t[ex,3])/fit$Student.t[ex,2]
	}
	
	if(d == "skewnormal"){
	  fx <- sn::dsn(x, fit$Skewnormal[ex, 1],
	                fit$Skewnormal[ex, 2],
	                fit$Skewnormal[ex, 3])
	}
	
	if(d == "gamma"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		fx <- dgamma(x - xl, fit$Gamma[ex,1], fit$Gamma[ex,2])  
	}
	
	if(d == "mirrorgamma"){
	  xu <- fit$limits[ex, 2]
	  fx <- dgamma(xu - x, fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])  
	}
	
	if(d == "lognormal"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		fx <- dlnorm(x - xl, fit$Log.normal[ex,1], fit$Log.normal[ex,2]) 
	}	
	
	if(d == "mirrorlognormal"){
	  xu <- fit$limits[ex, 2]
	  fx <- dlnorm(xu - x, fit$mirrorlognormal[ex,1], fit$mirrorlognormal[ex,2]) 
	}	
	
	if(d == "logt"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		fx <- dt( (log(abs(x - xl)) - fit$Log.Student.t[ex,1]) / fit$Log.Student.t[ex,2], fit$Log.Student.t[ex,3]) / ((x - xl) * fit$Log.Student.t[ex,2])
    fx[x<= xl] <- 0 # Hack to avoid NaN
    
	}
	
	if(d == "mirrorlogt"){
	  xu <- fit$limits[ex,2]
	  fx <- dt( (log(abs(xu - x)) - fit$mirrorlogt[ex,1]) /
	              fit$mirrorlogt[ex,2], fit$mirrorlogt[ex,3]) / ((xu - x) * fit$mirrorlogt[ex,2])
	  fx[x>= xu] <- 0 # Hack to avoid NaN
	  
	}
	
	
		
	if(d == "beta"){
		xl <- fit$limits[ex,1]
		xu <- fit$limits[ex,2]
		if(xl == -Inf){xl <- 0}
		if(xu == Inf){xu <- 1}
		fx <-  1/(xu - xl) * dbeta( (x - xl) / (xu - xl), fit$Beta[ex,1], fit$Beta[ex,2])
	}

	if(d == "hist"){
	 
	  fx <- dhist(x, c(fit$limits[ex, 1],
	                   fit$vals[ex,],
	                   fit$limits[ex, 2]),
	              c(0, fit$probs[ex, ],1))
	  fx[length(fx)] <- 0
	  }

 
list(x = x, fx = fx)	
	
}
