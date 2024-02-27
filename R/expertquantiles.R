expertquantiles <-
function(fit, q, d = "best", ex = 1){
	
  if(d == "best"){
    d <- fit$best.fitting[ex, 1]
  }

	if(d == "normal"){
		qx <- qnorm(q, fit$Normal[ex,1], fit$Normal[ex,2]) 		
	}
	
  if(d == "skewnormal"){
    qx <- sn::qsn(q, xi = fit$Skewnormal[ex,1],
                  omega = fit$Skewnormal[ex,2],
                  alpha = fit$Skewnormal[ex,3]) 		
  }
  
	if(d == "t"){
		qx <- fit$Student.t[ex,1] + fit$Student.t[ex,2] * qt(q, fit$Student.t[ex,3])	
	}
	
	if(d == "gamma"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		qx <- xl + qgamma(q, fit$Gamma[ex,1], fit$Gamma[ex,2])  
	}
	
	if(d == "mirrorgamma"){
	  xu <- fit$limits[ex, 2]
	  qx <- xu - qgamma(1 - q, fit$mirrorgamma[ex,1],
	                    fit$mirrorgamma[ex,2])  
	}
	
	if(d == "lognormal"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		qx <- xl + qlnorm(q, fit$Log.normal[ex,1], fit$Log.normal[ex,2]) 
	}
	
	if(d == "mirrorlognormal"){
	  xu <- fit$limits[ex, 2]
	  qx <- xu - qlnorm(1 - q, fit$mirrorlognormal[ex,1],
	                    fit$mirrorlognormal[ex,2]) 
	}	
	
	if(d == "logt"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		qx <- xl + exp(fit$Log.Student.t[ex,1] + fit$Log.Student.t[ex,2] * qt( q , fit$Log.Student.t[ex,3])) 
	}
	
	if(d == "mirrorlogt"){
	  xu <- fit$limits[ex, 2]
	  qx <- xu - exp(fit$mirrorlogt[ex,1] + fit$mirrorlogt[ex,2] * qt(1- q , fit$mirrorlogt[ex,3])) 
	}
		
	if(d == "beta"){
		xl <- fit$limits[ex,1]
		xu <- fit$limits[ex,2]
		if(xl == -Inf){xl <- 0}
		if(xu == Inf){xu <- 1}
		qx <- xl + (xu - xl) * qbeta(q, fit$Beta[ex,1], fit$Beta[ex,2])
	}
	
	if(d == "hist"){
	  qx <- approx(c(0, fit$probs[ex, ], 1),
	               c(fit$limits[ex, 1],
	                 fit$vals[ex, ],
	                 fit$limits[ex, 2]),
	               xout = q)$y
	}
	
qx	
	
}
