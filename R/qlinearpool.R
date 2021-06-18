#' @export
#' 
qlinearpool <-
function(fit, q, d = "best", w = 1){
	
	n.experts <- nrow(fit$vals)
	
	if(length(d) == 1){
	  d <- rep(d, n.experts)
	}
	
	qx.individual <- matrix(0, length(q), n.experts)
	
	for(i in 1:n.experts){
		qx.individual[,i] <- expertquantiles(fit, q, d[i], ex = i)
	}
	
	n.q <- length(q)
	qx<-rep(0, n.q)
	
	for(i in 1:n.q){
		x <- seq(from = min(qx.individual[i,]) - 
		           abs(0.001 * min(qx.individual[i,])),
		         to = max(qx.individual[i,]) +
		           abs(0.001 * max(qx.individual[i,])), length = 10000)
		px <- plinearpool(fit, x, d, w)
		qx[i] <- approx(x = px, y = x, xout = q[i], ties = min)$y 
	}
qx			
}
