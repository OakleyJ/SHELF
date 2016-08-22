makeLinearPoolPlot <-
function(fit, xl, xu, d = "best", w = 1){
	
  expert <- NULL # hack to avoid R CMD check NOTE
  
	n.experts <- nrow(fit$vals)
	x <- matrix(0, 200, n.experts)
	fx <- x
  if(min(w)<0 | max(w)<=0){stop("expert weights must be non-negative, and at least one weight must be greater than 0.")}
  
	if(length(w)==1){
	  w <- rep(w, n.experts)
	}
  
	weight <- matrix(w/sum(w), 200, n.experts, byrow = T)
 
	for(i in 1:n.experts){
		densitydata <- expertdensity(fit, d, ex = i, xl, xu)
		x[, i] <- densitydata$x
		fx[, i] <-densitydata$fx 
	}
	
	fx.lp <- apply(fx * weight, 1, sum)
	
	df1 <- data.frame(x = rep(x[, 1], n.experts + 1),
	                  fx = c(as.numeric(fx), fx.lp),
	                  expert = c(rep(LETTERS[c(1:n.experts)], each =200),
	                             rep("linear pool", 200)))
	p1 <- ggplot(df1, aes(x = x, y = fx, colour =  expert, linetype=expert, size =expert)) + 
	  geom_line() + 
	  labs(x="x", y=expression(f[X](x))) +
	  scale_linetype_manual(values = c(rep("dashed", n.experts), "solid"))+
	  scale_size_manual(values=c(rep(1, n.experts), 1.5))
	
	p1
}
