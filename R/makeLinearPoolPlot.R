makeLinearPoolPlot <-
function(fit, xl, xu, d = "best", w = 1, lwd, xlab, ylab, legend_full = TRUE){
	
  expert <- ftype <- NULL # hack to avoid R CMD check NOTE
  
	n.experts <- nrow(fit$vals)
	
	if(n.experts < 27){
	  expertnames <- LETTERS[1:n.experts]
	}
	
	if(n.experts > 26){
	  expertnames <- 1:n.experts
	}
	
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
	                  expert = c(rep(expertnames, each =200),
	                             rep("linear pool", 200)),
	                  ftype = c(rep("individual", 200 * n.experts), 
	                           rep("linear pool", 200)))
	df1$expert <- factor(df1$expert, levels = c(expertnames, "linear pool"))
	
	if(legend_full){
	  p1 <- ggplot(df1, aes(x = x, y = fx, 
	                        colour = expert, 
	                        linetype = expert, 
	                        size = expert)) +
	    scale_linetype_manual(values = c(rep("dashed", n.experts), "solid")) +
	    scale_size_manual(values = lwd * c(rep(0.5, n.experts), 1.5))}else{
	      p1 <- ggplot(df1, aes(x = x, y = fx, 
	                            colour =  ftype, 
	                            linetype=ftype, size =ftype)) +
	        scale_linetype_manual(name = "distribution", values = c("dashed", "solid"))+
	        scale_size_manual(name = "distribution", values = lwd * c(.5, 1.5)) +
	        scale_color_manual(name = "distribution", values = c("black", "red"))
	    }
	p1 <- p1 + geom_line(aes(group = expert)) + 
	  labs(x = xlab, y = ylab)
	    
	p1
}
