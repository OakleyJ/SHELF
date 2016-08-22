makeGroupPlot <-
function(fit, pl, pu, d = "best", lwd){
	
  expert <- NULL # hack to avoid R CMD check NOTE
  
	n.experts <- nrow(fit$vals)
	
	x <- matrix(0, 200 * n.experts, 1)
	fx <- x
	
	
	for(i in 1:n.experts){
		densitydata <- expertdensity(fit, d, ex = i, pl, pu)
		x[(1+(i-1)*200):(i*200), 1] <- densitydata$x
		fx[(1+(i-1)*200):(i*200), 1] <-densitydata$fx
	}
	df1 <- data.frame(x = x, fx = fx, 
	                  expert = rep(LETTERS[c(1:n.experts)], each =200))
	p1 <- ggplot(df1, aes(x = x, y = fx, colour = expert)) + geom_line(size=lwd) +
	  labs(x = "x", y = expression(f[X](x)))
	
	
	p1
}
