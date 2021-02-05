makeGroupPlot <-
function(fit, pl, pu, d = "best", lwd, xlab, ylab, fs = 12,
         expertnames = NULL){
	
  expert <- NULL # hack to avoid R CMD check NOTE
  
	n.experts <- nrow(fit$vals)
	
	if(is.null(expertnames)){
	
	if(n.experts < 27){
	  expertnames <- LETTERS[1:n.experts]
	}
	
	if(n.experts > 26){
	  expertnames <- factor(1:n.experts)
	}
	
	}
	
	x <- matrix(0, 200 * n.experts, 1)
	fx <- x
	
	
	for(i in 1:n.experts){
		densitydata <- expertdensity(fit, d, ex = i, pl, pu)
		x[(1+(i-1)*200):(i*200), 1] <- densitydata$x
		fx[(1+(i-1)*200):(i*200), 1] <-densitydata$fx
	}
	df1 <- data.frame(x = x, fx = fx, 
	                  expert = factor(rep(expertnames, each =200),
	                                  levels = expertnames))
	p1 <- ggplot(df1, aes(x = x, y = fx, colour = expert))  +
	  labs(x = xlab, y = ylab) +
	  theme(text = element_text(size = fs))
	
	if(d == "hist"){
	  p1 <- p1 + geom_step(size=lwd)
	}else{
	  p1 <- p1 + geom_line(size=lwd)
	}
	
	
	p1
}
