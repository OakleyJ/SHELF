makeLinearPoolPlot <-
function(fit, xl, xu, d = "best", w = 1, lwd, xlab, ylab, 
         legend_full = TRUE, ql = NULL, qu = NULL, 
         nx = 200, addquantile = FALSE, fs = 12,
         expertnames = NULL,
         lpname = "linear pool"){
	
  expert <- ftype <- NULL # hack to avoid R CMD check NOTE
  
	n.experts <- nrow(fit$vals)
	
	if(length(d) == 1){
	  d <- rep(d, n.experts)
	}
	
	
	if(is.null(expertnames)){
	  
	  if(n.experts < 27){
	    expertnames <- LETTERS[1:n.experts]
	  }
	  
	  if(n.experts > 26){
	    expertnames <- 1:n.experts
	  }
	  
	}
	  
	nxTotal <- nx + length(c(ql, qu))
	
	x <- matrix(0, nxTotal, n.experts)
	fx <- x
  if(min(w)<0 | max(w)<=0){stop("expert weights must be non-negative, and at least one weight must be greater than 0.")}
  
	if(length(w)==1){
	  w <- rep(w, n.experts)
	}
  
	weight <- matrix(w/sum(w), nxTotal, n.experts, byrow = T)
 
	for(i in 1:n.experts){
		densitydata <- expertdensity(fit, d[i], ex = i, xl, xu, ql, qu, nx)
		x[, i] <- densitydata$x
		fx[, i] <-densitydata$fx 
	}
	
	fx.lp <- apply(fx * weight, 1, sum)
	df1 <- data.frame(x = rep(x[, 1], n.experts + 1),
	                  fx = c(as.numeric(fx), fx.lp),
	                  expert = factor(c(rep(expertnames,
	                                        each = nxTotal),
	                                    rep(lpname, nxTotal)),
	                                  levels = c(expertnames,
	                                             lpname)),
	                  ftype = factor(c(rep("individual",
	                                       nxTotal * n.experts),
	                                   rep(lpname, nxTotal)),
	                                 levels = c("individual",
	                                            lpname))
	)
	
	if(legend_full){
	  
	  cols <- scales::hue_pal()(n.experts + 1)
	  linetypes <- c(rep("dashed", n.experts), "solid")
	  sizes <- lwd * c(rep(0.5, n.experts), 1.5)
	  names(cols) <- names(linetypes) <-
	    names(sizes) <- c(expertnames, lpname )
	  
	  p1 <- ggplot(df1, aes(x = x, y = fx, 
	                        colour = expert, 
	                        linetype = expert, 
	                        size = expert)) +
	    scale_colour_manual(values = cols) +
	    scale_linetype_manual(values = linetypes) +
	    scale_size_manual(values = sizes)}else{
	      p1 <- ggplot(df1, aes(x = x, y = fx, 
	                            colour =  ftype, 
	                            linetype=ftype, size =ftype)) +
	        scale_linetype_manual(name = "distribution", values = c("dashed", "solid"))+
	        scale_size_manual(name = "distribution", values = lwd * c(.5, 1.5)) +
	        scale_color_manual(name = "distribution", values = c("black", "red"))
	    }
	
	for(i in 1:n.experts){
	  if(d[i] == "hist"){
	    p1 <- p1 + geom_step(data = subset(df1, expert == expertnames[i]),
	                         aes(colour = expert))
	  }else{
	    p1 <- p1 + geom_line(data = subset(df1, expert == expertnames[i]),
	                   aes(colour = expert))
	  } 
	}
	
	if(length(unique(d)) == 1 & d[1] == "hist"){
	  p1 <- p1 + geom_step(data = subset(df1, expert == lpname),
	                       aes(colour = expert))
	}else{
	  p1 <- p1 + geom_line(data = subset(df1, expert == lpname),
	                 aes(colour = expert))
	} 
	
	
	 p1 <- p1 + labs(x = xlab, y = ylab)
	
	if((!is.null(ql)) & (!is.null(qu)) & addquantile){
	  if(legend_full){
	    ribbon_col <- scales::hue_pal()(n.experts + 1)[n.experts + 1]}else{
	      ribbon_col <- "red"
	    }
	  p1 <- p1 + geom_ribbon(data = with(df1, subset(df1, x <= ql  &expert == lpname)),
	                         aes(ymax = fx, ymin = 0),
	                         alpha = 0.2, show.legend = FALSE, colour = NA, fill =ribbon_col ) +
	    geom_ribbon(data = with(df1, subset(df1, x >=qu  &expert == lpname)),
	                aes(ymax = fx, ymin = 0),
	                alpha = 0.2, show.legend = FALSE, colour = NA, fill =ribbon_col )
	    
	  
	}
	 
	if(lpname == "marginal"){
	  p1 <- p1 + theme(legend.title = element_blank()) 
	} 
	 
	p1 + theme(text = element_text(size = fs))
}
