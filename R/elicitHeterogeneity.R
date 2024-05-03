#' Elicit a prior distribution for a random effects variance parameter
#' 
#' Opens a shiny app for the roulette elicitation method. The user clicks in the
#' grid to allocate 'probs' to 'bins'. The elicited probability inside each
#' bin is the proportion of probs in each bin. This will fit a distribution to the ratio R
#' of the 'largest' (97.5th percentile) to 'smallest' (2.5th percentile) treatment effect. 
#' A distribution for the variance effects variance parameter is inferred from the distribution 
#' of R, assuming that the random effects are normally distributed.
#' 
#' @param lower The lower limit on the x-axis of the roulette grid.
#' @param upper The upper limit on the x-axis of the roulette grid.
#' @param gridheight The maximum number of probs that can be allocated to a
#' single bin.
#' @param nbins The number of equally sized bins drawn between \code{lower} and
#' \code{upper}.
#' @param scale.free Logical. Default is \code{TRUE} for a scale free treatment effect,
#'  such as an odds ratio, hazard ratio or relative risk. Set to \code{FALSE} for a treatment effect
#'  that is scale dependent, or is on the probit scale. An approximation to the treatment effect
#'  on the logit scale will be used (assuming a dichotomised response). 
#' @param sigma Individual observation standard deviation, required if \code{scale.free} is
#'  \code{FALSE}.
#' @return BUGS code for incorporating the prior within a BUGS model. Additionally, a list with outputs 
#' \item{allocation }{table of bins, with number of probs allocated to each bin.}
#' \item{Gamma }{parameters of the fitted gamma distribution.}
#' \item{Log.normal }{parameters of the fitted lognormal distribution.}
#' \item{sumsq }{sum of squares of elicited - fitted probabilities for each distribution.}
#' \item{best.fitting}{the distribution with the lowest sum of squares.}
#' 
#' @note Regarding the option ``spread end probs over empty bins'' 
#' (unchecked as the default): suppose for example, the leftmost and rightmost non-empty
#' bins are [10,20] and [70,80], and each contain one prob, with 20 probs used in total. If the option
#' is unchecked, it is assumed P(X<20) = P(X>70) = 0.05 and P(X<10) = P(X>80) = 0. If the option
#' is checked, it is assumed P(X<20) = P(X>70) = 0.05 only. 
#' 
#' 
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' elicitHeterogen()
#' }
#' @export

elicitHeterogen <- function(lower = 1, upper = 10, 
                            gridheight = 10, nbins = 9,
                            scale.free = TRUE,
                            sigma = 1){
  
  ## Variables to be passed to the app
  multiplier <- sigma * ifelse(scale.free, 1, sqrt(3) / pi)
  l1 <- 0.1 * multiplier
  l2 <- 0.5 * multiplier
  l3 <- 1 * multiplier
  bin.width <- (upper - lower) / nbins
  bin.left <- seq(from = lower, to = upper - bin.width, length = nbins)
  bin.right <- seq(from = lower + bin.width,
                   to = upper, length = nbins)
  xy <- list(x = lower, y = 0)
  
  
  ui <- fluidPage(
    h1("Eliciting prior beliefs about heterogeneity", align = "center"),
    hr(),
    fluidRow(
    
    column(3, wellPanel(checkboxInput("fit", "Show fit", FALSE),
                 checkboxInput("round.end", "Spread end probs over empty bins", FALSE),
                 radioButtons("radio", label = h5("Distribution"), 
                              choices = list("Gamma" = 1,
                                             "Log normal" = 2,
                                             "Best fitting" = 3)),
                 actionButton("exit", "Finish"))),
      column(8, h4("Specify judgements about ratio of 'largest'
                   to 'smallest' treatment effect", align = "center"),
             plotOutput("plot1", click = "location"))),
    hr(),
    fluidRow(
      column(4, h4(textOutput("subtitle1"), align ="center"), plotOutput("plot2")),
      column(4, h4(textOutput("subtitle2"), align ="center"), plotOutput("plot3")),
      column(3, h4(textOutput("subtitle3"), align ="center"), tableOutput("values")))
      
     
  )
  
  server <- function(input, output) {
    
    vals <- reactiveValues(x = -1, y = -1, 
                           probs = rep(0, nbins), 
                           p = NULL, v = bin.right,
                           myfit = NULL,
                           pheteroG = NULL,
                           pheteroL = NULL,
                           phetero = NULL,
                           df.gamma = NULL,
                           df.lognormal = NULL)
    
    observeEvent(input$exit, {
     
      if(!is.null(vals$myfit)){
        
        g1 <- signif(vals$myfit$Gamma[1], 4)
        g2 <- signif(vals$myfit$Gamma[2], 4)
        l1 <- signif(vals$myfit$Log.normal[1], 4)
        l2 <- signif(1 / vals$myfit$Log.normal[2]^2, 4)

        line1g <- paste("R ~ dgamma(", g1, ", ",g2,")\n", sep = "")
        line1l <- paste("R ~ dlnorm(", l1, ", ",l2,")\n", sep = "")
        line2 <- ifelse(multiplier == 1,
                        paste("tau <- log(R + ", lower,") / 3.92 ", sep=""),
                        paste("tau <- ", signif(multiplier, 4)," * ",
                              "log(R + ", lower,") / 3.92 ", sep=""))
        line3 <- "\nprecision <- pow(tau, -2)"
        cat("## Gamma prior BUGS code\n", line1g, line2, line3, sep="")
        cat("\n\n## Log normal prior BUGS code\n", line1l, line2, line3, "\n\n", sep="")

        vals$myfit$allocation <- matrix(c(bin.left, bin.right, vals$probs),
                                  nrow = nbins)
        colnames(vals$myfit$allocation) <- c("lower endpoint", "upper endpoint", "no. probs")


    vals$myfit$sumsq <- vals$myfit$ssq[c("gamma", "lognormal")]
        vals$myfit$best.fitting <- ifelse(vals$myfit$sumsq[1]< vals$myfit$sumsq[2],
                                          "Gamma", "Log normal")
        colnames(vals$myfit$best.fitting) <- NULL
        attr(vals$myfit$best.fitting, "dim") <- NULL
      
        stopApp(vals$myfit[c("allocation", "Gamma", "Log.normal", "sumsq", "best.fitting")])
      
        }}
    )
    
    
    observeEvent(input$location, {
      vals$x <- input$location$x
      vals$y <- input$location$y

      # Update allocation of probs to bins
      if(vals$x > lower & vals$x <upper & vals$y < gridheight ){
        index <- which(vals$x >= bin.left & vals$x < bin.right)
        vals$probs[index] <- ceiling(max(vals$y, 0))
        vals$p <- cumsum(vals$probs) / sum(vals$probs)
        vals$v <- bin.right
      }

      # Extract CDF judgements from allocation
      if(any(vals$probs >0)){
      if(input$round.end == T ){
        index <- vals$p > 0 & vals$p < 1
        vals$v <- vals$v[index]
        vals$p <- vals$p[index]
      
        }else{
        newv <- c(lower, vals$v)
        newp <- c(0, vals$p)
        i1 <- max(which(newp == 0))
        i2 <- match(1, newp)
        vals$v <- newv[i1:i2]
        vals$p <- newp[i1:i2]
        }
      }

      # Fit distribution, if allocation is sufficient
      
      vcheck <- checkJudgementsValid(probs = vals$p,
                           vals = vals$v, tdf = 1, 
                           lower = lower,
                           upper = upper, silent = TRUE,
                           excludeExponential = TRUE)
      if(vcheck$valid == TRUE){
        vals$myfit <- fitdist(vals$v, vals$p, lower, upper)
      }else{
        vals$myfit <- NULL
      }
     
      #vals$myfit <- try(fitdist(vals$v, vals$p, lower, upper), silent = TRUE)
      #if (inherits(vals$myfit, "try-error")){
      #  vals$myfit <- NULL}

      if(!is.null(vals$myfit)){
        allDfs <- getKDEandPheteroDf(vals$myfit, multiplier, l1, l2, l3, lower)
        vals$pheteroG <- allDfs$pheteroG
        vals$pheteroL <- allDfs$pheteroL
        vals$df.gamma <- allDfs$df.gamma
        vals$df.lognormal <- allDfs$df.lognormal
        vals$phetero <- vals$pheteroG
        if(as.numeric(input$radio) == 2){vals$phetero <- vals$pheteroL}
        if(as.numeric(input$radio) == 3){
          if(vals$myfit$ssq[3] > vals$myfit$ssq[4]){vals$phetero <- vals$pheteroL}
        }
      }
    })
    
    observeEvent(input$round.end, {
      if(max(vals$probs) > 0){
      # Reset extracted p and v from allocation.
      vals$p <- cumsum(vals$probs) / sum(vals$probs)
      vals$v <- bin.right

      # Adjust given round.end
      if(input$round.end == T){
        index <- vals$p > 0 & vals$p < 1
        vals$v <- vals$v[index]
        vals$p <- vals$p[index]
      }else{
        newv <- c(lower, vals$v)
        newp <- c(0, vals$p)
        i1 <- max(which(newp == 0))
        i2 <- match(1, newp)
        vals$v <- newv[i1:i2]
        vals$p <- newp[i1:i2]
      }

      # Fit distribution, if allocation is sufficient
      vals$myfit <- try(fitdist(vals$v, vals$p, lower, upper), silent = TRUE)
      if (inherits(vals$myfit, "try-error")){
        vals$myfit <- NULL}

      if(!is.null(vals$myfit)){
        allDfs <- getKDEandPheteroDf(vals$myfit, multiplier, l1, l2, l3, lower)
        vals$pheteroG <- allDfs$pheteroG
        vals$pheteroL <- allDfs$pheteroL
        vals$df.gamma <- allDfs$df.gamma
        vals$df.lognormal <- allDfs$df.lognormal
        vals$phetero <- vals$pheteroG
        if(as.numeric(input$radio) == 2){vals$phetero <- vals$pheteroL}
        if(as.numeric(input$radio) == 3){
          if(vals$myfit$ssq[3] > vals$myfit$ssq[4]){vals$phetero <- vals$pheteroL}
        }
      }
      }
    })
    
    observeEvent(input$radio, {
      if(!is.null(vals$myfit)){
        vals$phetero <- vals$pheteroG
        if(as.numeric(input$radio) == 2){vals$phetero <- vals$pheteroL}
        if(as.numeric(input$radio) == 3){
          if(vals$myfit$ssq[3] > vals$myfit$ssq[4]){vals$phetero <- vals$pheteroL}
        }
      }
      
    })
    
    output$plot1 <- renderPlot({
      par(ps=15)
      plot(c(lower,upper),c(0,0),xlim=c(lower,upper),
           ylim=c(-1,max(gridheight,max(vals$probs)+1)),type="l",
           ylab="",xaxp=c(lower,upper,nbins), 
           main = paste("Total probs:", sum(vals$probs)),
           xlab="R")
      for(i in 1:nbins){
        lines(c(bin.left[i],bin.left[i]),
              c(0,max(gridheight,max(vals$probs)+1)),lty=3,col=8)
      }
      lines(c(bin.right[nbins],bin.right[nbins]),
            c(0,max(gridheight,max(vals$probs)+1)),lty=3,col=8)
      
      for(i in 1:gridheight){
        lines(c(lower,upper),c(i,i), lty=3,col=8)
      }
      
      for(i in 1:nbins){
        if(vals$probs[i]>0){
          rect(rep(bin.left[i],vals$probs[i]),c(0:(vals$probs[i]-1)),
               rep(bin.right[i],vals$probs[i]),c(1:vals$probs[i]),col=2)
        }
      }
      
      
      
      
    })
    
    output$plot2 <- renderPlot({
      req(vals$myfit)

      if(input$fit & !is.null(vals$myfit)){
        dist<-c("gamma", "lognormal", "gamma")
        if(as.numeric(input$radio) == 3){
          if(vals$myfit$ssq[3] > vals$myfit$ssq[4] ){
            dist[3] <- "lognormal"
          }
        }
           xu <- max(c(upper, 
                      qgamma(0.999, as.numeric(vals$myfit$Gamma[1]),
                             as.numeric(vals$myfit$Gamma[2])), 
                      qlnorm(0.999,as.numeric(vals$myfit$Log.normal[1]),
                             as.numeric(vals$myfit$Log.normal[2]))))
      
        plotfit(vals$myfit, d=dist[as.numeric(input$radio)],
                 ql=0.025, qu=0.975, xl = lower, xu = xu,
                xlab = "r", ylab = expression(f[R](r)))
      }
    })
    
    output$subtitle1 <- renderText({
      if(input$fit & !is.null(vals$myfit)){
        "Fitted distribution for R"
      }
    })
    
    output$subtitle2 <- renderText({
      if(input$fit & !is.null(vals$myfit)){
        "Sampled distribution for tau"
      }
    })
    
    output$subtitle3 <- renderText({
      if(input$fit & !is.null(vals$myfit)){
        "Probability of heterogeneity magnitude"
      }
    })
    

       output$plot3 <- renderPlot({
       if(input$fit & !is.null(vals$myfit)){
         
         if(as.numeric(input$radio) == 1){df1 <- vals$df.gamma}
         if(as.numeric(input$radio) == 2){df1 <- vals$df.lognormal}
         if(as.numeric(input$radio) == 3){
           if(vals$myfit$ssq[3] < vals$myfit$ssq[4]){df1 <- vals$df.gamma}else{
             df1 <- vals$df.lognormal}
         }
           

         r <- range(df1$x)
         x <- fx <- NULL
         p1<-ggplot(df1, aes(x = x, y = fx)) +
           geom_line(size = 1) +
           labs(title = "Kernel density estimate",
                x = expression(tau), y = expression(f(tau)))
         if(r[1] < l1){
           p1 <- p1 + geom_ribbon(data = subset(df1, x <= l1),
                                  aes(ymax = fx, ymin = 0),
                                  fill = "green",
                                  alpha = 0.2)
         }
         if(r[1] < l2 & r[2] > l1){
           p1 <- p1 + geom_ribbon(data = subset(df1, x > l1 & x <= l2),
                                  aes(ymax = fx, ymin = 0),
                                  fill = "yellow",
                                  alpha = 0.4)
         }
         if(r[1] < l3 & r[2] > l2){
           p1 <- p1 + geom_ribbon(data = subset(df1, x > l2 & x  <= l3),
                                  aes(ymax = fx, ymin = 0),
                                  fill = "orange",
                                  alpha = 0.4)
         }
         if(r[2] > l3){
           p1 <- p1 + geom_ribbon(data = subset(df1, x > l3),
                                  aes(ymax = fx, ymin = 0),
                                  fill = "red",
                                  alpha = 0.5)
         }

         p1

         
       }

     })
       
       output$values <- renderTable({
         if(input$fit & !is.null(vals$myfit)){
           vals$phetero
        }  
         
       }, digits = 3)
    
  }
  
  elicited <- runApp(list(ui=ui, server=server))
  
  elicited
}

getKDEandPheteroDf <- function(myfit, multiplier, l1, l2, l3, lower, n = 10000){
  Rg <- rgamma(n, as.numeric(myfit$Gamma[1]), as.numeric(myfit$Gamma[2]))
  Rl <- rlnorm(n, as.numeric(myfit$Log.normal[1]), as.numeric(myfit$Log.normal[2]))
  tauG <- multiplier * log(Rg + lower)/3.92
  tauL <- multiplier * log(Rl + lower)/3.92
  pheteroG <- getPheteroDf(tauG, l1, l2, l3)
  pheteroL <- getPheteroDf(tauL, l1, l2, l3)
  kde.gamma <- density(tauG)
  kde.lognormal <- density(tauL)
  df.gamma <- data.frame(x = kde.gamma$x, fx = kde.gamma$y)
  df.lognormal <- data.frame(x = kde.lognormal$x, fx = kde.lognormal$y)
  list(pheteroG = pheteroG, 
       pheteroL = pheteroL,
       df.gamma = df.gamma,
       df.lognormal = df.lognormal)
}

getPheteroDf <- function(tau, l1, l2, l3){
  prob1 <- round(mean(tau > l1 & tau <= l2), 3)
  prob2 <- round(mean(tau > l2 & tau <= l3), 3)
  prob3 <- round(mean(tau > l3), 3)
  data.frame(heterogeneity = c("low", "moderate", "high", "extreme"),
             probability = c(abs(1 - prob1 - prob2 - prob3),
                             prob1, prob2, prob3))
}




