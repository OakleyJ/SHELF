#' Elicit a concordance probability for two uncertain quantities, and plot a joint sample
#' 
#' Given two elicited marginal distributions, open a browser in which one specifies a quadrant
#' probability P(X_1 > m_1, X_2 > m_2), where m_1 and m_2 are the elicited medians of X_1 and X_2.
#' A joint sample from the distribution of X_1 and X_2 is generated, using the two elicited marginal
#' distributions and a bivariate normal copula. 
#' 
#' 
#' @param fit1 An elicitation fit produced from the \code{fitdist}
#' command for the first uncertain quantity X_1.
#' @param fit2 An elicitation fit produced from the \code{fitdist}
#' command for the second uncertain quantity X_2.
#' @param m1 The elicited (or fitted) median of X_1.
#' @param m2 The elicited (or fitted) median of X_2.
#' @param d A vector of distributions to be used for each elicited quantity: a string with elements chosen from
#' \code{"Normal", "Student-t", "Gamma", "Log normal" "Log Student-t", "Beta"}. The default is to use 
#' the best fitting distribution in each case.
#' @param n The number of sampled (X_1, X_2) pairs to be plotted.
#' @return A matrix of sampled values, one row per sample.
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' p1 <- c(0.25, 0.5, 0.75)
#' v1 <- c(0.5, 0.55, 0.6)
#' v2 <- c(0.22, 0.3, 0.35)
#' myfit1 <- fitdist(v1, p1, 0, 1)
#' myfit2 <- fitdist(v2, p1, 0, 1)
#' elicitConcProb(myfit1, myfit2, 0.55, 0.3, d=c("Beta", "Beta"))
#' }
#' @import shiny
#' @importFrom ggExtra ggMarginal
#' @export

elicitConcProb <- function(fit1, fit2, m1, m2, 
                              d = c("best", "best"), 
                              n = 10000){
  qplabel <- paste("\\( P(X_1>", m1,
                   ",X_2 >", m2,
                   " \\mbox{ or } X_1 <", m1, 
                   ",X_2 <", m2,
                   ")\\)")
  conc.probs <- matrix(0.5, 2, 2)
  theta<-data.frame(copulaSample(fit1, fit2, cp=conc.probs, n=n, 
                                 d=d))
  xrange <- range(theta$X1)
  yrange <- range(theta$X2)
  
  ui <- basicPage(
    withMathJax(), 
    titlePanel("Elicit a concordance probability"),
    sliderInput("cprob", label = h4(qplabel), 
                0.5, min = 0, max = 1, step = 0.01),
    plotOutput("plot1"),
    numericInput("fs", label = h4("font size"), value = 12)
  )
  
  server <- function(input, output) {
    
    X1 <- X2 <- xpos <- ypos <- hjustvar <- vjustvar <- annotateText <- NULL # hack to avoid R CMD check NOTE
    
    output$plot1 <- renderPlot({
      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- input$cprob
      set.seed(1)
      theta<-data.frame(copulaSample(fit1, fit2, cp=conc.probs, n=n, 
                                     d=d))
    
      df1<-data.frame(theta)
      theme_set(theme_grey(base_size = input$fs))
      
      annotations <- data.frame(
        xpos = c(Inf,Inf,-Inf,-Inf),
        ypos =  c(Inf, -Inf,-Inf,Inf),
        annotateText = as.character(c(input$cprob / 2, 0.5 - input$cprob /2,
                                      input$cprob / 2, 
                                      0.5 - input$cprob /2)),
        hjustvar = c(1.5, 1.5, -0.5, -0.5) ,
        vjustvar = c(1.5, -0.5, -0.5, 1.5))
      
      
      p1<-ggplot(data=df1,aes(x=X1, y=X2))+
        geom_point(alpha=0.15, colour = "red") +
        geom_hline(yintercept = m2)+
        geom_vline(xintercept = m1)+
        labs(x=expression(X[1]), y = expression(X[2]))+
        geom_text(data = annotations, aes(x = xpos,
                                          y = ypos,
                                          hjust = hjustvar,
                                          vjust = vjustvar,
                                          label = annotateText),
                  size =10)+
        xlim(0.95*xrange[1], 1.05*xrange[2])+
        ylim(0.95*yrange[1], 1.05*yrange[2])
      suppressWarnings(suppressMessages(ggExtra::ggMarginal(p1, type = "histogram",
                                           fill = "red")))
    })
  }
  
  shinyApp(ui, server)
}
  
