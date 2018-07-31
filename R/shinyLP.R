#' Plot a weighted linear pool
#' 
#' Opens up a web browser (using the shiny package), from which you can choose the weights
#' and plot a weighted linear pool. Different distributions can be selected for the individual
#' experts' distribution, and feedback can be reported in the form of two quantiles from the 
#' weighted linear pool.  
#' 
#' The weights should be positive, but do not need to some to 1; they will be normalised. 
#' Click the Finish button to quit the elicitation session. This will output the distributions 
#' and weights used in the displayed linear pool.
#' 
#' @param fit An object of class \code{elicitation}.
#' 
#' @return An extract from the \code{fit} object of the components used in the linear pool.
#' See \code{\link{fitdist}} for details.
#' 
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' # Two experts
#' # Expert 1 states P(X<30)=0.25, P(X<40)=0.5, P(X<50)=0.75
#' # Expert 2 states P(X<20)=0.25, P(X<25)=0.5, P(X<35)=0.75
#' # Both experts state 0<X<100. 
#' 
#' v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
#' p <- c(0.25, 0.5, 0.75)
#' myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
#' 
#' plotWeightLP(myfit)
#' 
#' }
#' @export

plotWeightLP <- function(fit){
  
  dist<-c("normal", "t", "gamma", "lognormal", "logt","beta", "best")
  
  plotlimits <- paste(min(fit$limits), max(fit$limits) , sep = ",")
  
  # Determine set of suitable distributions
  if(fit$limits[1, 1]>=-Inf & fit$limits[1, 2] < Inf){
    distributionchoices <- list("Normal" = 1, "Student t" = 2,
                                "Gamma" = 3, "Log normal" = 4, "Log Student t" = 5,
                                "Beta" = 6, "Best fitting" =7)
  }
  if(fit$limits[1, 1]>=-Inf & fit$limits[1, 2] == Inf){
    distributionchoices <- list("Normal" = 1, "Student t" = 2,
                                "Gamma" = 3, "Log normal" = 4,
                                "Log Student t" = 5, "Best fitting" =7)
  }
  if(fit$limits[1, 1]==-Inf & fit$limits[1, 2] == Inf){
    distributionchoices <- list("Normal" = 1, "Student t" = 2, "Best fitting" =7)
  }

 
  
  ###
  
  
  runApp(list(
  ui = shinyUI(fluidPage(
    
    # Application title
    titlePanel("Weighted linear pool"),
    
    
    sidebarLayout(
      sidebarPanel(
        textInput("xlimits", label = h5("x-axis limits"), value = plotlimits),
        radioButtons("radio", label = h5("Distribution"), 
                     choices = distributionchoices, selected = 1 ),
   
   textInput("weights", label = h5("Linear pool weights"), 
             paste(rep(1, nrow(fit$vals)), collapse = ", ")),
   checkboxInput("showfeedback", label = "Show feedback", value = FALSE),
  numericInput("fq1", label = h5("lower feedback quantile"), value = 0.05,min=0,max=1),
   numericInput("fq2", label = h5("upper feedback quantile"), value = 0.95,min=0,max=1),
  actionButton("exit", "Finish"),
  numericInput("fs", label = h5("Font size"), value = 12)
                     
      ),
            mainPanel(
        plotOutput("distPlot"),
        
          tableOutput("values")
        
      )
    )
  )),
   
  server = function(input, output) {
    
    lpweights <- reactive({
      eval(parse(text=paste("c(",input$weights,")")))
    })
    
    
    fit <- get("fit")
    output$distPlot <- renderPlot({
      xlimits<-eval(parse(text=paste("c(",input$xlimits,")")))
      theme_set(theme_grey(base_size = input$fs))
      
        print(makeLinearPoolPlot(fit, xl = xlimits[1], 
                                 xu = xlimits[2], 
                                 d=dist[as.numeric(input$radio)], w = lpweights(), lwd=1, 
                                 xlab="x", ylab="x", legend_full = TRUE,
                                 ql = quantileValues()[1, 2], 
                                 qu = quantileValues()[2, 2],
                                 addquantile = input$showfeedback)) 
      
     
    })
    
    quantileValues <- reactive({
      values <- qlinearpool(fit, c(input$fq1,input$fq2), 
                            d=dist[as.numeric(input$radio)], 
                            w = lpweights())
      data.frame(quantiles=c(input$fq1,input$fq2), values=values)
      
    }) 
    
    output$values <- renderTable({
      if(input$showfeedback){quantileValues()}
      
    })
    
    observeEvent(input$exit, {
      stopApp(extractDistributions(fit,as.numeric(input$radio), lpweights()))
    }) 
    
}
))
}
