#' Elicit one set of probabilities using the roulette method.
#' 
#' Opens a shiny app for the roulette elicitation method. The user clicks in the
#' grid to allocate 'probs' to 'bins'. The elicited probability inside each
#' bin is the proportion of probs in each bin.
#' 
#' 
#' @param lower The lower limit on the x-axis of the roulette grid.
#' @param upper The upper limit on the x-axis of the roulette grid.
#' @param gridheight The maximum number of probs that can be allocated to a
#' single bin.
#' @param nbins The number of equally sized bins drawn between \code{lower} and
#' \code{upper}.
#' Click the Finish button to quit the elicitation session.
#' 
#' @return An object of class \code{elicitation}, which is returned once the 
#' Finish button has been clicked. See \code{\link{fitdist}} for details.
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
#' roulette(lower = 0, upper = 100)
#' }
#' @export

roulette <- function(lower=0, upper=100, gridheight=10, nbins=10){
  
  bin.width<-(upper-lower)/nbins
  bin.left<-seq(from=lower,to=upper-bin.width,length=nbins)
  bin.right<-seq(from=lower+bin.width,to=upper,length=nbins)
  xy<-list(x=lower,y=0)
  
  ui <- fluidPage(
    titlePanel("Roulette elicitation"),
    sidebarPanel(checkboxInput("fit", "Show fit", FALSE),
                 checkboxInput("round.end", "Spread end probs over empty bins", FALSE),
                 radioButtons("radio", label = h5("Distribution"), choices = list("Normal" = 2, 
                                                                                  "Student t" = 3, 
                                                                                  "Gamma" = 4, 
                                                                                  "Log normal" = 5, 
                                                                                  "Log Student t" = 6, 
                                                                                  "Beta" = 7, 
                                                                                  "Best fitting" =8), selected = 2 ),
                 numericInput("tdf", label = h5("Student-t degrees of freedom"), value = 3),
                 numericInput("fq1", label = h5("lower feedback quantile"), value = 0.05,
                              min = 0, max = 1, step = 0.01),
                 numericInput("fq2", label = h5("upper feedback quantile"), value = 0.95, 
                              min = 0, max = 1, step = 0.01),
                 actionButton("exit", "Finish")),
    mainPanel(plotOutput("plot1", click = "location"),
              plotOutput("plot2"))
  )
  
  server <- function(input, output) {
    
    vals <- reactiveValues(x=-1, y=-1, 
                           probs = rep(0,nbins), 
                           allBinsPr=NULL, nonempty = NULL
                           )
    
    observeEvent(input$location, {
      vals$x <-input$location$x
      vals$y <-input$location$y

      
      if(vals$x > lower & vals$x <upper & vals$y < gridheight){
        index <- which(vals$x >= bin.left & vals$x < bin.right)
        vals$probs[index]<-ceiling(max(vals$y,0))
        vals$allBinsPr <- cumsum(vals$probs)/sum(vals$probs)
        vals$nonempty <- vals$allBinsPr > 0 & vals$allBinsPr < 1
      }
     
    })
    
    v <- reactive({
      if(input$round.end == FALSE){bin.right}else{
        bin.right[vals$nonempty]
      }
    })
    p <- reactive({
      if(input$round.end == FALSE){vals$allBinsPr}else{
        vals$allBinsPr[vals$nonempty]
      }
    })
    
    myfit <- reactive({
      fitdist(v(), p(), lower, upper)
    })
    
    observeEvent(input$exit, {
        stopApp(myfit())
      }) 
    
    output$plot1 <- renderPlot({
      par(ps=15)
      plot(c(lower,upper),c(0,0),xlim=c(lower,upper),
           ylim=c(-1,max(gridheight,max(vals$probs)+1)),type="l",
           ylab="",xaxp=c(lower,upper,nbins), 
           main = paste("Total probs:", sum(vals$probs)),
           xlab="X")
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
      
      if((min(p()) < 0.4 & max(p())>0.6 & input$fit & input$round.end) | 
         (sum(p() > 0) >=2 & input$fit & !input$round.end)){
        dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
        plotfit(myfit(), d=dist[as.numeric(input$radio)], 
                int = F, ql=input$fq1, qu=input$fq2, xl = lower, xu = upper)
        
      }
      
    })
    
  }
  
  elicited <- suppressWarnings(runApp(list(ui=ui, server=server)))
  
  #elicited
}
