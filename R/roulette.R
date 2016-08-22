#' Elicit one set of probabilities using the roulette method.
#' 
#' Produces a graphics window with the roulette grid. The user clicks in the
#' window to allocate 'probs' to 'bins'. The elicited probability inside each
#' bin is the proportion of probs in each bin.
#' 
#' 
#' @param lower The lower limit on the x-axis of the roulette grid.
#' @param upper The upper limit on the x-axis of the roulette grid.
#' @param gridheight The maximum number of probs that can be allocated to a
#' single bin.
#' @param nbins The number of equally sized bins drawn between \code{Lo} and
#' \code{Up}.
#' @param round.end If set to \code{TRUE}, empty bins and the uppermost non-empty
#' bin will be ignored. For example, with 20 probs in total, if the uppermost
#' non-empty bin is [70,80] and contains 1 prob, setting \code{round.end = FALSE}
#' will result in an elicited probability P(X>80)=0, but setting
#' \code{round.end = TRUE} will remove this judgement, instead only having
#' P(X>70)=0.05.
#' @return A list, with outputs 
#' \item{v }{ upper limits of
#' each bin.}
#' \item{p }{ cumulative probabilities for each
#' upper bin limit.}
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' x <- roulette(lower = 0, upper = 100)
#' # Then allocate probs to bins and click "Finish"
#' 
#' # To fit distributions and see the results
#' myfit <- fitdist(vals = x$v, probs = x$p, lower = 0, upper = 100)
#' plotfit(myfit)
#' }
#' @export

roulette <- function(lower=0, upper=100, gridheight=10, nbins=10, round.end = T){
  
  bin.width<-(upper-lower)/nbins
  bin.left<-seq(from=lower,to=upper-bin.width,length=nbins)
  bin.right<-seq(from=lower+bin.width,to=upper,length=nbins)
  xy<-list(x=lower,y=0)
  
  ui <- fluidPage(
    titlePanel("Roulette elicitation"),
    sidebarPanel(checkboxInput("fit", "Show fit", FALSE),
                 radioButtons("radio", label = h5("Distribution"), choices = list("Normal" = 2, 
                                                                                  "Student t" = 3, 
                                                                                  "Gamma" = 4, 
                                                                                  "Log normal" = 5, 
                                                                                  "Log Student t" = 6, 
                                                                                  "Beta" = 7, 
                                                                                  "Best fitting" =8), selected = 2 ),
                 numericInput("tdf", label = h5("Student-t degrees of freedom"), value = 3),
                 numericInput("fq1", label = h5("lower feedback quantile"), value = 0.05,min=0,max=1),
                 numericInput("fq2", label = h5("upper feedback quantile"), value = 0.95,min=0,max=1),
                 actionButton("exit", "Finish")),
    mainPanel(plotOutput("plot1", click = "location"),
              plotOutput("plot2"))
  )
  
  server <- function(input, output) {
    
    vals <- reactiveValues(x=-1, y=-1, probs = rep(0,nbins), p=NULL, v = bin.right )
    
    observeEvent(input$location, {
      vals$x <-input$location$x
      vals$y <-input$location$y
      if(vals$x > lower & vals$x <upper & vals$y < gridheight){
        index<-ceiling(vals$x/upper*nbins)
        vals$probs[index]<-ceiling(max(vals$y,0))
        vals$p <- cumsum(vals$probs)/sum(vals$probs)
        vals$v <- bin.right
      }
      observe({
        if(input$exit > 0){
          stopApp(list(v = vals$v, p = vals$p))
        }
      }) 
      
      if(round.end == T){
        index <- vals$p>0 & vals$p<1
        vals$v <- vals$v[index]
        vals$p <- vals$p[index]
      }
      
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
      if(sum(vals$probs >0) >2 & input$fit){
        myfit <-fitdist(vals$v, vals$p, lower, upper)
        dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
        plotfit(myfit, d=dist[as.numeric(input$radio)], int = F, ql=input$fq1, qu=input$fq2, xl = lower, xu = upper)
        
      }
      
    })
    
  }
  
  elicited <- runApp(list(ui=ui, server=server))
  
  elicited
}
