#' Elicit judgements and fit distributions interactively using the tertile method
#' 
#' Opens up a web browser (using the shiny package), from which you can specify
#' the median and tertiles, fit distributions and plot the fitted density functions with
#' additional feedback.
#' 
#' Parameter limits determine which distributions can be fitted. Finite
#' lower limits are needed for the gamma, lognormal and log-t distributions,
#' and both limits must be finite for to fit a beta distribution. If a
#' histogram is fitted without specifying finite limits, endpoints are chosen
#' based on fitting a normal distribution.
#' 
#' 
#' Click the Finish button to quit the elicitation session.
#' 
#' @return An object of class \code{elicitation}, which is returned once the 
#' Finish button has been clicked. See \code{\link{fitdist}} for details.
#'  
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' 
#' elicitTertiles()
#' 
#' }
#' @import shiny
#' @import ggplot2
#' @export
elicitTertiles<- function(){
  
  runApp(list(
  ui = shinyUI(fluidPage(
    
    # Application title
    titlePanel("Elicitation: tertile method"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
      sidebarPanel(
        textInput("limits", label = h5("Parameter limits"), value = "0, 100"),
        textInput("values", label = h5("1st Tertile, Median, 2nd Tertile"), value = "33, 50, 66"),
        checkboxInput("showfit", label = "Show fitted distribution"),
        checkboxInput("showfeedback", label = "Show feedback", value = FALSE),
        radioButtons("radio", label = h5("Distribution"), choices = list("Histogram" = 1, "Normal" = 2, "Student t" = 3, "Gamma" = 4, "Log normal" = 5, "Log Student t" = 6, "Beta" = 7, "Best fitting" =8), selected = 1 ),
        numericInput("tdf", label = h5("Student-t degrees of freedom"), value = 3),
        numericInput("fq1", label = h5("lower feedback quantile"), value = 0.05,min=0,max=1),
        numericInput("fq2", label = h5("upper feedback quantile"), value = 0.95,min=0,max=1),
        actionButton("exit", "Finish"),
        numericInput("fs", label = h5("font size"), value = 12)
      ),
            mainPanel(
        plotOutput("distPlot"),
        tableOutput("values")
      )
    )
  )),
   
  server = function(input, output) {
    
    limits <- reactive({
      eval(parse(text = paste("c(", input$limits, ")")))
    })
    
    v <- reactive({
      eval(parse(text = paste("c(", input$values, ")")))
    })
    
    myfit <- reactive({
      fitdist(vals = v(), probs=c(1/3, 0.5, 2/3),
              lower=limits()[1], upper=limits()[2], tdf=input$tdf)
    })
    
    observeEvent(input$exit, {
      stopApp(myfit())
    }) 
    
    output$distPlot <- renderPlot({
      
      dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      
      p1 <-ggplot()+
        annotate("rect", xmin = limits()[1], 
                 xmax = v()[1], ymin=0.2, ymax = 0.8, fill = "#66c2a5")+
        annotate("rect", xmin = v()[1], xmax = v()[3], 
                 ymin=0.2, ymax = 0.8, fill = "#fc8d62")+
        annotate("rect", xmin = v()[3], xmax = limits()[2],
                 ymin=0.2, ymax = 0.8, fill = "#8da0cb")+
        xlim(limits()[1], limits()[2])+
        geom_vline(xintercept = v()[2], linetype = "dashed")+
        theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+
        scale_y_continuous(breaks = NULL, limits = c(0, 1))+
        labs(title = "Tertiles and median", y = expression(f[X](x))) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.y = element_text(colour = "white"),
              text = element_text(size = input$fs))
      if(input$showfit){
        p2 <- makeSingleExpertPlot(myfit(), d=dist[as.numeric(input$radio)],
                                   limits()[1], limits()[2], ql=input$fq1, qu=input$fq2,
                                   sf = 3, ex = 1,
                                   lwd = 1, xlab = "x",
                                   ylab = expression(f[X](x))) +
          theme(axis.text.y = element_blank(), 
                axis.ticks.y = element_blank(),
                text = element_text(size = input$fs))}else{
                                     p2 <- ggplot() + theme_void()
                                   
                                 }
      
      
      p1 <- ggplot_gtable(ggplot_build(p1))
      p2 <- ggplot_gtable(ggplot_build(p2))
      maxWidth = grid::unit.pmax(p1$widths[2:3], p2$widths[2:3])
      p1$widths[2:3] <- maxWidth
      p2$widths[2:3] <- maxWidth
      gridExtra::grid.arrange(p1, p2, heights = c(1.5, 4))
      
    })
    

    quantileValues <- reactive({
      ssq <- myfit()$ssq[1, is.na(myfit()$ssq[1,])==F]
      best.index <- which(ssq == min(ssq))[1]
      
      ex <- 1
      pl <- limits()[1]
      pu <- limits()[2]
      if(as.numeric(input$radio)==8){index<-best.index}else{index<-as.numeric(input$radio) - 1}
      if(as.numeric(input$radio)==1){
        if(pl == -Inf & myfit()$limits[ex,1] > -Inf){pl <- myfit()$limits[ex,1]}
        if(pu == Inf & myfit()$limits[ex,2] < Inf){pu <- myfit()$limits[ex,2] }
        if(pl == -Inf & myfit()$limits[ex,1] == -Inf){
          pl <- qnorm(0.001, myfit()$Normal[ex,1], myfit()$Normal[ex,2])}
        if(pu == Inf & myfit()$limits[ex,2] == Inf){
          pu <- qnorm(0.999, myfit()$Normal[ex,1], myfit()$Normal[ex,2])}
        p <- c(0, myfit()$probs[ex,], 1)
        x <- c(pl, myfit()$vals[ex,], pu)
        values <- qhist(c(input$fq1,input$fq2), x, p)
      }
      
      if(as.numeric(input$radio)>1){
        temp<-feedback(myfit(), quantiles=c(input$fq1,input$fq2), ex=1)
        values=temp$fitted.quantiles[,index]
      }
      data.frame(quantiles=c(input$fq1,input$fq2), values=values)
      
    }) 
    
    output$values <- renderTable({
      if(input$showfeedback){quantileValues()}
      
    })
    
  }
  ))
}
