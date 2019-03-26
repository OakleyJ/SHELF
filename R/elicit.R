#' Elicit judgements and fit distributions interactively
#' 
#' Opens up a web browser (using the shiny package), from which you can specify
#' judgements, fit distributions and plot the fitted density functions with
#' additional feedback. Probabilities can be specified directly, or the roulette 
#' elicitation method can be used. 
#' 
#' Click on the "Help" tab for instructions. Click the "Quit" button to exit the app and return
#' the results from the \code{fitdist} command. Click "Download report" to generate a report
#' of all the fitted distributions.
#'
#' @aliases elicit roulette 
#' @return An object of class \code{elicitation}, which is returned once the 
#' Quit button has been clicked. See \code{\link{fitdist}} for details.
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' 
#' elicit()
#' 
#' }
#' @import shiny
#' @export
elicit<- function(){
  runApp(list(
  ui = shinyUI(fluidPage(
    
    # Application title
    titlePanel("SHELF: single distribution"),
    
    sidebarLayout(
      sidebarPanel(
        wellPanel(
        textInput("limits", label = h5("Parameter limits"), 
                  value = "0, 100"),
        radioButtons("method", label = h5("Elicitation method"), 
                     choiceNames = list("General", "Roulette"),
                     choiceValues =  1:2,
                     selected = 1)
        ),
        conditionalPanel(
          condition = "input.method == 1",
          wellPanel(
            h5("Elicited judgements"),
            textInput("values", label = h5("Parameter values"), 
                  value = "25, 50, 75"),
            textInput("probs", label = h5("Cumulative probabilities"), 
                  value = "0.25, 0.5, 0.75")
            )
          ),
        conditionalPanel(
          condition = "input.method == 2",
          wellPanel(
            h5("Roulette options"),
            numericInput("nBins", label = h5("Number of bins"),
                         value = 10),
            numericInput("gridHeight", 
                         label = h5("Grid height"), value = 10)
          )
        ),
            
        wellPanel(
          h5("Fitting and feedback"),
          checkboxInput("showFittedPDF", label = "show fitted PDF"),
          checkboxInput("showFittedCDF", label = "show fitted CDF"),
           conditionalPanel(
            condition = "input.showFittedPDF == true || input.showFittedCDF == true",
          
          selectInput("dist", label = "Distribution", 
                      choices =  list(Histogram = "hist",
                                      Normal = "normal", 
                                      'Student-t' = "t",
                                      Gamma = "gamma",
                                      'Log normal' = "lognormal",
                                      'Log Student-t' = "logt",
                                      Beta = "beta", 
                                      'Best fitting' = "best")
                     ),
        conditionalPanel(
          condition = "input.dist == 't' || input.dist == 'logt'",
          numericInput("tdf", label = h5("Student-t degrees of freedom"),
                     value = 3)
          ),
        # checkboxInput("showQuantiles", label = "show fitted quantiles"),
        # conditionalPanel(
        #   condition = "input.showQuantiles == true",
          numericInput("fq1", label = h5("lower feedback quantile"),
                       value = 0.05,min=0,max=1),
          numericInput("fq2", label = h5("upper feedback quantile"),
                       value = 0.95,min=0,max=1)
        #)
          )
        )
      #  downloadButton("report", "Download report"),
        
        # Include in the R package, but not in the web app
        #actionButton("exit", "Finish"),
        
        #actionButton("exit", "Quit")
      ),
            mainPanel(
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              
              
              wellPanel(
                fluidRow(
                  column(3, selectInput("outFormat", label = "Report format",
                                        choices = list('html' = "html_document",
                                                       'pdf' = "pdf_document",
                                                       'Word' = "word_document"))
                  ),
                  column(3, offset = 1, 
                         numericInput("fs", label = "Font size", value = 12)
                  )),
                fluidRow(
                  column(3, downloadButton("report", "Download report")
                  ),
                  column(2, offset = 1, actionButton("exit", "Quit")
                )
                )
                
              ),
              hr(),
              
              tabsetPanel(
                tabPanel("PDF", 
                         plotOutput("distPlot"),
                         conditionalPanel(
                           condition = "input.showFittedPDF == true",
                         tableOutput("valuesPDF"))),
                tabPanel("CDF", plotOutput("cdf"),
                         conditionalPanel(
                           condition = "input.showFittedCDF == true",
                         tableOutput("valuesCDF"))),
                tabPanel("Tertiles", plotOutput("tertiles"),
                         helpText("The coloured bars divide the plausible range into three equally likely regions, as specified by the tertiles. The median
                                  is shown by a dashed line. The tertiles and median displayed will either be the elicited values, if they have been provided,
                                  or estimates obtained by linear interpolation of the elicited probabilities, with zero probability assumed
                                  outside the parameter limits.")),
                tabPanel("Quartiles", plotOutput("quartiles"),
                         helpText("The coloured bars divide the plausible range 
into four equally likely regions, as specified by the quartiles. The quartiles displayed will either be the elicited quartiles,
                                  if they have been provided,
                                  or estimates obtained by linear interpolation of the elicited probabilities, with zero probability assumed
                                  outside the parameter limits.")),
                tabPanel("Roulette", 
                         conditionalPanel(
                           condition = "input.method == 2",
                           plotOutput("roulette",
                                                click = "location"),
                           helpText("Click directly in the plot to allocate probs to bins. 
                                    Click just below the line at 0 on the y-axis to clear a bin. 
                                    Note that in the distribution fitting, empty bins are not assumed to have 
                                    a probability of 0: probabilities from adjacent non-empty bins will be 
                                    smoothed out over the empty bins. Set the Parameter Limits and use an 
                                    appropriate choice of fitted distribution to enforce zero probabilities as
                                    required.")),
                         conditionalPanel(
                           condition = "input.method == 1",
                           h5("Please select the roulette elicitation method")
                         )),
                tabPanel("Help", 
                         includeHTML(system.file("shinyAppFiles", "help.html",
                                                 package="SHELF"))
                         )
              )
      )
    )
  )),
   
  server = function(input, output) {
    
    limits <- reactive({
      eval(parse(text = paste("c(", input$limits, ")")))
    })
    
    p <- reactive({
      gp <- eval(parse(text = paste("c(", input$probs, ")")))
      rp <- rl$allBinsPr[rl$nonEmpty]
      if(input$method ==1){myp <- gp}else{myp <- rp}
      myp
    })
    
    v <- reactive({
      gv <- eval(parse(text = paste("c(", input$values, ")")))
      rv <- bin.right()[rl$nonEmpty]
      if(input$method ==1){myv <- gv}else{myv <- rv}
      myv
    })
  
    myfit <- reactive({
        fitdist(vals = v(), probs = p(), lower = limits()[1],
              upper = limits()[2], 
              tdf = input$tdf)
    })
    
    t1 <- reactive({
      if(min(p()) > 1/3){
        xIn <- c(0, p())
        yIn <- c(limits()[1], v())
      }else{
        xIn <- p()
        yIn <- v()
      }
      approx(xIn, yIn, 1/3)$y
    })
    
    Q1 <- reactive({
      if(min(p()) > 0.25){
        xIn <- c(0, p())
        yIn <- c(limits()[1], v())
      }else{
        xIn <- p()
        yIn <- v()
      }
      approx(xIn, yIn, 0.25)$y
    })
    
    Q3 <- reactive({
      if(max(p()) < 0.75){
        xIn <- c(p(), 1)
        yIn <- c(v(), limits()[2])
      }else{
        xIn <- p()
        yIn <- v()
      }
      approx(xIn, yIn, 0.75)$y
    })
    
    t2 <- reactive({
      if(max(p()) < 2/3){
        xIn <- c(p(), 1)
        yIn <- c(v(), limits()[2])
      }else{
        xIn <- p()
        yIn <- v()
      }
      approx(xIn, yIn, 2/3)$y
    })
    
    m <- reactive({
      approx(p(), v(), 0.5)$y
    })
    
    bin.width <- reactive({
      (limits()[2] - limits()[1]) / input$nBins
    })
    
    bin.left <- reactive({
      seq(from = limits()[1],
          to = limits()[2] - bin.width(),
          length=input$nBins)
    })
    
    bin.right <- reactive({
      seq(from = limits()[1] + bin.width(),
          to = limits()[2],
          length = input$nBins)
    })
    
    
    rl <- reactiveValues(x=-1, y=-1,
                         chips = rep(0, 10),
                         allBinsPr = NULL,
                         nonempty = NULL
    )
    
    observeEvent(input$nBins,{
      rl$chips <- rep(0, input$nBins)
    })
    
    observeEvent(input$limits,{
      rl$chips <- rep(0, input$nBins)
    })

    # Include in the package, but not in the web app.
     observeEvent(input$exit, {
       stopApp(myfit())
     }) 
    
    output$distPlot <- renderPlot({
      
    if(input$showFittedPDF){
      
      dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      suppressWarnings(plotfit(myfit(), d = input$dist,
                               int = F, ql = input$fq1, qu = input$fq2,
                               xl = limits()[1], xu = limits()[2], 
                               fs = input$fs))
    }
      
    })
    
    output$tertiles <- renderPlot({
      makeTertilePlot(limits()[1], t1(), m(), t2(), limits()[2], input$fs)
    })
    
    output$quartiles <- renderPlot({
      makeQuartilePlot(limits()[1], Q1(), m(), Q3(), limits()[2], input$fs)
    })
    
    output$cdf <- renderPlot({
      
      if(input$dist == "best"){
        ssq <- myfit()$ssq[1, is.na(myfit()$ssq[1,])==F]
        best.index <- which(ssq == min(ssq))[1]
        dist <-c("normal", "t", "gamma", "lognormal",
              "logt","beta")
        makeCDFPlot(limits()[1], v(), p(), limits()[2], input$fs,
                  myfit(), dist = dist[best.index],
                  showFittedCDF = input$showFittedCDF,
                  showQuantiles = TRUE)}else{
                    makeCDFPlot(limits()[1], v(), p(), limits()[2], input$fs,
                                myfit(), dist = input$dist,
                                showFittedCDF = input$showFittedCDF,
                                showQuantiles = TRUE)
                    
                  }
      
    })
    

    quantileValues <- reactive({
      # ssq <- myfit()$ssq[1, is.na(myfit()$ssq[1,])==F]
      # best.index <- which(ssq == min(ssq))[1]
      # 
      # ex <- 1
      # pl <- limits()[1]
      # pu <- limits()[2]
      # #  if(input$dist=="hist"){
      #   if(pl == -Inf & myfit()$limits[ex,1] > -Inf){pl <- myfit()$limits[ex,1]}
      #   if(pu == Inf & myfit()$limits[ex,2] < Inf){pu <- myfit()$limits[ex,2] }
      #   if(pl == -Inf & myfit()$limits[ex,1] == -Inf){
      #     pl <- qnorm(0.001, myfit()$Normal[ex,1], myfit()$Normal[ex,2])}
      #   if(pu == Inf & myfit()$limits[ex,2] == Inf){
      #     pu <- qnorm(0.999, myfit()$Normal[ex,1], myfit()$Normal[ex,2])}
      #   p <- c(0, myfit()$probs[ex,], 1)
      #   x <- c(pl, myfit()$vals[ex,], pu)
      #   values <- qhist(c(input$fq1,input$fq2), x, p)
      # }
      
      FB <- feedback(myfit(), 
                                 quantiles = c(input$fq1, input$fq2),
                                 ex = 1)
      
      
      # 
      # if(input$dist != "hist" & input$dist!= "best"){
      #    values=temp$fitted.quantiles[, input$dist]
      # }
      
      if(input$dist == "best"){
        ssq <- myfit()$ssq[1, is.na(myfit()$ssq[1,])==F]
        best.index <- which(ssq == min(ssq))[1]
        values <- FB$fitted.quantiles[, best.index]
      }else{
        # cleverly, I used different distribution labels in feedback...
        index <- switch(input$dist,
                        normal = "Normal",
                        t = "Student-t",
                        gamma = "Gamma",
                        lognormal = "Log normal",
                        logt = "Log Student-t",
                        beta = "Beta",
                        hist = "Histogram")
        values <- FB$fitted.quantiles[, index]
        
      }
     
      
      
      data.frame(quantiles=c(input$fq1,input$fq2), values=values)
      
    }) 
    
    output$valuesPDF <- renderTable({
      quantileValues()
    })
   
    output$valuesCDF <- renderTable({
      quantileValues()
    })
    output$roulette <- renderPlot({
      
      plotHeight <-  max(input$gridHeight, 
                         max(rl$chips) + 1)
      
      par(ps = input$fs)
      plot(c(limits()[1], limits()[2]), c(0, 0),
           xlim=c(limits()[1], limits()[2]),
           ylim=c(-1, plotHeight),
           type="l",
           ylab="",
           xaxp=c(limits()[1], limits()[2], input$nBins), 
           main = paste("Total probs:", sum(rl$chips)),
           xlab="X")
      for(i in 1:input$nBins){
        lines(c(bin.left()[i],bin.left()[i]),
              c(0, plotHeight),lty=3,col=8)
      }
      lines(c(bin.right()[input$nBins],bin.right()[input$nBins]),
            c(0, plotHeight),lty=3,col=8)

      for(i in 1:plotHeight){
        lines(c(limits()[1], limits()[2]),
              c(i,i), lty=3,col=8)
      }

      for(i in 1:input$nBins){
        if(rl$chips[i]>0){
          rect(rep(bin.left()[i],rl$chips[i]),c(0:(rl$chips[i]-1)),
               rep(bin.right()[i],rl$chips[i]),c(1:rl$chips[i]),col=2)
        }
      }
      
    })
    
    observeEvent(input$location, {
      rl$x <-input$location$x
      rl$y <-input$location$y
      
      plotHeight <- max(input$gridHeight, max(rl$chips) + 1)


      if(rl$x > limits()[1] & rl$x < limits()[2] & rl$y < plotHeight){
        index <- which(rl$x >= bin.left() & rl$x < bin.right())
        rl$chips[index]<-ceiling(max(rl$y, 0))
        rl$allBinsPr <- cumsum(rl$chips)/sum(rl$chips)
        rl$nonEmpty <- rl$allBinsPr > 0 & rl$allBinsPr < 1
      }

    })
    
    observeEvent(input$exit, {
      stopApp(myfit())
    }) 
    
    output$report <- downloadHandler(
      filename = function(){switch(input$outFormat,
                                   html_document = "distributions-report.html",
                                   pdf_document = "distributions-report.pdf",
                                   word_document = "distributions-report.docx")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "elicitationShinySummary.Rmd")
        file.copy(system.file("shinyAppFiles", "elicitationShinySummary.Rmd",
                              package="SHELF"),
                  tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(fit = myfit())
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          output_format = input$outFormat,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
  }
  ), launch.browser = TRUE)
}
