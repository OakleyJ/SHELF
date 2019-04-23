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
                         value = 10, min = 3),
            numericInput("gridHeight", 
                         label = h5("Grid height"), value = 10, min  = 1)
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
        textInput("fq", label = h5("Feedback quantiles"), 
                  value = "0.05, 0.95")
    
          )
        )
     
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
                           fluidRow(
                             column(4, 
                                    uiOutput("setPDFxaxisLimits")),
                             column(4,
                                    tableOutput("valuesPDF")
                             )
                           )
                         )),
                tabPanel("CDF", plotOutput("cdf"),
                         conditionalPanel(
                           condition = "input.showFittedCDF == true",
                           fluidRow(
                             column(4, 
                                    uiOutput("setCDFxaxisLimits")),
                             column(4,
                                    tableOutput("valuesCDF")
                             )
                             
                           )
                         )),
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
      tryCatch(eval(parse(text = paste("c(", input$limits, ")"))),
               error = function(e){NULL})
    })
    
    fq <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$fq, ")"))),
               error = function(e){NULL})
      
    })
    
    p <- reactive({
      gp <-  tryCatch(eval(parse(text = paste("c(", input$probs, ")"))),
                      error = function(e){NULL})
        
      rp <- rl$allBinsPr[rl$nonEmpty]
      if(input$method ==1){myp <- gp}else{myp <- rp}
      myp
    })
    
    v <- reactive({
      gv <-  tryCatch(eval(parse(text = paste("c(", input$values, ")"))),
                      error = function(e){NULL})
        
      rv <- bin.right()[rl$nonEmpty]
      if(input$method ==1){myv <- gv}else{myv <- rv}
      myv
    })
  
    myfit <- reactive({
      req(limits(), v(), p())
        fitdist(vals = v(), probs = p(), lower = limits()[1],
              upper = limits()[2], 
              tdf = input$tdf)
    })
    
    t1 <- reactive({
      req(p(), v(), limits())
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
      req(p(), v(), limits())
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
      req(p(), v(), limits())
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
      req(p(), v(), limits())
      if(max(p()) < 2/3){
        xIn <- c(p(), 1)
        yIn <- c(v(), limits()[2])
      }else{
        xIn <- p()
        yIn <- v()
      }
      approx(xIn, yIn, 2/3)$y
    })
    
    nBins <- reactive({
      req(input$nBins)
      if(is.integer(input$nBins) & input$nBins > 0){
        return(input$nBins)}else{
          return(NULL)}
        
    })
    
    gridHeight <- reactive({
      req(input$gridHeight)
      if(is.integer(input$gridHeight) & input$gridHeight > 0){
        return(input$gridHeight)}else{
          return(NULL)}
      
    })
    
    m <- reactive({
      req(p(), v())
      approx(p(), v(), 0.5)$y
    })
    
    bin.width <- reactive({
      req(limits(), nBins())
      (limits()[2] - limits()[1]) / nBins()
    })
    
    bin.left <- reactive({
      req(limits(), nBins(), bin.width())
      seq(from = limits()[1],
          to = limits()[2] - bin.width(),
          length=nBins())
    })
    
    bin.right <- reactive({
      req(limits(), nBins(), bin.width())
      seq(from = limits()[1] + bin.width(),
          to = limits()[2],
          length = nBins())
    })
    
    
    rl <- reactiveValues(x=-1, y=-1,
                         chips = rep(0, 10),
                         allBinsPr = NULL,
                         nonempty = NULL
    )
    
    observeEvent(input$nBins,{
      req(input$nBins)
      if(is.integer(input$nBins) & input$nBins > 0){
      rl$chips <- rep(0, input$nBins)}
    })
    
    observeEvent(input$limits,{
      req(input$nBins)
      if(is.integer(input$nBins) & input$nBins > 0){
      rl$chips <- rep(0, input$nBins)}
    })

     observeEvent(input$exit, {
       stopApp(myfit())
     }) 
    
     output$setPDFxaxisLimits <- renderUI({
       textInput("xlimPDF", label = h5("x-axis limits"), 
                 paste(limits(), collapse = ", "))
     }) 
     
     xlimPDF <- reactive({
       tryCatch(eval(parse(text = paste("c(", input$xlimPDF, ")"))),
                error = function(e){NULL})
       
     })
     
    output$distPlot <- renderPlot({
      req(myfit(), xlimPDF(), fq(), input$fs, quantileValues())
    
    if(input$showFittedPDF){
      
      dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      suppressWarnings(plotfit(myfit(), d = input$dist,
                               int = F, ql = fq()[1], qu = fq()[2],
                               xl = xlimPDF()[1], xu = xlimPDF()[2], 
                               fs = input$fs))
    }
      
    })
    
    output$tertiles <- renderPlot({
      req(limits(), t1(), m(), t2(), input$fs)
      makeTertilePlot(limits()[1], t1(), m(), t2(), limits()[2], input$fs)
    })
    
    output$quartiles <- renderPlot({
      req(limits(), Q1(), m(), Q3(), input$fs)
      makeQuartilePlot(limits()[1], Q1(), m(), Q3(), limits()[2], input$fs)
    })
    
    output$setCDFxaxisLimits <- renderUI({
      textInput("xlimCDF", label = h5("x-axis limits"), 
                paste(limits(), collapse = ", "))
    }) 
    
    xlimCDF <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xlimCDF, ")"))),
               error = function(e){NULL})
      
    })
    
    output$cdf <- renderPlot({
      req(myfit(), xlimCDF(), limits(), fq())
      
      if(input$dist == "best"){
        mydist <- as.character(myfit()$best.fitting[1, 1])
       }else{
          mydist <- input$dist
        }
      
      if(is.null(xlimCDF())){
        xL <- limits()[1]
        xU <- limits()[2]
      }else{
        xL <- xlimCDF()[1]
        xU <- xlimCDF()[2]
      }
     
  
        makeCDFPlot(lower = limits()[1],
                    v = v(),
                    p = p(), limits()[2],
                    input$fs,
                    fit = myfit(),
                    dist = mydist,
                    showFittedCDF = input$showFittedCDF,
                    showQuantiles = TRUE,
                    ql = fq()[1],
                    qu = fq()[2],
                    xaxisLower = xL,
                    xaxisUpper = xU)
      
    })
    
    
    quantileValues <- reactive({
      req(fq(), myfit())
      
      if(min(fq())<=0 | max(fq())>=1){
        return(NULL)
      }else{
        
        FB <- feedback(myfit(), 
                       quantiles = fq(),
                       ex = 1)
        
        if(input$dist == "best"){
          values <- FB$fitted.quantiles[, 
                                        as.character(myfit()$best.fitting[1,
                                                                          1])]
        }else{
          values <- FB$fitted.quantiles[, input$dist]
          
        }
        
        return(data.frame(quantiles=fq(), values=values))
      }
      
    }) 
    
    output$valuesPDF <- renderTable({
      req(quantileValues())
      quantileValues()
    })
   
    output$valuesCDF <- renderTable({
      req(quantileValues())
      quantileValues()
    })
    output$roulette <- renderPlot({
      req(limits(), input$fs, nBins(), gridHeight(), bin.left(),
          bin.right())
      
      plotHeight <-  max(gridHeight(), 
                         max(rl$chips) + 1)
      
      par(ps = input$fs)
      plot(c(limits()[1], limits()[2]), c(0, 0),
           xlim=c(limits()[1], limits()[2]),
           ylim=c(-1, plotHeight),
           type="l",
           ylab="",
           xaxp=c(limits()[1], limits()[2], nBins()), 
           main = paste("Total probs:", sum(rl$chips)),
           xlab="X")
      for(i in 1:nBins()){
        lines(c(bin.left()[i],bin.left()[i]),
              c(0, plotHeight),lty=3,col=8)
      }
      lines(c(bin.right()[nBins()],bin.right()[nBins()]),
            c(0, plotHeight),lty=3,col=8)

      for(i in 1:plotHeight){
        lines(c(limits()[1], limits()[2]),
              c(i,i), lty=3,col=8)
      }

      for(i in 1:nBins()){
        if(rl$chips[i]>0){
          rect(rep(bin.left()[i],rl$chips[i]),c(0:(rl$chips[i]-1)),
               rep(bin.right()[i],rl$chips[i]),c(1:rl$chips[i]),col=2)
        }
      }
      
    })
    
    observeEvent(input$location, {
      rl$x <-input$location$x
      rl$y <-input$location$y
      
      plotHeight <- max(gridHeight(), max(rl$chips) + 1)


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
