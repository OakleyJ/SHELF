#' Elicit judgements and fit distributions interactively
#' 
#' Opens up a web browser (using the shiny package), from which you can specify
#' judgements, fit distributions and plot the fitted density functions with
#' additional feedback. Probabilities can be specified directly, or the roulette 
#' elicitation method can be used. 
#' 
#' All input arguments are optional, and can be set/changed within the app itself.
#' Click on the "Help" tab for instructions. Click the "Quit" button to exit the app and return
#' the results from the \code{fitdist} command. Click "Download report" to generate a report
#' of all the fitted distributions. 
#' 
#' @param lower A lower limit for the uncertain quantity X. 
#' Will be ignored when fitting distributions that are not bounded below. Also sets 
#' the lower limit for the grid in the roulette method.
#' @param upper An upper limit for the uncertain quantity X. 
#' Will be ignored when fitting distributions that are not bounded above. Also sets 
#' the upper limit for the grid in the roulette method.
#' @param gridheight The number of grid cells for each bin in the roulette method.
#' @param nbins The number of bins used in the rouletted method.
#' @param method Set to "roulette" for the app to display the roulette method by default.
#' Any other string will display the general method by default.

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
elicit<- function(lower = 0, upper = 100, gridheight = 10,
                  nbins = 10, method = "general"){
  
  limits <- paste0(lower, ", ", upper)
  if(method=="roulette"){
    startingMethod <- 2
    startingPanel <- "Roulette"}else{
    startingMethod <- 1
    startingPanel <- "PDF"
  }
  
  
  runApp(list(
  
  # User interface ----  
    
  ui = shinyUI(fluidPage(
    
    # Application title
    titlePanel("SHELF: single distribution"),
    
    sidebarLayout(
      sidebarPanel(
        wellPanel(
        textInput("limits", label = h5("Parameter limits"), 
                  value = limits),
        radioButtons("method", label = h5("Elicitation method"), 
                     choiceNames = list("General", "Roulette"),
                     choiceValues =  1:2,
                     selected = startingMethod)
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
                         value = nbins, min = 3),
            numericInput("gridHeight", 
                         label = h5("Grid height"), value = gridheight, min  = 1)
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
                                      'Mirror gamma' = "mirrorgamma",
                                      'Mirror log normal' = "mirrorlognormal",
                                      'Mirror log Student-t' = "mirrorlogt",
                                      'Best fitting' = "best")
                     ),
        conditionalPanel(
          condition = "input.dist == 't' || input.dist == 'logt' || input.dist == 'mirrorlogt'",
          numericInput("tdf", label = h5("Student-t degrees of freedom"),
                     value = 10)
          ),
        textInput("fq", label = h5("Feedback quantiles"), 
                  value = "0.1, 0.9"),
        uiOutput("feedbackProbabilities")
    
          )
        ),
        wellPanel(
          textInput("xLabel", label = h5("Parameter (x-axis) label"), 
                    value = "x")
          
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
                         numericInput("fs", label = "Font size (plots)", value = 16)
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
                           wellPanel(
                             h5("Fitted quantiles and cumulative probabilities"),
                             fluidRow(
                               column(4,
                                      tableOutput("valuesPDF")
                               ),
                               column(4,
                                      tableOutput("fittedProbsPDF")
                               )
                             )),
                           fluidRow(
                             column(4,
                                    downloadButton('downloadDensities',
                                                   "Download plot")),
                             column(3,  
                             uiOutput("setPDFxaxisLimits")
                             )
                           )
                         )),
                tabPanel("CDF", plotOutput("cdf"),
                         conditionalPanel(
                           condition = "input.showFittedCDF == true",
                           wellPanel(
                             h5("Fitted quantiles and cumulative probabilities"),
                             fluidRow(
                               column(4,
                                      tableOutput("valuesCDF")
                               ),
                               column(4,
                                      tableOutput("fittedProbsCDF")
                               )
                             )),
                           fluidRow(
                             column(4,
                                    downloadButton('downloadCDF',
                                                   "Download plot")),
                             column(3,  
                                    uiOutput("setCDFxaxisLimits")
                             )
                           )
                         )),
                tabPanel("Tertiles", plotOutput("tertiles"),
                         helpText("The coloured bars divide the plausible range into three equally likely regions, as specified by the tertiles. The median
                                  is shown by a dashed line. The tertiles and median displayed will either be the elicited values, if they have been provided,
                                  or estimates obtained by linear interpolation of the elicited probabilities, with zero probability assumed
                                  outside the parameter limits."),
                         fluidRow(
                           column(4,
                                  downloadButton('downloadTertiles',
                                                 "Download plot")))
                         ),
                tabPanel("Quartiles", plotOutput("quartiles"),
                         helpText("The coloured bars divide the plausible range 
into four equally likely regions, as specified by the quartiles. The quartiles displayed will either be the elicited quartiles,
                                  if they have been provided,
                                  or estimates obtained by linear interpolation of the elicited probabilities, with zero probability assumed
                                  outside the parameter limits."),
                         fluidRow(
                           column(4,
                                  downloadButton('downloadQuartiles',
                                                 "Download plot")))
                ),
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
                                    required."),
                           fluidRow(
                             column(4,
                                    downloadButton('downloadRoulette',
                                                   "Download plot")))),
                         conditionalPanel(
                           condition = "input.method == 1",
                           h5("Please select the roulette elicitation method")
                         )),
                tabPanel("Compare group/RIO",
                         helpText("If you are working with multiple experts, 
                                  and you have elicited their judgements individually with the 
                                  multiple experts app, you can download the judgements from that 
                                  app as a .csv file, and upload it here. You will then see a comparison
                                  between the individual judgements, a linear pool, and the distribution
                                  you have elicited in this app."),
                         fileInput("loadCSV", label = NULL, 
                                   buttonLabel = "Upload judgements"),
                         radioButtons("comparePlotType", label = h5("Plot Type"),
                                      choices = c("Density functions" = "density",
                                                  "Quartile judgements" = "quartiles",
                                                  "Tertile judgements" = "tertiles")),
                         plotOutput("compareRIO"),
                         ),
                tabPanel("Help", 
                         includeHTML(system.file("shinyAppFiles", "help.html",
                                                 package="SHELF"))
                         ),
                selected = startingPanel
              )
      )
    )
  )),
  
  # Server ----
   
  server = function(input, output) {
    
    # Parameter limits ----
    limits <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$limits, ")"))),
               error = function(e){NULL})
    })
    
    # Feedback quantiles ----
    fq <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$fq, ")"))),
               error = function(e){NULL})
      
    })
    
    # Feedback probabilities. Needs to know parameter limits ----
    output$feedbackProbabilities <- renderUI({
      textInput("fp", label = h5("Feedback probabilities"), 
                paste(limits(), collapse = ", "))
    }) 
    fp <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$fp, ")"))),
               error = function(e){NULL})
      
    }) 
    
    # Axes limits for pdf/cdf plots. Needs to know parameter limits ----
    output$setPDFxaxisLimits <- renderUI({
      textInput("xlimPDF", label = h5("x-axis limits"), 
                paste(limits(), collapse = ", "))
    }) 
    xlimPDF <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xlimPDF, ")"))),
               error = function(e){NULL})
      
    })
    output$setCDFxaxisLimits <- renderUI({
      textInput("xlimCDF", label = h5("x-axis limits"), 
                paste(limits(), collapse = ", "))
    }) 
    xlimCDF <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xlimCDF, ")"))),
               error = function(e){NULL})
      
    })
    
    # Elicited probabilities and values ----
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
    
    # Extract quartiles and tertiles by linear interpolation ----
    # (Will be correct if elicited directly)
    m <- reactive({
      req(p(), v(), limits())
      approx(c(0, p(), 1),
             c(limits()[1], v(), limits()[2]),
             0.5)$y
    })
    t1 <- reactive({
      req(p(), v(), limits())
      approx(c(0, p(), 1),
             c(limits()[1], v(), limits()[2]),
             1/3)$y
    })
    t2 <- reactive({
      req(p(), v(), limits())
      approx(c(0, p(), 1),
             c(limits()[1], v(), limits()[2]),
             2/3)$y
    })
    Q1 <- reactive({
      req(p(), v(), limits())
      approx(c(0, p(), 1),
             c(limits()[1], v(), limits()[2]),
             0.25)$y
    })
    Q3 <- reactive({
      req(p(), v(), limits())
      approx(c(0, p(), 1),
             c(limits()[1], v(), limits()[2]),
             0.75)$y
    })
    
    # Roulette ----

    # Extract number of bins and grid height, 
    # and grid positions for roulette method
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
    
    # Initial allocation of chips to bins
    rl <- reactiveValues(x=-1, y=-1,
                         chips = rep(0, 10),
                         allBinsPr = NULL,
                         nonempty = NULL
    )
    
    # Update allocation as bins are clicked on
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
    
    # Reset chip allocation if number of bins or limits change
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
   
    # Fit distributions to elicited judgements ----
    myfit <- reactive({
      req(limits(), v(), p())
      fitdist(vals = v(), probs = p(), lower = limits()[1],
              upper = limits()[2], 
              tdf = input$tdf)
    })
    
    # All plots have separate functions, so can be called from 
    # both renderPlot, and from ggsave() for downloading
    
    plotPDF <- function(){
      req(myfit(), xlimPDF(), fq(), input$fs, quantileValues())
      
      if(input$showFittedPDF){
        
        dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
        suppressWarnings(plotfit(myfit(), d = input$dist,
                                  ql = fq()[1], qu = fq()[2],
                                 xl = xlimPDF()[1], xu = xlimPDF()[2], 
                                 fs = input$fs,
                                 xlab = input$xLabel))
      }
      
    }
    output$distPlot <- renderPlot({
      plotPDF()
    })
    
    plotCDF <- function(){
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
                  xaxisUpper = xU,
                  xlab = input$xLabel)
      
    }
    output$cdf <- renderPlot({
      plotCDF()
    })
    
    plotTertileJudgements <- function(){
      req(limits(), t1(), m(), t2(), input$fs)
      makeTertilePlot(limits()[1], t1(), m(), t2(), limits()[2], input$fs,
                      xlab = input$xLabel)
    }
    output$tertiles <- renderPlot({
      plotTertileJudgements()
    })
    
    plotQuartileJudgements <- function(){
      req(limits(), Q1(), m(), Q3(), input$fs)
      makeQuartilePlot(limits()[1], Q1(), m(), Q3(), limits()[2], input$fs,
                       xlab = input$xLabel)
    }
    output$quartiles <- renderPlot({
      plotQuartileJudgements()
    })
    
    plotRoulette <- function(){
      
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
           xlab = input$xLabel)
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
      
    }
    output$roulette <- renderPlot({
      plotRoulette()
    })
    
    # Feedback - get fitted quantiles/probabilities ----
    
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
    probabilityValues <- reactive({
      req(fp(), myfit())
      
      FB <- feedback(myfit(), 
                     values = fp(),
                     ex = 1)
      
      if(input$dist == "best"){
        probs <- FB$fitted.probabilities[, 
                                         as.character(myfit()$best.fitting[1,
                                                                           1])]
      }else{
        probs <- FB$fitted.probabilities[, input$dist]
        
      }
      
      return(data.frame(values=fp(), probabilities = probs))
      
      
    }) 
    
    # ...and display on the PDF tab...
    output$valuesPDF <- renderTable({
      req(quantileValues())
      quantileValues()
    })
    output$fittedProbsPDF <- renderTable({
      req(probabilityValues())
      probabilityValues()
    })
    
    # ...and display on the CDF tab
    output$fittedProbsCDF <- renderTable({
      req(probabilityValues())
      probabilityValues()
    })
    output$valuesCDF <- renderTable({
      req(quantileValues())
      quantileValues()
    })
    
    # Compare individual elicited judgements with RIO ----
    
    groupFit <- reactive({
      file <- input$loadCSV
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      readSHELFcsv(file$datapath)
    })
    
    output$compareRIO <- renderPlot({
      req(groupFit(), myfit())
      compareGroupRIO(groupFit(), myfit(), type = input$comparePlotType,
                      fs = input$fs,
                      xlab = input$xLabel)
    })
    
    # Download individual plots ----
    output$downloadDensities = downloadHandler(
      filename = 'fittedPDF.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotPDF(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadCDF = downloadHandler(
      filename = 'fittedCDF.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotCDF(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadTertiles = downloadHandler(
      filename = 'tertiles.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotTertileJudgements(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadQuartiles = downloadHandler(
      filename = 'quartiles.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotQuartileJudgements(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadRoulette = downloadHandler(
      filename = 'roulette.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = plotRoulette(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    # Download R Markdown report
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
    
    # Quit app button
    observeEvent(input$exit, {
      stopApp(myfit())
    }) 
    
  }
  ), launch.browser = TRUE)
}
