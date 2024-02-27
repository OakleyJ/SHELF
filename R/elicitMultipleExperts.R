#' Elicit individual judgements and fit distributions for multiple experts
#' 
#' Opens up a web browser (using the shiny package), from which you can specify
#' judgements, fit distributions and plot the fitted density functions and a 
#' (weighted) linear pool with additional feedback.
#' 
#' Click the "Quit" button to exit the app and return
#' the results from the \code{fitdist} command. Click "Download report" to generate a report
#' of all the fitted distributions.
#' 
#' @return An object of class \code{elicitation}, which is returned once the 
#' Finish button has been clicked. See \code{\link{fitdist}} for details.
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' 
#' elicitMultiple()
#' 
#' }
#' @import shiny
#' @export

elicitMultiple <- function(){
  
  dist<-c("hist", "normal", "t", "gamma", "lognormal", "logt","beta", "best")
  
  # UI ----
  
  ui <- shinyUI(fluidPage(
    
    titlePanel("SHELF: individual distributions"),
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          numericInput("nExperts", label = h5("Number of experts"),
                       value = 2, min = 1),
          radioButtons("entry", "Input method", 
                       choices = c("Quantiles", "Roulette")),
          conditionalPanel(
            condition = "input.entry == 'Quantiles'",
            textInput("probs", label = h5("Cumulative probabilities"), 
                      value = "0.25, 0.5, 0.75")),
          conditionalPanel(
            condition = "input.entry == 'Roulette'",
            numericInput("nBins", label = h5("Number of bins"), value = 10),
            textInput("limits", label = h5("Parameter limits"), value = "0, 100")
          )
        ),
        wellPanel(
          selectInput("dist", label = "Distribution", 
                      choices =  list(Histogram = "hist",
                                      Normal = "normal", 
                                      'Student-t' = "t",
                                      'Skew normal' = "skewnormal",
                                      Gamma = "gamma",
                                      'Log normal' = "lognormal",
                                      'Log Student-t' = "logt",
                                      Beta = "beta",
                                      'Mirror gamma' = "mirrorgamma",
                                      'Mirror log normal' = "mirrorlognormal",
                                      'Mirror log Student-t' = "mirrorlogt",
                                      'Best fitting' = "best")
          ),
          checkboxInput("excludeLogT", "Exclude log-t and mirror log-t from best fit", TRUE),
          uiOutput("setPDFxaxisLimits"),
          checkboxGroupInput("lp", label = h5("Linear pool"), 
                             choices = list("Display linear pool" = 1)),
          conditionalPanel(
            condition = "input.lp == true",
            uiOutput("setLPWeights"),
            radioButtons("leg", label = h5("Linear pool legend"),
                         choices = list("full" = 1, "reduced" = 2), selected = 1 ),
            checkboxInput("showfeedback", label = "Show feedback", value = FALSE),
            conditionalPanel(
              condition = "input.showfeedback == true",
              textInput("fq", label = h5("Feedback quantiles"), 
                        value = "0.05, 0.95")
            )
          ),
          
          textInput("xLabel", label = h5("Parameter (x-axis) label"), 
                    value = "x")
          
          
        )
        
      ),
      
      mainPanel(
        wellPanel(
          fluidRow(
            column(3, selectInput("outFormat", label = "Report format",
                                  choices = list('html' = "html_document",
                                                 'pdf' = "pdf_document",
                                                 'Word' = "word_document"))
            ),
            column(3, offset = 1, 
                   numericInput("fs", label = "Font size",
                                value = 16)
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
          tabPanel("Judgements",
                   conditionalPanel(
                     condition = "input.entry == 'Quantiles'",
                     helpText("Enter the judgements in the table below,
                            one column per expert. Enter lower plausible limits in the first row,
                            upper plausible limits in the last row, and quantile values in between, 
                            corresponding to the cumulative probabilities."),
                     uiOutput("EnterQuantiles"),
                     fluidRow(
                       column(3, downloadButton("saveQuantiles", "Download judgements") ),
                       column(3, fileInput("loadQuantiles", label = NULL, 
                                           buttonLabel = "Upload judgements") )
                       
                     )),
                   conditionalPanel(
                     condition = "input.entry == 'Roulette'",
                     helpText("Enter the number of chips allocated to each bin, one row per expert. 
                            The bins are defined to have equal size, starting and ending at the specified
                            values in 'Parameter limits'. To fit distributions, 
                              each expert must have at least three non-empty bins."),
                     uiOutput("EnterChips"),
                     fluidRow(
                       column(3, downloadButton("saveChips", "Download judgements") ),
                       column(3, fileInput("loadChips", label = NULL, 
                                           buttonLabel = "Upload judgements") )
                       
                     )
                   ),
                   helpText("You can save and load judgements as .csv files. 
                 When loading a file, make sure the Input 
                 method, Number of experts, Cumulative probabilities/Number of bins 
                          and Parameter limits are correctly specified in the 
                          control panel.")
                   
                   
                   
          ),
          tabPanel("PDF",
                   plotOutput("distPlot"),
                   fluidRow(
                     column(4,
                            downloadButton('downloadDensities',
                                           "Download plot")),
                     
                     column(4,
                            tableOutput("bestFittingDistributions")
                     ),
                     column(4, 
                            conditionalPanel(
                              condition = "input.showfeedback == true",
                              tableOutput('lpquantiles')))
                   )
          ),
          tabPanel("Tertiles",
                   plotOutput("Tertiles"),
                   helpText("The coloured bars divide the plausible range for each expert
into three equally likely regions, as specified by each expert's tertiles. Each expert's median
is shown by a dashed line. The tertiles displayed will either be the elicited tertiles, if they have been provided,
or estimates obtained by linear interpolation of the elicited probabilities, with zero probability assumed
outside the plausible range."),
                   fluidRow(
                     column(4,
                            downloadButton('downloadTertiles',
                                           "Download plot")))
          ),
          tabPanel("Quartiles",
                   plotOutput("Quartiles"),
                   helpText("The coloured bars divide the plausible range for each expert
into four equally likely regions, as specified by each expert's quartiles. The quartiles displayed will either be the elicited quartiles,
if they have been provided,
                            or estimates obtained by linear interpolation of the elicited probabilities, with zero probability assumed
                            outside the plausible range."),
                   fluidRow(
                     column(4,
                            downloadButton('downloadQuartiles',
                                           "Download plot")))
          )
        )
        
      )
    )
  ))
  # Server ----
  
  server <- shinyServer(function(input, output) {
    
    
    pQuantile <- reactive({
      tryCatch(eval(parse(text = paste("c(",
                                       input$probs, ")"))),
               error = function(e){NULL})
    })
    
    pChip <- reactive({
      # Need at least 3 non-empty bins per expert
      check <- apply(input$myChips > 0, 1, sum)
      if(min(check) < 3 | min(input$myChips) < 0){
        return(NULL)
      }else{
        rouletteP <- apply(input$myChips, 1, cumsum) /
          matrix(apply(input$myChips, 1, sum), 
                 ncol(input$myChips), nExp(), byrow = TRUE)
        rownames(rouletteP) <- NULL
        return(rouletteP)
      }
      
    })
    
    lpweights <- reactive({
      tryCatch(eval(parse(text=paste("c(",input$weights,")"))),
               error = function(e){NULL})
      
    })
    
    fq <- reactive({
      feedbackq <- tryCatch(eval(parse(text=paste("c(",input$fq,")"))),
                            error = function(e){NULL})
      if(!is.null(feedbackq)){
        if(min(feedbackq)<=0 | max(feedbackq)>=1 | length(feedbackq) !=2){
          return(NULL)
        }
        if(length(feedbackq) == 2 & feedbackq[1] >= feedbackq[2]){
          return(NULL)
        }
      }
      return(feedbackq)
      
    })
    
    nExp <- reactive({
      req(input$nExperts)
      if(input$nExperts <=0 | !is.integer(input$nExperts)){
        return(NULL)}else{
          return(input$nExperts)
        }
      
    })
    
    limits <- reactive({
      tryCatch(eval(parse(text=paste("c(",input$limits,")"))),
               error = function(e){NULL})
    })
    
    boundaries <- reactive({
      req(limits(), input$nBins)
      
      if(length(limits()) == 1){return(NULL)}
      
      if(is.integer(input$nBins) & input$nBins > 0 & limits()[1] < limits()[2]){
        return(signif(seq(from = limits()[1],
                          to = limits()[2],
                          length = 1 + input$nBins),
                      3))
      }else{
        return(NULL)
      }
      
    })
    
    l <- reactive({
      if(input$entry == "Quantiles"){
        return(input$myvals[1, ])}
      if(input$entry == "Roulette"){
        return(boundaries()[1])}
    })
    
    u <- reactive({
      if(input$entry == "Quantiles"){
        return(input$myvals[nrow(input$myvals), ])}
      if(input$entry == "Roulette"){
        return(boundaries()[1 + input$nBins])}
    })
    
    vQuantile <- reactive({
      n <- nrow(input$myvals)
      as.matrix(input$myvals[2:(n - 1), ])
    })
    
    vChip <- reactive({
      req(boundaries())
      matrix(boundaries()[2:(input$nBins +1)],
             input$nBins,
             input$nExperts)
    })
    
    
    myfitQuantile <- reactive({
      req(pQuantile(), vQuantile(), l(), u())
      fitdist(vals = vQuantile(),
              probs = pQuantile(),
              lower = l(),
              upper = u(),
              expertnames = colnames(input$myvals),
              excludelogt = input$excludeLogT)
    })
    
    myfitChip <- reactive({
      if(is.null(pChip())){return(NULL)}else{
        return(fitdist(vals = vChip(),
                       probs = pChip(),
                       lower = l(),
                       upper = u(),
                       expertnames = rownames(input$myChips),
                       excludelogt = input$excludeLogT)
        )}
    })
    
    myfit <- reactive({
      if(input$entry == "Quantiles"){mf <- myfitQuantile()}
      if(input$entry == "Roulette"){mf <- myfitChip()}
      mf
    })
    
    expertNames <- reactive({
      if(input$entry == "Quantiles"){
        en <- colnames(input$myvals)}
      if(input$entry == "Roulette"){
        en <- rownames(input$myChips)}
      en
    })
    
    
    output$setLPWeights <- renderUI({
      req(nExp())
      textInput("weights", label = h5("Linear pool weights"), 
                paste(rep(1, nExp()), collapse = ", "))
    })
    
    output$EnterQuantiles <- renderUI({
      
      shinyMatrix::matrixInput(inputId = "myvals", value =  initialVals(),
                               class = "numeric",
                               cols = list(names = TRUE,
                                           editableNames = TRUE),
                               rows = list(names = TRUE))
    })
    
    newFile <- reactiveValues(chips = TRUE,
                              quantiles = TRUE)
    
    observeEvent(input$loadQuantiles,{
      newFile$quantiles <- FALSE
    }, priority = 1
    )
    
    observeEvent(input$loadChips,{
      newFile$chips <- FALSE
    }
    )
    
    initialChips <- reactive({
      req(boundaries(), input$nBins, nExp())
      
      inFile <- input$loadChips
      if (is.null(inFile) | isolate(newFile$chips)){
        initialdf <- matrix(0, nExp(), input$nBins)
      }else{
        initialdf <- as.matrix(utils::read.csv(inFile$datapath, row.names = 1))
        newFile$chips <- TRUE
        if(nrow(initialdf) != nExp() | ncol(initialdf) != input$nBins){
          showNotification("Make sure selected Number of experts and selected Number of bins
            match the numbers of rows and columns in the input file. Then try 
                     uploading the file again.", 
                           type = "error",
                           duration = 60)
          initialdf <- matrix(0, nExp(), input$nBins)
        }
      }
      rownames(initialdf) <- LETTERS[1:nExp()]
      
      colnames(initialdf)<- paste0("(",
                                   boundaries()[1:input$nBins],
                                   "-",
                                   boundaries()[2:(1 + input$nBins)], "]")
      initialdf
      
    })
    
    initialVals <- reactive({
      
      inFile <- input$loadQuantiles
      if (is.null(inFile) | isolate(newFile$quantiles)){
        initialdf <- matrix(rep(1:(2 + length(pQuantile())), nExp()),
                            2 + length(pQuantile()),
                            nExp())
        colnames(initialdf) <- LETTERS[1:nExp()]
      }else{
        initialdf <- as.matrix(utils::read.csv(inFile$datapath, row.names = 1))
        newFile$quantiles <- TRUE
        if(nrow(initialdf) != (2 + length(pQuantile())) |
           ncol(initialdf) != nExp()){
          showNotification("Check that the dimensions and row/column headings of the table in the input file
        match those displayed in the table here. Adjust the Number of experts and/or Cumulative probabilities
        as appropriate, then try uploading the file again.", 
                           type = "error",
                           duration = 60)
          initialdf <- matrix(rep(1:(2 + length(pQuantile())), nExp()),
                              2 + length(pQuantile()),
                              nExp())
        }
      }
      
      
      rownames(initialdf) <- c("L", pQuantile(), "U")
      initialdf
      
    })
    
    output$EnterChips <- renderUI({
      
      # Warning message: copy paste functionality has been removed from the current version. 
      # Please use version 0.4.0 if needed
      
      shinyMatrix::matrixInput(inputId = "myChips", value =  initialChips(),
                               class = "numeric",
                               cols = list(names = TRUE),
                               rows = list(names = TRUE,
                                           editableNames = TRUE))
    })
    
    
    quantileValues <- reactive({
      
      req(lpweights(), myfit(), fq())
      values <- qlinearpool(myfit(), fq(), 
                            d=input$dist, 
                            w = lpweights())
      data.frame(quantiles = fq(), values=values)
      
    }) 
    output$lpquantiles <- renderTable({
      req(quantileValues())
      quantileValues()
    })
    
    output$bestFittingDistributions <- renderTable({
      req(myfit(), nExp())
      df <- data.frame(expert = expertNames(),
                       bf = myfit()$best.fitting[, 1])
      
      colnames(df) <- c("expert", "best fit")
      df
    })
    
    output$setPDFxaxisLimits <- renderUI({
      req(input$myvals, boundaries())
      if(input$entry == "Quantiles"){
        initialRange <- range(input$myvals)
      }
      if(input$entry == "Roulette"){
        initialRange <- range(boundaries())
      }
      textInput("xlimPDF", label = h5("x-axis limits"), 
                paste(initialRange, collapse = ", "))
    })
    
    xlimPDF <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xlimPDF, ")"))),
               error = function(e){NULL})
      
    })
    
    densityPlot <- function(){
      req(myfit(), lpweights(), fq(), xlimPDF(), input$fs)
      xlimits <- xlimPDF()
      
      if(is.null(input$lp)){
        
        print(makeGroupPlot(myfit(), pl = xlimits[1], 
                            pu = xlimits[2], 
                            d=input$dist,
                            lwd = 1,
                            xlab = input$xLabel,
                            ylab = expression(f[X](x)),
                            fs = input$fs,
                            expertnames = expertNames()))}else{
                              print(makeLinearPoolPlot(myfit(), xl = xlimits[1], 
                                                       xu = xlimits[2], 
                                                       d=input$dist, w = lpweights(), lwd = 1,
                                                       xlab = input$xlabel,
                                                       ylab = expression(f[X](x)), legend_full = input$leg ==1,
                                                       ql = quantileValues()[1, 2], 
                                                       qu = quantileValues()[2, 2],
                                                       addquantile = input$showfeedback,
                                                       fs = input$fs,
                                                       expertnames = expertNames()))
                            }
      
    }
    
    output$distPlot <- renderPlot({
      densityPlot()
    })
    
    tertilePlot <- function(){
      req(myfit(), input$fs)
      tertilevals <- matrix(0, 3, input$nExperts)
      for(i in 1:input$nExperts){
        if(input$entry == "Quantiles"){
          
          tertilevals[, i] <- approx(c(0, pQuantile(), 1), 
                                     input$myvals[,  i],
                                     c(1/3, 0.5, 2/3))$y}
        if(input$entry == "Roulette"){
          
          tertilevals[, i] <- approx(pChip()[, i], 
                                     vChip()[, i],
                                     c(1/3, 0.5, 2/3))$y}
        
      }
      plotTertiles(tertilevals, l(), u(), fs = input$fs,
                   expertnames = expertNames(),
                   xl = xlimPDF(),
                   xlabel = input$xLabel)
      
    }
    
    output$Tertiles <- renderPlot({
      tertilePlot()
    })
    
    quartilePlot <- function(){
      req(myfit(), input$fs)
      quartilevals <- matrix(0, 3, input$nExperts)
      for(i in 1:input$nExperts){
        if(input$entry == "Quantiles"){
          quartilevals[, i] <- approx(c(0, pQuantile(), 1), 
                                      input$myvals[,  i],
                                      c(0.25, 0.5, 0.75))$y}
        if(input$entry == "Roulette"){
          quartilevals[, i] <- approx(pChip()[, i], 
                                      vChip()[, i],
                                      c(0.25, 0.5, 0.75))$y}
        
      }
      plotQuartiles(quartilevals, l(), u(), fs = input$fs,
                    expertnames = expertNames(),
                    xl = xlimPDF(),
                    xlabel = input$xLabel)
      
    }
    
    output$Quartiles <- renderPlot({
      quartilePlot()
      
    })
    
    observeEvent(input$exit, {
      stopApp(myfit())
    })
    
    
    output$saveQuantiles <- downloadHandler(
      filename = function() {
        paste('judgements-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        utils::write.csv(input$myvals, file)
      }
    )
    
    output$saveChips <- downloadHandler(
      filename = function() {
        paste('judgements-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        utils::write.csv(input$myChips, file)
      }
    )
    
    
    output$downloadDensities = downloadHandler(
      filename = 'expertDensities.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = densityPlot(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadTertiles = downloadHandler(
      filename = 'expertTertiles.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = tertilePlot(),
               device = device, width = 5,
               height = 3, units = "in")
      })
    
    output$downloadQuartiles = downloadHandler(
      filename = 'expertQuartiles.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = 5, height = 3,
                         res = 300, units = "in")
        }
        ggsave(file, plot = quartilePlot(),
               device = device, width = 5,
               height = 3, units = "in")
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
        tempReport <- file.path(tempdir(), "elicitationShinySummaryGroup.Rmd")
        file.copy(system.file("shinyAppFiles", "elicitationShinySummaryGroup.Rmd",
                              package="SHELF"),
                  tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(fit = myfit(), 
                       entry = input$entry, 
                       chips = input$myChips)
        
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
    
  })
  
  
  ## run app 
  runApp(list(ui=ui, server=server), launch.browser = TRUE)
}
