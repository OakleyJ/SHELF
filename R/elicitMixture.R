#' Elicit a mixture distribution using the extension method
#' 
#' Opens up a web browser (using the shiny package), from which you can specify
#' judgements, fit distributions and plot the fitted density function.
#' 
#' Click the "Quit" button to exit the app and return
#' the fitted distributions. Click "Download report" to generate a report
#' of all the fitted distributions.
#' 
#' @return When the Quit button is clicked, a list, with elements
#' \item{fit}{an object of class \code{elicitation}. See \code{\link{fitdist}} for details.}
#' \item{extensionProbs}{the probability mass function for the extension variable.}
#' 
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' 
#' elicitMixture()
#' 
#' }
#' @import shiny
#' @export

elicitMixture <- function(){
  
  
#### ui ####
    
  ui <- shinyUI(fluidPage(
    
    titlePanel("SHELF: extension method - mixture distributions"),
    
    #### sidebar ####
    
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition="input.myTab==1",

        wellPanel(
          h4("Extension variable"),
          numericInput("nY", label = h5("Number of possible values"),
                       value = 3, min = 1)
          )
        ),
        conditionalPanel(condition="input.myTab==2",
                         
                         wellPanel(
        h4("Conditional probability judgements"),
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
          )),
        conditionalPanel(condition="input.myTab==2 | input.myTab==3",
        
          wellPanel(
            conditionalPanel(condition="input.myTab==2",
                             h4("Conditional density plots"),
                             uiOutput("extensionValue"),
                             selectInput("dist", label = h5("Distribution"), 
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
                                                         'Best fitting' = "best"),
                                         #choiceValues = 1:8,
                                         selected = "normal"
                             )),
            conditionalPanel(condition="input.myTab==3",
                             h4("Marginal density plot"),
                             uiOutput("allDistributions")),
            hr(),
            
            uiOutput("setPDFxaxisLimits"),
            
            textInput("fq", label = h5("Feedback quantiles"), 
                      value = "0.05, 0.95")
            
          
        )
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
                   numericInput("fs", label = "Font size", value = 12)
            ),
            conditionalPanel(condition = "input.myTab == 3",
                             column(3,
                                    numericInput("sampleSize", 
                                                 label = "sample size",
                                                 value = 1000, min = 1, step = 1)
                             ))
            ),
          fluidRow(
            column(3, downloadButton("report", "Download report")
            ),
            column(3, offset = 1, actionButton("exit", "Quit")
            ),
            conditionalPanel(condition = "input.myTab == 3",
                             column(3,
                                    downloadButton("downloadData", "Download sample")))
          )
          
        ),
        
        hr(),
        
        #### UI tabs ####
        
        tabsetPanel(id = "myTab",
          tabPanel("Extension variable", value = 1, 
                   helpText("Specify the marginal probability distribution of
                            the extension variable: the probability of the
                            extension variable taking each of its possible values.
                            Click in the cells to edit. You can also click on the 
                            column names to change them."),
                   uiOutput("ExtensionProbs"),
                   plotOutput("barplot")),
          tabPanel("Judgements", value = 2, 
                   conditionalPanel(
                     condition = "input.entry == 'Quantiles'",
                     helpText("Enter the judgements in the table below,
                            one column per value of the extension variable. Each column gives judgements
                            about the target variable, 
                            conditional on the corresponding value of the extension variable.
                            Enter lower plausible limits in the first row,
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
                     helpText("Enter the number of chips allocated to each bin, one row per value of 
                     the extension variable. Each row corresponds to a conditional distribution
                     of the target variable, given the respective value of the extension variable.
                            The bins are defined to have equal size, starting and ending at the specified
                            values in 'Parameter limits'. To fit distributions, 
                              each row must have at least three non-empty bins."),
                     uiOutput("EnterChips"),
                     fluidRow(
                       column(3, downloadButton("saveChips", "Download judgements") ),
                       column(3, fileInput("loadChips", label = NULL, 
                                           buttonLabel = "Upload judgements") )
                       
                     )
                   ),
                   helpText("You can save and load judgements as .csv files. 
                 When loading a file, make sure the Input 
                 method, Number of possible values for the extension variable,
                 Cumulative probabilities/Number of bins 
                          and Parameter limits are correctly specified in the 
                          control panel."),
                   hr(),
                   plotOutput("condDistPlot")
                   
                   
                   
                   
          ),
          tabPanel("PDF", value = 3, 
                   plotOutput("distPlot"),
                   fluidRow(
                     column(4,
                            tableOutput("bestFittingDistributions")
                     ),
                     column(4,
                            tableOutput('lpquantiles'))
                   )
                     
                   
          )
          
        )
        
      )
    )
  ))

#### server ####
  
  server <- shinyServer(function(input, output) {
    
    # Hack to avoid CRAN check NOTE
    
    probability <- value <- NULL
    

    #### Define reactive values ####
        
    pQuantile <- reactive({
      pQ <- tryCatch(eval(parse(text = paste("c(",
                                       input$probs, ")"))),
               error = function(e){NULL})
      if(!is.null(pQ)){
        if(min(pQ) > 0.4 | max(pQ) < 0.6){
          showNotification("Smallest cumulative probability needs to be less than 0.4,
                           and largest needs to be greater than 0.6.",
                           type = "error",
                           duration  = 10)
        }
      }
      pQ
    })
    
    pChip <- reactive({
      req(input$myChips)
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
      req(input$nY)
      if(input$nY <=0 | !is.integer(input$nY)){
        return(NULL)}else{
          return(input$nY)
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
      req(input$myvals)
      n <- nrow(input$myvals)
      as.matrix(input$myvals[2:(n - 1), ])
    })
    
    vChip <- reactive({
      req(boundaries())
      matrix(boundaries()[2:(input$nBins +1)],
             input$nBins,
             input$nY)
    })
    
    myfitQuantile <- reactive({
      req(pQuantile(), vQuantile(), l(), u())
      tryCatch(fitdist(vals = vQuantile(),
              probs = pQuantile(),
              lower = l(),
              upper = u(),
              expertnames = colnames(input$extensionProbs),
              tdf = 10),
              error = function(e){NULL})
    })
    
    myfitChip <- reactive({
      if(is.null(pChip())){return(NULL)}else{
      return(tryCatch(fitdist(vals = vChip(),
              probs = pChip(),
              lower = l(),
              upper = u(),
              expertnames = colnames(input$extensionProbs))),
             error = function(e){NULL})}
    })
    
    myfit <- reactive({
      if(input$entry == "Quantiles"){mf <- myfitQuantile()}
      if(input$entry == "Roulette"){mf <- myfitChip()}
      mf
    })
    
    initialPy <- reactive({
      Py <- matrix(round(1 / input$nY, 2), 1, input$nY)
      Py[1, 1] <- 1 - sum(Py[1, -1 ])
      rownames(Py) <- "probability"
      colnames(Py) <- paste0("Y=", 1:input$nY)
      
      
      Py
    })
    
    newFile <- reactiveValues(chips = TRUE,
                              quantiles = TRUE)
    
    validPy <- reactiveValues(valid = TRUE)
    
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
      rownames(initialdf) <- colnames(input$extensionProbs)
      
      colnames(initialdf)<- paste0("(",
                                   boundaries()[1:input$nBins],
                                   "-",
                                   boundaries()[2:(1 + input$nBins)], "]")
      initialdf
      
    })
    
    initialVals <- reactive({
      
      inFile <- input$loadQuantiles
      if (is.null(inFile) | isolate(newFile$quantiles)){
        initialdf <- matrix(rep(1:(2 + length(pQuantile())), input$nY),
                            2 + length(pQuantile()),
                            input$nY)
        
      }else{
        initialdf <- as.matrix(utils::read.csv(inFile$datapath, row.names = 1))
        newFile$quantiles <- TRUE
        if(nrow(initialdf) != (2 + length(pQuantile())) |
           ncol(initialdf) != input$nY){
          showNotification("Check that the dimensions and row/column headings of the table in the input file
        match those displayed in the table here. Adjust the number of values 
        for the extension variable (on the extension variable tab) and/or Cumulative probabilities
        as appropriate, then try uploading the file again.", 
                           type = "error",
                           duration = 60)
          initialdf <- matrix(rep(1:(2 + length(pQuantile())), input$nY),
                              2 + length(pQuantile()),
                              input$nY)
        }
      }
      
      colnames(initialdf) <- colnames(input$extensionProbs)
      rownames(initialdf) <- c("L", pQuantile(), "U")
      initialdf
      
    })
    
    
    xSample <- reactive({
      req(myfit(), input$dist1)
  
      xMatrix <- matrix(0, input$sampleSize, input$nY )
      
      for(i in 1:input$nY){
        dist <- eval(parse(text = paste0("input$dist", i)))
        if(dist == "best"){
          dist <- as.character(myfit()$best.fitting[i, 1])
        }
        xMatrix[, i] <- sampleFit(myfit(), n = input$sampleSize, expert = i)[, dist]
      }
      
      data.frame(X = apply(xMatrix,
                           1, 
                           sample,
                           size = 1,
                           prob = input$extensionProbs[1, ]))
    })
    
    quantileValues <- reactive({
      
      distVector <- rep("best", input$nY)
      
      for(i in 1:input$nY){
        distVector[i] <- eval(parse(text = paste0("input$dist", i)))
      }
      
      req(input$extensionProbs[1, ], myfit(), fq())
      values <- qlinearpool(myfit(), fq(), 
                            d = distVector, 
                            w = input$extensionProbs[1, ])
      data.frame(quantiles = fq(), values=values)
      
    }) 
    
    xlimPDF <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xlimPDF, ")"))),
               error = function(e){NULL})
      
    })
    
   
    
    #### User defined UI ####
    
    output$extensionValue <- renderUI({
      selectInput("extensionVariableValue", 
                  label = h5("Extension variable value"),
                  choices = colnames(input$extensionProbs))
    })
    
    output$ExtensionProbs <- renderUI({
      shinyMatrix::matrixInput(inputId = "extensionProbs", value =  initialPy(),
                               class = "numeric",
                               cols = list(names = TRUE, editableNames = TRUE),
                               rows = list(names = TRUE))
      
    })
    
    output$EnterQuantiles <- renderUI({
      
      shinyMatrix::matrixInput(inputId = "myvals", value =  initialVals(),
                               class = "numeric",
                               cols = list(names = TRUE),
                               rows = list(names = TRUE))
    })
    
    output$EnterChips <- renderUI({
      
      shinyMatrix::matrixInput(inputId = "myChips", value =  initialChips(),
                               class = "numeric",
                               cols = list(names = TRUE),
                               rows = list(names = TRUE))
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
    
    output$allDistributions <- renderUI({
      
      distnames <- paste0("dist", 1:input$nY)
      allControls <- vector("list", input$nY)
      for(i in 1:input$nY){
        allControls[[i]] <- selectInput(distnames[i], 
                    label = colnames(input$extensionProbs)[i], 
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
                                    'Best fitting' = "best"),
                    selected = "best")
      }
      tagList(allControls)
    })
    
    #### observe events ####
    
    
    
    observeEvent(input$extensionProbs,{
      pY <- input$extensionProbs[1, ] 
      # rounding errors in sum(pY) - 1
      if(min(pY)<0 | max(pY)>1 | abs(sum(pY) - 1)>(10^-8)){
        showNotification("Make sure probabilities are between 0 and 1, and sum
                         to 1.",
                         type = "error",
                         duration  = 10)
        validPy$valid <- NULL
       
      }else{
        validPy$valid <- TRUE
      }
    }
    )
    
    observeEvent(input$loadQuantiles,{
      newFile$quantiles <- FALSE
    }, priority = 1
    )
    
    observeEvent(input$loadChips,{
      newFile$chips <- FALSE
    }
    )
    
    observeEvent(input$exit, {
      stopApp(list(fit = myfit(), extensionProbs = input$extensionProbs))
    })
   
    #### render tables and plots ####
  
    output$lpquantiles <- renderTable({
      req(quantileValues(), validPy$valid)
      quantileValues()
    })
    
    output$bestFittingDistributions <- renderTable({
      req(myfit(), nExp(), input$extensionProbs, validPy$valid)
      df <- data.frame(Y = colnames(input$extensionProbs),
                       weight = input$extensionProbs[1, ],
                       bf = myfit()$best.fitting[, 1])
      
      colnames(df) <- c("Y", "weight", "best fit")
      df
    })
    
    output$barplot <- renderPlot({
      req(input$extensionProbs, validPy$valid)
      dat <- data.frame(
        value = factor(colnames(input$extensionProbs),
                       levels = colnames(input$extensionProbs)),
        probability = input$extensionProbs[1, ]
      )
      
      ggplot(data=dat, aes(x=value, y=probability)) +
        geom_bar(stat="identity", fill = "#F8776D",
                 col = "#F8776D", alpha = 0.9) +
        ylim(0, 1) +
        theme(text = element_text(size = input$fs))
    })
    
    output$condDistPlot <- renderPlot({
      req(myfit(), fq(), xlimPDF(), input$fs)
      xlimits <- xlimPDF()
      
      suppressWarnings(plotfit(myfit(), d = input$dist,
              ex = which(input$extensionVariableValue ==
                           colnames(input$extensionProbs))[1],
               ql = fq()[1], qu = fq()[2],
              xl = xlimPDF()[1], xu = xlimPDF()[2], 
              fs = input$fs))
    })
    
    output$distPlot <- renderPlot({
      req(myfit(), fq(), xlimPDF(), input$fs, validPy$valid)
      
      distVector <- rep("best", input$nY)
      
      for(i in 1:input$nY){
        distVector[i] <- eval(parse(text = paste0("input$dist", i)))
      }
      
     
      xlimits <- xlimPDF()
      

      print(makeLinearPoolPlot(myfit(), xl = xlimits[1], 
                               xu = xlimits[2], 
                               d=distVector, 
                               w = input$extensionProbs[1, ], lwd = 1,
                               xlab = "x",
                               ylab = expression(f[X](x)),
                               ql = quantileValues()[1, 2], 
                               qu = quantileValues()[2, 2],
                               addquantile = TRUE,
                               fs = input$fs,
                               expertnames = colnames(input$extensionProbs),
                               lpname = "marginal"))
                            
      
    })
    
    output$Tertiles <- renderPlot({
      req(myfit(), input$fs)
      tertilevals <- matrix(0, 3, input$nY)
      for(i in 1:input$nY){
        if(input$entry == "Quantiles"){
          tertilevals[, i] <- approx(c(0, pQuantile(), 1), 
                                     input$myvals[,  i],
                                     c(1/3, 0.5, 2/3))$y}
        if(input$entry == "Roulette"){
          tertilevals[, i] <- approx(pChip()[, i], 
                                     vChip()[, i],
                                     c(1/3, 0.5, 2/3))$y}
        
      }
      plotTertiles(tertilevals, l(), u(), fs = input$fs)
      
    })
    
    output$Quartiles <- renderPlot({
      req(myfit(), input$fs)
      quartilevals <- matrix(0, 3, input$nY)
      for(i in 1:input$nY){
        if(input$entry == "Quantiles"){
          quartilevals[, i] <- approx(c(0, pQuantile(), 1), 
                                      input$myvals[,  i],
                                      c(0.25, 0.5, 0.75))$y}
        if(input$entry == "Roulette"){
          quartilevals[, i] <- approx(pChip()[, i], 
                                      vChip()[, i],
                                      c(0.25, 0.5, 0.75))$y}
        
      }
      plotQuartiles(quartilevals, l(), u(), fs = input$fs)
      
    })
    
  
    
    #### file downloads ####
    
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
    
    output$report <- downloadHandler(
      filename = function(){switch(input$outFormat,
                                   html_document = "distributions-report.html",
                                   pdf_document = "distributions-report.pdf",
                                   word_document = "distributions-report.docx")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "elicitationShinySummaryMixture.Rmd")
        file.copy(system.file("shinyAppFiles", "elicitationShinySummaryMixture.Rmd",
                              package="SHELF"),
                  tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(fit = myfit(), 
                       entry = input$entry, 
                       chips = input$myChips,
                       probY = input$extensionProbs)
        
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
    
    output$downloadData <- downloadHandler(
      filename = "marginal-sample.csv",
      content = function(file) {
        utils::write.csv(xSample(), file, row.names = FALSE)
      }
    )
    
  })
  
 
  
  
  ## run app 
  runApp(list(ui=ui, server=server), launch.browser = TRUE)
}
