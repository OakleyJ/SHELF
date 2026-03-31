#' SHELF Elicitation
#' 
#' Opens up a web browser in which you can implement the general SHELF protocol of individual elicitation
#' followed by group discussion and elicitation of a single set of Rational Impartial Observer (RIO) probabilities
#' 
#'
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' 
#' 
#' elicitSHELF()
#' 
#' }
#' @import shiny
#' @export
elicitSHELF<- function(){
  runApp(list(
  ui = shinyUI(fluidPage(
    # code to disable mouse wheel input for numericInput
    tags$head(
      tags$script(HTML("
      $(document).on('wheel', 'input[type=number]', function (e) {
        $(this).blur();
      });
    "))
    ),
    
    # Application title
    titlePanel("SHELF elicitation: individual and group judgements"),
    
   # sidebarLayout(
  mainPanel(tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
                       ),
      
              tabsetPanel(
                tabPanel("Setup",
                         fluidRow(
                           column(width = 3, 
                                  wellPanel(
                         radioButtons("sessionType", label = NULL,
                                      c("Start new session" = "newSession",
                                        "Upload previous judgements" = "previousSession"))),

                         numericInput("fs", label = "Font size (all plots)", value = 16)
                         ),
                         column(width = 9,
                                textInput("QoI", label = h5("Quantity of Interest (QoI) definition"),
                                          value = "Text to be displayed with plots"),
                                conditionalPanel(
                                  condition = "input.sessionType == 'newSession'",
                                  numericInput("nExperts", label = h5("Number of experts"),
                                                 value = 2, min = 1),
                                  textInput("expertNames", label = h5("Expert names"),
                                            value = "A, B")),
                                conditionalPanel(
                                  condition = "input.sessionType == 'previousSession'",
                                  fileInput("loadQuantiles", label = "csv file upload", 
                                            buttonLabel = "Upload judgements"))
                                
                         )
                         )
                ),
                tabPanel("Individual judgements",
                         
                         sidebarLayout(
                           sidebarPanel(
                             wellPanel(
                               radioButtons("elicMethod", "Elicitation method",
                                            c("Quartiles" = "quartiles",
                                              "Tertiles" = "tertiles")),
                               downloadButton("saveQuantiles", "Download judgements"),
                               uiOutput("indAxis")
          
                             )),
                           mainPanel(
                             h4("Quantity of interest:"),
                             textOutput("defn"),
                             hr(),
                             uiOutput("EnterQuantiles"),
                             plotOutput("individualQuantiles"),
                             
                             )
                            
                           )
                         ),
        
                
                tabPanel("RIO judgements",
                         h4("Quantity of Interest (QoI):"),
                         textOutput("RIOJudgementsDefn"),
                         hr(),
                         wellPanel(
                           fluidRow(
                             column(4,  numericInput("nRIOprobs", "Number of probs",
                                                     value = 20)),
                             column(4, checkboxInput("generateX", "Randomly suggest X1, X2, X3 values (can be modified).",
                                                     value = FALSE))
                           ),
                           fluidRow(
                           column(4,
                          uiOutput("RIOLimits")))
                          ),
                         
                         wellPanel(
                           fluidRow(
                             
                             column(4, 
                                    helpText("Choose X1 between L and U."),
                                    uiOutput("RIOJudgementX1"),
                                    uiOutput("RIOJudgementP1"),
                                    checkboxInput("show_X2", "Elicit next probability (X2)", value = FALSE)),
                             column(4,
                                    conditionalPanel(
                                      condition = "input.show_X2 == true",
                                      helpText("Choose X2 between X1 and U."),
                                      uiOutput("RIOJudgementX2"),
                                      uiOutput("RIOJudgementP2"),
                                      checkboxInput("show_X3", "Elicit next probability (X3)", value = FALSE)
                                    )),
                             column(4,
                                    conditionalPanel(
                                      condition = "input.show_X3 == true",
                                      helpText("Choose X3 between X1 and X2."),
                                      uiOutput("RIOJudgementX3"),
                                      uiOutput("RIOJudgementP3"),
                                    )
                             )
                             
                           )
                         ),
                         plotOutput("RIOJudgementsPlot"),
                         hr(),
                         actionButton("reset", "Reset")
                ),
            
                
                tabPanel("RIO distribution",
                         sidebarLayout(
                           sidebarPanel(
                             wellPanel(
                               uiOutput("RIOdistLimits"),
                               uiOutput("RIOvalues"),
                               uiOutput("RIOprobs"),
                               selectInput("RIOdist", label = h5("QoI distribution"), 
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
                                           selected = 1),
                               hr(style = "border-top: 1px solid #000000;"),
                               textInput("RIOfq", label = h5("Feedback quantiles"), 
                                         value = "0.1, 0.9"),
                               textOutput("RIOfq"),
                               h5("Feedback probability"),
                               fluidRow(
                                 column(width = 7,
                                        "Probability QoI <= "
                                 ),
                                 column(width = 5,
                                        numericInput("RIOfeedbackProbability",  label = NULL,
                                                     value = 0.5, min = 0, max = 1)
                                 )
                                 ),
                               textOutput("RIOfp"),
         
                               hr(style = "border-top: 1px solid #000000;"),
                               uiOutput("RIOaxisLimits"),
                                     )),
                           mainPanel(
                             h4("Quantity of Interest (QoI):"),
                             textOutput("RIOdefn"),
                             hr(),
                             plotOutput("pdfPlot"),
                             plotOutput("cdfPlot")
                             )
                           )
                           ),
                tabPanel("Compare group/RIO",
                         sidebarLayout(
                           
                           sidebarPanel(
                             selectInput("LPdist", label = h5("Linear pool distribution"), 
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
                                         selected = 1),
                             selectInput("compareGroupRioPlotType", label = h5("Plot type"),
                                         choices = list(Quartiles = "quartiles",
                                                        Tertiles = "tertiles",
                                                        Density = "density",
                                                        'Median & IQR' = "MIQR",
                                                        'Median & ITR' = "MITR"))
                           ),
                           
                           mainPanel(helpText("Plots comparing the individual elicited judgements, a linear pool 
                                    obtained from the individual judgements, and the RIO distribution."),
                                     plotOutput("compareRIO"))
                         )
                         
                         
                ),
                tabPanel("Report",
                         wellPanel(
                           
                           checkboxInput("reportDistributions", "Include all fitted distributions",
                                         value = TRUE, width = NULL),
                           selectInput("outFormat", label = "Report format",
                                       choices = list('html' = "html_document",
                                                      'pdf' = "pdf_document",
                                                      'Word' = "word_document"),
                                       width = "30%"),
                           downloadButton("report", "Download report")
                           
                         )
                )
                         )
                )
            )
  ),
   
  server = function(input, output, session) {
    
    # Setup tab ----
    
  
    
    

    # Individual judgements tab ----
    
    newFile <- reactiveValues(quantiles = TRUE)
    
    observeEvent(input$loadQuantiles,{
      newFile$quantiles <- FALSE
    }, priority = 1
    )
    
    initialVals <- reactive({
      
      inFile <- input$loadQuantiles
     # if (is.null(inFile) | isolate(newFile$quantiles)){
      if(input$sessionType == "newSession"){
        initialdf <- matrix(" ", nrow = 5, ncol = input$nExperts
        )
        if(input$elicMethod == "quartiles"){
          rownames(initialdf) <- c("L", "Q1", "M", "Q3", "U")
        }
        if(input$elicMethod == "tertiles"){
          rownames(initialdf) <- c("L", "T1", "M", "T2", "U")
        }
        expertNames <- strsplit(input$expertNames, split = "\\s*,\\s*")[[1]]
        if(length(expertNames)!=input$nExperts){
          showNotification(
            "Check there is one name per expert (comma separated) on the setup tab.",
            duration = 60,
            type = "error"
          )
          return(NULL)}else{
        colnames(initialdf) <- expertNames
        return(initialdf)}
      }
        
      if(input$sessionType == "previousSession"){
      req(inFile$datapath)
        initialdf <- as.matrix(utils::read.csv(inFile$datapath, row.names = 1))
        if(rownames(initialdf)[2] == "T1"){
          updateRadioButtons(session,
                            "elicMethod",
                            selected = "tertiles")}
        if(rownames(initialdf)[2] == "Q1"){
          updateRadioButtons(session,
                             "elicMethod",
                             selected = "quartiles")}
        
        newFile$quantiles <- TRUE
        # if(nrow(initialdf) != 5 ){
        #   showNotification("The input file doesn't have the right number of rows. The .csv file should have 
        #                    one row of expert names, and five rows of judgements.", 
        #                    type = "error",
        #                    duration = 60)
        #   initialdf <- matrix(" ", nrow = 5, ncol = input$nExperts
        #   )
        #   if(input$elicMethod == "quartiles"){
        #     rownames(initialdf) <- c("L", "Q1", "M", "Q3", "U")
        #   }
        #   if(input$elicMethod == "tertiles"){
        #     rownames(initialdf) <- c("L", "T1", "M", "T2", "U")
        #   }
        #   expertNames <- strsplit(input$expertNames, split = "\\s*,\\s*")[[1]]
        #   colnames(initialdf) <- expertNames
        #   return(initialdf)
        # }else{
        #   return(initialdf)
        # }
        return(initialdf)
      }
      
      
    })

    
    
    indAxis <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$indAxis, ")"))),
               error = function(e){NULL})
    })
    
    output$defn <- renderText({input$QoI
      
    })
    
    output$EnterQuantiles <- renderUI({
      req(initialVals())
      en <- input$sessionType == "previousSession"
        shinyMatrix::matrixInput(
          inputId = "myQuantiles", value =  initialVals(),
                                 class = "numeric",
                                 cols = list(names = TRUE, 
                                             editableNames = en),
                                 rows = list(names = TRUE))
    })
    
    observe({
      req(input$myQuantiles)
      current_matrix <- input$myQuantiles
      
      if(input$sessionType == "newSession"){
        req(input$expertNames)
        expertNames <- strsplit(input$expertNames, split = "\\s*,\\s*")[[1]]
        if (length(expertNames) == ncol(current_matrix)) {
          colnames(current_matrix) <- expertNames
        }
      }
      
      if(input$elicMethod == "quartiles"){
        rownames(current_matrix) <- c("L", "Q1", "M", "Q3", "U")
      }
      if(input$elicMethod == "tertiles"){
        rownames(current_matrix) <- c("L", "T1", "M", "T3", "U")
      }
      
     
        shinyMatrix::updateMatrixInput(session,
                                       "myQuantiles",
                                       value = current_matrix)
      
    })

    
    output$individualQuantiles <- renderPlot({
      req(indAxis(), input$myQuantiles)
      p1 <- NULL

      if(checkPlot(input$myQuantiles)== TRUE){
        if(input$elicMethod == "quartiles"){
          p1 <- plotQuartiles(vals = input$myQuantiles[2:4, ],
                              lower = input$myQuantiles[1, ],
                              upper = input$myQuantiles[5, ],
                              expertnames = colnames(input$myQuantiles))
        }
        if(input$elicMethod == "tertiles"){
          p1 <- plotTertiles(vals = input$myQuantiles[2:4, ],
                              lower = input$myQuantiles[1, ],
                              upper = input$myQuantiles[5, ],
                              expertnames = colnames(input$myQuantiles))
        }
      }
      
      print(p1 + theme_bw(base_size = input$fs)+
              xlim(indAxis()[1], indAxis()[2]))
    })  
    
    output$saveQuantiles <- downloadHandler(
      filename = function() {
        paste('judgements-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        df1 <- input$myQuantiles
        
        utils::write.csv(input$myQuantiles, file)
      }
    )
    
    output$indAxis <- renderUI({
      req(input$myQuantiles)
      textInput("indAxis", label = h5("Axis limits"), 
                value = paste0(c(min(input$myQuantiles),max(input$myQuantiles)),
                               collapse = ", "))
    })
    
    
    
    
    
    # RIO judgements tab ----
    
    
    output$RIOJudgementsDefn <- renderText({input$QoI
    })
    
    output$RIOLimits <- renderUI({
      textInput("RIOLimits", label = h5("Plausible Limits"), 
                value = paste0(c(min(input$myQuantiles), max(input$myQuantiles)), collapse = ", "))
    })

    RIOLimits <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$RIOLimits, ")"))),
               error = function(e){NULL})
    })
    
    lpfit <-reactive({
      req(input$myQuantiles)
      if(input$elicMethod == "quartiles"){p <- c(0.25, 0.5, 0.75)}else{
        p <- c(0.33, 0.5, 0.66)
      }
     fitdist(vals = input$myQuantiles[2:4, ],
                       lower = input$myQuantiles[1, ],
                       upper = input$myQuantiles[5, ],
                       probs = p,
             expertnames = colnames(input$myQuantiles))
    })
    
    XDefaults <-reactive({
      if(input$generateX && !is.null(input$myQuantiles)){
        if(input$elicMethod == "quartiles"){p <- c(0.25, 0.5, 0.75)}else{
          p <- c(0.33, 0.5, 0.66)
        }
        
        return(signif(qlinearpool(lpfit(),
                                  q=c(runif(1, 0.2, 0.3),
                                      runif(1, 0.4, 0.6),
                                      runif(1, 0.7, 0.8)),
                                  d = "beta"),
                      2))
        
      }else{
        return(c(NA, NA, NA))
      }
    })
    
    
    
    output$RIOJudgementX1 <- renderUI({

        numericInput("RIOX1", label = h5("Value X1"), 
                  value = XDefaults()[1])
      
    })
    output$RIOJudgementP1 <- renderUI({
      
        numericInput("RIOP1", label = h5("P(X<= X1)"), 
                     value = NA, min = 0, max = 1, step = 1/input$nRIOprobs)
      
    })
    
    output$RIOJudgementX2 <- renderUI({
        numericInput("RIOX2", label = h5("Value X2"), 
                     value = XDefaults()[3])
    })
  
    output$RIOJudgementP2<- renderUI({
        numericInput("RIOP2", label = h5("P(X>= X2)"), 
                     value = NA, min = 0, max = 1, step = 1/input$nRIOprobs)
    })
    
    output$RIOJudgementX3<- renderUI({
        numericInput("RIOX3", label = h5("Value X3"), 
                     value = XDefaults()[2])
    })
    output$RIOJudgementP3 <- renderUI({
        numericInput("RIOP3", label = h5("P(X1 <= X<= X3)"), 
                     value = NA, min = 0, max = 1, step = 1/input$nRIOprobs)
    })
    
    
    output$RIOJudgementsPlot <- renderPlot({
      req(RIOLimits(), input$nRIOprobs)
      
     
     RIOJudgementsPlot(L = RIOLimits()[1],
                              U = RIOLimits()[2],
                              nRIOprobs = input$nRIOprobs,
                              X1 = input$RIOX1,
                              X2 = input$RIOX2,
                              X3 = input$RIOX3,
                              P1 = input$RIOP1,
                              P2 = input$RIOP2,
                              P3 = input$RIOP3,
                              show_X2 = input$show_X2,
                              show_X3 = input$show_X3,
                              fs = input$fs)
      })

  
    
   # RIO distribution tab ----
    
  
    
    output$RIOdefn <- renderText({input$QoI
    })
    
    
    RIODefaults <-reactive({
      v <- c(input$RIOX1, input$RIOX3, input$RIOX2)
      p <- c(input$RIOP1,
             input$RIOP1 + input$RIOP3,
             1 - input$RIOP2)
      l <- RIOLimits()
      if(anyNA(c(v, p, l))){
        return(c(NA, NA, NA))
      }else{
        return(c(paste(v, collapse = ", "),
                 paste(p, collapse = ", "),
                 paste(l, collapse = ", ")))
      }
    })
  
    output$RIOdistLimits <- renderUI({
      textInput("ParameterLimits", label = h5("Parameter Limits"), 
                value = RIODefaults()[3])
    })
    
    output$RIOvalues <- renderUI({
        textInput("values", label = h5("QoI values"), 
                  value = RIODefaults()[1])
    })
    

    output$RIOprobs <- renderUI({
        textInput("probs", label = h5("QoI cumulative probabilities"), 
                  value = RIODefaults()[2])
    })
    
   
    
 
    
    # Hack to avoid CRAN check NOTE
    
    X1 <- X2 <- xpos <- ypos <- hjustvar <- vjustvar <- annotateText <- NULL
    
    
    p <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$probs, ")"))),
               error = function(e){NULL})
    })

    v <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$values, ")"))),
               error = function(e){NULL})
    })
    
    limits <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$ParameterLimits, ")"))),
               error = function(e){NULL})
    })
    

    
    fq <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$RIOfq, ")"))),
               error = function(e){NULL})
      
    })
    
    
    output$RIOaxisLimits <- renderUI({
      textInput("xaxisLimits", label = h5("x-axis limits"), 
                value = RIODefaults()[3])
    })
    
    xaxis <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xaxisLimits, ")"))),
               error = function(e){NULL})
    })
    
   
    
    myfit <- reactive({
      
      req(v(), p(), limits())
      
      check <- checkJudgementsValid(probs = p(), vals = v(),
                                    tdf = 10,
                                    lower = limits()[1],
                                    upper= limits()[2])
      if(check$valid == TRUE){
      
        fitdist(vals = v(), probs = p(), lower = limits()[1],
              upper = limits()[2], 
              tdf = 10)
      }
    })
    
   
 
    
    # RIO pdf plots ----
    
  
    
    output$pdfPlot <- renderPlot({
      req(myfit(), fq(), xaxis())
      plotfit(myfit(), d = input$RIOdist,
              ql = fq()[1], qu = fq()[2],
              xl = xaxis()[1], xu = xaxis()[2], 
              fs = input$fs,
              returnPlot = FALSE)
      
    })
    
    # RIO cdf plots ----
    
 
    
    output$cdfPlot <- renderPlot({
      req(myfit(), v(), p(), input$RIOdist, limits())
      makeCDFPlot(lower = limits()[1], 
                  v = v(),
                  p = p(),
                  upper = limits()[2],
                  fit = myfit(),
                  dist = input$RIOdist,
                  showFittedCDF = TRUE,
                  fontsize = input$fs,
                  xaxisLower = xaxis()[1], xaxisUpper = xaxis()[2])
      
    })
    
    quantileValues <- reactive({
      req(fq(), myfit())
     
      if(min(fq())<=0 | max(fq())>=1){
        return(NULL)
      }else{
        
        FB <- feedback(myfit(), 
                       quantiles = fq(),
                       ex = 1)
        
        if(input$RIOdist == "best"){
          values <- FB$fitted.quantiles[, 
                                        as.character(myfit()$best.fitting[1,
                                                                          1])]
        }else{
          values <- FB$fitted.quantiles[, input$RIOdist]
          
        }
        
        return(values)
      }
      
    }) 
    
    probabilityValues <- reactive({
      
      req(myfit(),
          input$RIOfeedbackProbability)
  
      
      FB <- feedback(myfit(), 
                     values = input$RIOfeedbackProbability,
                     ex = 1)
      
      if(input$RIOdist == "best"){
        probs <- FB$fitted.probabilities[, 
                                         as.character(myfit()$best.fitting[1,
                                                                           1])]
      }else{
        probs <- FB$fitted.probabilities[, input$RIOdist]
        
      }
      
      return(probs)
      
      
    }) 
    
    # ...and display on the PDF tab...
    output$RIOfq <- renderText({
      req(quantileValues())
      paste0("Fitted quantiles: ", quantileValues()[1], ", ", quantileValues()[2])
    })
    output$RIOfp <- renderText({
      req(probabilityValues())
      #paste0("P(QoI <= ", input$RIOfeedbackProbability,") = ", probabilityValues())
      paste0(" = ", probabilityValues())
    })
    
    # Compare group/RIO tab ----
    
    output$compareRIO <- renderPlot({
      req(lpfit(), myfit())
      compareGroupRIO(lpfit(), myfit(),
                      type = input$compareGroupRioPlotType,
                      dLP = input$LPdist,
                      dRIO = input$RIOdist,
                      fs = input$fs)
      
    })
   
    # Report tab ----
    
    output$report <- downloadHandler(
      filename = function(){switch(input$outFormat,
                                   html_document = "extrapolation-report.html",
                                   pdf_document = "extrapolation-report.pdf",
                                   word_document = "extrapolation-report.docx")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "elicitationShinySummarySHELF.Rmd")
        file.copy(system.file("shinyAppFiles", "elicitationShinySummarySHELF.Rmd",
                              package="SHELF"),
                  tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        
       
        params <- list(QoI = input$QoI,
                       myfit = myfit(), myQuantiles = input$myQuantiles,
                       reportDistributions = input$reportDistributions,
                       dist = input$RIOdist,
                       elicMethod = input$elicMethod,
                       compareGroupRioPlotType = input$compareGroupRioPlotType,
                       LPdist = input$LPdist,
                       xLimits = xaxis()
                       )
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          output_format = input$outFormat,
                          output_options = list(self_contained = TRUE),
                          envir = new.env(parent = globalenv())
        )
      }
    )
 
  }
  ), launch.browser = TRUE)
}


# Helper function check quartiles can be plotted ----


checkPlot <- function(myvals){
  if(anyNA(myvals)){
    return(FALSE)
  }
  if(any(diff(myvals)<=0)){
    return(FALSE)
    }
  return(TRUE)
}