#' Elicitation for survival extrapolation
#' 
#' Opens up a web browser in which you can implement the SHELF protocol for survival extrapolation.
#' Start with uploading a .csv file of individual patient survival data (time, event to indicate censoring, and
#' treatment group). Then elicit individual judgements, perform scenario testing as required, and elicit a RIO distribution.
#' Judgements for two treatment groups can be elicited in the same session.
#' 
#'
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' 
#' # make a suitable csv file using a built in data set from the survival package
#' sdf <- survival::veteran[, c("time", "status", "trt")]
#' colnames(sdf) <- c("time", "event", "treatment")
#' sdf$treatment <- factor(sdf$treatment, labels = c("standard", "test"))
#' 
#' # write the data frame sdf to a .csv file in the current working directory
#' write.csv(sdf, file = "testFile.csv", row.names = FALSE)
#' 
#' # Run the app and upload testFile.csv in the first tab, and change unit of time to "days"
#' 
#' elicitSurvivalExtrapolation()
#' 
#' }
#' @import shiny
#' @import survival
#' @importFrom survminer ggsurvplot
#' @export
elicitSurvivalExtrapolation<- function(){
  runApp(list(
  ui = shinyUI(fluidPage(
    
    # Application title
    titlePanel("SHELF: survival extrapolation"),
    
   # sidebarLayout(
  mainPanel(tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
                       ),
      
              tabsetPanel(
                tabPanel("Upload data",
                         
                         sidebarLayout(
                           sidebarPanel(
                             wellPanel(
                               fileInput("file", label = "Survival data upload"),
                               uiOutput("targetTime"),
                               uiOutput("truncationTime"),
                               uiOutput("timeUnit"),
                               uiOutput("fontSize")
                              
                         
                         )
                           )
                         ,
                         mainPanel(
                           helpText('Upload a .csv file with three columns:
                                    "time", "event" and "treatment" (in that order).
                                    Values in the "event" column should be 0 for
                                    a censored observation, and 1 otherwise. The
                                    "treatment" column should be included even 
                                    if there is only one treatment group.'),
                           plotOutput("KMbasic"),
                           tableOutput("survivalSummary"),
                         
                         )
                         )
                         ),
                tabPanel("KM table",
                         
                         sidebarLayout(
                           sidebarPanel(
                             wellPanel(
                               uiOutput("breakTime"),
                               
                             )),
                           mainPanel(
                             helpText('The survivor column is the proportion surviving
                                      after the end of the corresponding time interval. The hazard
                                      column is the proportion who do not survive to the
                                      end of the time interval, out of those who survived to the
                                      beginning of the time interval'),
                             tableOutput("KMdata")
                           )
                         )
                ),
                tabPanel("Individual judgements",
                         
                         sidebarLayout(
                           sidebarPanel(
                             wellPanel(
                               uiOutput("treatmentGroup"),
                               numericInput("nExperts", label = h5("Number of experts"),
                                            value = 2, min = 1),
                               radioButtons("elicMethod", "Elicitation method",
                                            c("Quartiles" = "quartiles",
                                              "Tertiles" = "tertiles")),
                               textInput("indAxis", label = h5("Axis limits"), 
                                         value = "0, 1"),
                               hr(style = "border-top: 1px solid #000000;"),
                               h5("Scenario testing"),
                               helpText("Use next tab before displaying here"),
                               checkboxInput("showScenario", 
                                            label = h5("Show scenario interval"))
                               
                             )),
                           mainPanel(
                             h4("Quantity of interest:"),
                             textOutput("defn"),
                             hr(),
                             helpText('Enter plausible limits and quantiles as values
                                      between 0 and 1.'),
                             uiOutput("EnterQuartilesGp1"),
                             uiOutput("EnterQuartilesGp2"),
                             plotOutput("individualQuartiles")
                             )
                            
                           )
                         ),
                tabPanel("Scenario testing",
                         
                         sidebarLayout(
                           sidebarPanel(
                             wellPanel(
                               uiOutput("scenarioGroup"),
                               uiOutput("scenarioTruncationTime"),
                               uiOutput("expRange")
                               
                             )),
                           mainPanel(
                             helpText('In this tab you can consider a scenario in which the hazard
                                      remains constant after a specified time point. The shaded
                                      bar indicates a point-wise approximate 95% credible interval for 
                                      the survivor function based on this assumption. Intervals can be
                                      added to the "Individual judgements" tab.'),
                             plotOutput("KMplot"),
                             htmlOutput("reportInterval")
                           )
                         )
                ),
                
                tabPanel("RIO elicitation",
                         sidebarLayout(
                           sidebarPanel(
                             wellPanel(
                               uiOutput("RIOtreatmentGroup"),
                               uiOutput("RIOvaluesGp1"),
                               uiOutput("RIOprobsGp1"),
                               uiOutput("RIOvaluesGp2"),
                               uiOutput("RIOprobsGp2"),
                               uiOutput("RIOdist1"),
                               uiOutput("RIOdist2"),
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
                               uiOutput("xaxis1"),
                               uiOutput("xaxis2")
                               
                               
                             
                               
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
                
                
                
                tabPanel("Extrapolation plot",
                         plotOutput("extrapolationPlot")
                         
                ),
                tabPanel("Joint distribution",
                         uiOutput("bivariate")
                         
                ),
                tabPanel("Report",
                         wellPanel(
                           h5("Report content"),
                          
                         checkboxInput("reportData", "KM plot and summary data",
                                       value = TRUE, width = NULL),
                         checkboxInput("reportTable", "Survivor and hazard table",
                                       value = TRUE, width = NULL),
                         uiOutput("reportGroup1"),
                         uiOutput("reportGroup2"),
                         checkboxInput("reportIndividual", "Individual judgements",
                                       value = TRUE, width = NULL),
                         checkboxInput("reportScenarioTest", "Scenario Test results",
                                       value = TRUE, width = NULL),
                         checkboxInput("reportExtrapolation", "Plot extrapolation",
                                       value = TRUE, width = NULL),
                         checkboxInput("reportDistributions", "All fitted distributions",
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
   
  server = function(input, output) {
    
    # File uploading ----
    
    survivalDF <- reactive({
      req(input$file)
      inFile <- input$file
      sdf <- utils::read.csv(inFile$datapath)
     
      colnames(sdf) <- c("time", "event", "treatment")
      sdf$treatment <- as.factor(sdf$treatment)
      
      if(length(levels(sdf$treatment)) > 2){
        showNotification(
          "This app will only with work with one or two treatment groups.",
          duration = 60,
          type = "error"
        )
        sdf <- NULL
      }
    
     
      sdf
    })
    
    
    output$KMbasic <- renderPlot({
      req(survivalDF(),input$targetTime, input$truncationTime)
      
      # Truncate data frame for plotting
      index <- survivalDF()$time > input$truncationTime
      truncatedDf <- survivalDF()
      truncatedDf$event[index] <- 0
      truncatedDf$time[index] <- input$truncationTime
      
     
      fit <- survival::survfit(survival::Surv(time, event) ~ treatment, data = truncatedDf)
      myplot<- survminer::ggsurvplot(fit, data = truncatedDf, censor = TRUE,
                                      legend = "right",
                                      legend.title = "",
                            conf.int = TRUE,
                            legend.labs = levels(truncatedDf$treatment),
                            xlim = c(0, input$targetTime),
                            xlab = paste0("Time (", input$timeUnit, ")"),
                            break.time.by = input$targetTime/8)
      myplot$plot +
        geom_vline(xintercept = input$targetTime, linetype="dotted") +
        theme_bw(base_size = input$fs)
    })
    
    output$survivalSummary <-renderTable({
      req(survivalDF())
      makeSurvSummaryTable(survivalDF())}, 
      rownames = TRUE)
    
    output$targetTime <- renderUI({
      req(survivalDF())
      maxTime <- signif(max(survivalDF()$time), 1)
      numericInput("targetTime", "Target extrapolation time", value = 2* maxTime)
    })
    
    output$truncationTime <- renderUI({
      req(survivalDF())
      maxTime <- signif(max(survivalDF()$time), 1)
      numericInput("truncationTime", "Data truncation time", value = 2* maxTime)
    })
    
    output$timeUnit <- renderUI({
      req(survivalDF())
      selectInput("timeUnit", "Unit of time", 
                  c("days", "weeks", "months", "years"),
                  selected = "years")
    })
    
    output$fontSize <- renderUI({
      req(survivalDF())
      numericInput("fs", label = "Font size (all plots)", value = 16)
    })
    
    # KM table tab ----
    
    
    
    
    output$breakTime <- renderUI({
      req(survivalDF(), input$timeUnit)
      maxTime <- min(max(survivalDF()$time), input$truncationTime)
      numericInput("breakTime", paste0("Time interval duration (",
                                       input$timeUnit, ")"),
                   value = signif(maxTime/4, 1))
    })
    
    output$KMdata <- renderTable({
      req(survivalDF(), input$breakTime,
          input$truncationTime)
      makeSurvivalTable(survivalDF(), input$breakTime,
                        input$truncationTime, input$timeUnit, dp = 2)
      
    })
    
    # Scenario testing tab ----
    
    
    
    output$scenarioGroup <- renderUI({
      selectInput("scenarioGroup", "Treatment group", 
                  choices = levels(survivalDF()$treatment))
    })
    
    output$expRange <- renderUI({
      req(survivalDF())
      maxTime <- min(max(survivalDF()$time), input$truncationTime)
      textInput("expRange", label = h5("Constant hazard fitting period"), 
                value = paste0(0, ", ", signif(maxTime, 1) ))
      })
    
    expRange <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$expRange, ")"))),
               error = function(e){NULL})
    })
    
    
    output$KMplot <- renderPlot({
      req(survivalDF(), expRange(), input$scenarioGroup,
          input$targetTime, 
          input$truncationTime)
      scenario <- survivalScenario(tLower = 0,
                                tUpper = min(input$truncationTime,
                                             max(survivalDF()$time)),
                                expLower= expRange()[1],
                                expUpper = expRange()[2],
                                expGroup = input$scenarioGroup,
                                tTarget = input$targetTime,
                                survDf = survivalDF(),
                    groups = levels(survivalDF()$treatment),
                    xl = paste0("Time (", input$timeUnit, ")"),
                    showPlot = FALSE,
                                fontsize = input$fs)
      scenario$KMplot
     
    })
    
    
    scenarioIntervalGp1 <- reactive({ 
      req(survivalDF(), expRange(),input$scenarioGroup,
          input$targetTime,
          input$truncationTime)
      scenarioTestInterval(tLower = 0,
                                       tUpper = input$truncationTime,
                                       expLower = expRange()[1],
                                       expUpper = expRange()[2],
                                       expGroup = levels(survivalDF()$treatment)[1],
                                       tTarget = input$targetTime,
                                       survDf = survivalDF())
      })
    
    scenarioIntervalGp2 <- reactive({ 
      req(survivalDF(), expRange(),input$scenarioGroup,
          input$targetTime,
          input$truncationTime)
      interval <- NULL
      if(length(levels(survivalDF()$treatment)) > 1){
      interval <- scenarioTestInterval(tLower = 0,
                           tUpper = input$truncationTime,
                           expLower = expRange()[1],
                           expUpper = expRange()[2],
                           expGroup = levels(survivalDF()$treatment)[2],
                           tTarget = input$targetTime,
                           survDf = survivalDF())
      }
      interval
    })
    
    output$reportInterval <- renderUI({

      req(input$scenarioGroup, survivalDF())
      txt <- NULL
      if(input$scenarioGroup == levels(survivalDF()$treatment)[1]){
        req(scenarioIntervalGp1())
        txt <- paste0("(", signif(scenarioIntervalGp1()[1], 2)
               , ", ", signif(scenarioIntervalGp1()[2], 2), ")" )
      }
      
      if(input$scenarioGroup != levels(survivalDF()$treatment)[1]){
        req(scenarioIntervalGp2())
        txt <- paste0("(", signif(scenarioIntervalGp2()[1], 2), ", ",
               signif(scenarioIntervalGp2()[2], 2), ")" )
      }
      req(txt)
      HTML(paste(hr(),
                 h5(paste0("Approximate 95% credible interval at the target time: ",
                           txt))))
      
      })
    
    # Individual judgements tab ----
    
    initialVals1 <- reactive({
      initialdf <- matrix(0.1*(1:5), nrow = 5, ncol = input$nExperts
                         )
      if(input$elicMethod == "quartiles"){
      rownames(initialdf) <- c("L", "Q1", "M", "Q3", "U")
      }
      if(input$elicMethod == "tertiles"){
        rownames(initialdf) <- c("L", "T1", "M", "T2", "U")
      }
      colnames(initialdf) <- LETTERS[1:input$nExperts]
      initialdf
    })
    
    initialVals2 <- reactive({
      initialdf <- matrix(0.1*(1:5), nrow = 5, ncol = input$nExperts
      )
      if(input$elicMethod == "quartiles"){
        rownames(initialdf) <- c("L", "Q1", "M", "Q3", "U")
      }
      if(input$elicMethod == "tertiles"){
        rownames(initialdf) <- c("L", "T1", "M", "T2", "U")
      }
      colnames(initialdf) <- LETTERS[1:input$nExperts]
      initialdf
    })
    
    output$treatmentGroup <- renderUI({
      selectInput("treatmentGroup", "Treatment group", 
                  choices = levels(survivalDF()$treatment))
    })
    
    output$tgpNumber <- reactive({
      which(input$treatmentGroup == levels(survivalDF()$treatment)) 
    })
    
    outputOptions(output, 'tgpNumber', suspendWhenHidden = FALSE)
    
    indAxis <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$indAxis, ")"))),
               error = function(e){NULL})
    })
    
    output$defn <- renderText({
      paste0("Proportion surviving for at least ", input$targetTime,
             " ", input$timeUnit, 
             ' in the treatment group "', 
             input$treatmentGroup,'".')
    })
    
    output$EnterQuartilesGp1 <- renderUI({

      conditionalPanel(
        condition = "output.tgpNumber == 1",
        shinyMatrix::matrixInput(
          inputId = "myvals1", value =  initialVals1(),
                                 class = "numeric",
                                 cols = list(names = TRUE,
                                             editableNames = TRUE),
                                 rows = list(names = TRUE))
      )
      
    })
    
    output$EnterQuartilesGp2 <- renderUI({

      conditionalPanel(
        condition = "output.tgpNumber == 2",
        shinyMatrix::matrixInput(inputId = "myvals2", value =  initialVals2(),
                                 class = "numeric",
                                 cols = list(names = TRUE,
                                             editableNames = TRUE),
                                 rows = list(names = TRUE))
      )
      
    })
    
    output$individualQuartiles <- renderPlot({
      req(input$treatmentGroup, survivalDF()$treatment, 
          indAxis())
      p1 <- NULL
   
      if(input$treatmentGroup == levels(survivalDF()$treatment)[1]){
        req(input$myvals1)
      if(checkPlot(input$myvals1)== TRUE){
        if(input$elicMethod == "quartiles"){
          p1 <- plotQuartiles(vals = input$myvals1[2:4, ],
                              lower = input$myvals1[1, ],
                              upper = input$myvals1[5, ],
                              expertnames = colnames(input$myvals1),
                              xlabel = "Surivivor proportion")
        }
        if(input$elicMethod == "tertiles"){
          p1 <- plotTertiles(vals = input$myvals1[2:4, ],
                              lower = input$myvals1[1, ],
                              upper = input$myvals1[5, ],
                              expertnames = colnames(input$myvals1),
                             xlabel = "Surivivor proportion")
        }
        
       
      }}
      
      
      if(input$treatmentGroup != levels(survivalDF()$treatment)[1]){
        req(input$myvals2)
        if(checkPlot(input$myvals2)== TRUE){
          if(input$elicMethod == "quartiles"){
         p1 <- plotQuartiles(vals = input$myvals2[2:4, ],
                        lower = input$myvals2[1, ],
                        upper = input$myvals2[5, ],
                        expertnames = colnames(input$myvals2),
                        xlabel = "Surivivor proportion")
          }
          if(input$elicMethod == "tertiles"){
            p1 <- plotTertiles(vals = input$myvals2[2:4, ],
                                lower = input$myvals2[1, ],
                                upper = input$myvals2[5, ],
                                expertnames = colnames(input$myvals2),
                               xlabel = "Surivivor proportion")
          }
         
        }
      }
      req(p1)
      
      if(input$showScenario == TRUE){

        if(input$treatmentGroup == levels(survivalDF()$treatment)[1]){
          interval1 <- tryCatch(eval(scenarioIntervalGp1()), error = function(e){NULL})
          if(!is.null(interval1)){
        p1 <- p1 + geom_vline(xintercept = scenarioIntervalGp1(), 
                              linetype = "dashed")
          }
        }
        if(input$treatmentGroup != levels(survivalDF()$treatment)[1]){
          interval2 <- tryCatch(eval(scenarioIntervalGp1()), error = function(e){NULL})
          if(!is.null(interval2)){
          p1 <- p1 + geom_vline(xintercept = scenarioIntervalGp2(), 
                                linetype = "dashed")
          }
        }
        
      }
      
      print(p1 + theme_bw(base_size = input$fs)+
              xlim(indAxis()[1], indAxis()[2]) + coord_flip())
    })  
    
    
      

        
   # RIO tab ----
    
  
    
    output$RIOdefn <- renderText({
      paste0("Proportion surviving for at least ", input$targetTime,
             " ", input$timeUnit, 
             ' in the treatment group "', 
             input$RIOtreatmentGroup,'".')
    })
    
    output$RIOtreatmentGroup <- renderUI({
      selectInput("RIOtreatmentGroup", "Treatment group", 
                  choices = levels(survivalDF()$treatment))
    })
    
    output$RIOtgpNumber <- reactive({
      which(input$RIOtreatmentGroup == levels(survivalDF()$treatment)) 
    })
    
    outputOptions(output, 'RIOtgpNumber', suspendWhenHidden = FALSE)
    
    output$RIOvaluesGp1 <- renderUI({
      conditionalPanel(
        condition = "output.RIOtgpNumber == 1",
        textInput("values1", label = h5("QoI values"), 
                  value = "0.2, 0.3, 0.4")
      )
    })
    
    output$xaxis1 <- renderUI({
      conditionalPanel(
        condition = "output.RIOtgpNumber == 1",
        textInput("xaxis1", label = h5(paste0("x-axis limits (",
                                              levels(survivalDF()$treatment)[1], ")")), 
                  value = "0, 1")
      )
    })
    
    output$xaxis2 <- renderUI({
      conditionalPanel(
        condition = "output.RIOtgpNumber == 2",
        textInput("xaxis2", label = h5(paste0("x-axis limits (",
                                              levels(survivalDF()$treatment)[2], ")")), 
                  value = "0, 1")
      )
    })
  
    
    output$RIOprobsGp1 <- renderUI({
      conditionalPanel(
        condition = "output.RIOtgpNumber == 1",
        textInput("probs1", label = h5("QoI cumulative probabilities"), 
                  value = "0.25, 0.5, 0.75")
      )
    })
    
    output$RIOvaluesGp2 <- renderUI({
      conditionalPanel(
        condition = "output.RIOtgpNumber == 2",
        textInput("values2", label = h5("QoI values"), 
                  value = "0.5, 0.6, 0.7")
      )
    })
    
    output$RIOprobsGp2 <- renderUI({
      conditionalPanel(
        condition = "output.RIOtgpNumber == 2",
        textInput("probs2", label = h5("QoI cumulative probabilities"), 
                  value = "0.25, 0.5, 0.75")
      )
    })
    
    output$RIOdist1 <- renderUI({
      
      conditionalPanel(
        condition = "output.RIOtgpNumber == 1",
        selectInput("RIOdist1", label = h5("QoI distribution"), 
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
                         selected = 1)
      )
    })
    
    output$RIOdist2 <- renderUI({
      
      conditionalPanel(
        condition = "output.RIOtgpNumber == 2",
        selectInput("RIOdist2", label = h5("Distribution"), 
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
                    selected = 1)
      )
    })
    
    
    
    # Hack to avoid CRAN check NOTE
    
    X1 <- X2 <- xpos <- ypos <- hjustvar <- vjustvar <- annotateText <- NULL
    
    
    p1 <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$probs1, ")"))),
               error = function(e){NULL})
    })
    
    p2 <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$probs2, ")"))),
               error = function(e){NULL})
    })
    
    v1 <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$values1, ")"))),
               error = function(e){NULL})
    })
    
    v2 <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$values2, ")"))),
               error = function(e){NULL})
    })
    
    fq <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$RIOfq, ")"))),
               error = function(e){NULL})
      
    })
    
    
    xaxis1 <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xaxis1, ")"))),
               error = function(e){NULL})
    })
    
    xaxis2 <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$xaxis2, ")"))),
               error = function(e){NULL})
    })
    
    myfit1 <- reactive({
      
      req(v1(), p1())
      
      check <- checkJudgementsValid(probs = p1(), vals = v1(),
                                    tdf = 10,
                                    lower = 0,
                                    upper= 1)
      if(check$valid == TRUE){
      
        fitdist(vals = v1(), probs = p1(), lower = 0,
              upper = 1, 
              tdf = 10)
      }
    })
    
    myfit2 <- reactive({
      req( v2(), p2())
      
      check <- checkJudgementsValid(probs = p2(), vals = v2(),
                                    tdf = 10,
                                    lower = 0,
                                    upper= 1)
      if(check$valid == TRUE){
      fitdist(vals = v2(), probs = p2(), lower = 0,
              upper = 1, 
              tdf = 10)
      }
    })
    
    
    
    # RIO pdf plots ----
    
    pdfPlot1 <- reactive({
      req(myfit1())
      plotfit(myfit1(), d = input$RIOdist1,
              ql = fq()[1], qu = fq()[2],
              xl = xaxis1()[1], xu = xaxis1()[2], 
              fs = input$fs,
              returnPlot = TRUE,
              showPlot = FALSE)
    })
    
    pdfPlot2 <- reactive({
      req(myfit2())
      plotfit(myfit2(), d = input$RIOdist2,
              ql = fq()[1], qu = fq()[2],
              xl = xaxis2()[1], xu = xaxis2()[2], 
              fs = input$fs,
              returnPlot = TRUE,
              showPlot = FALSE)
    })
    
    
    
    output$pdfPlot <- renderPlot({
      req(survivalDF(), input$RIOtreatmentGroup)

      
      if(input$RIOtreatmentGroup == levels(survivalDF()$treatment)[1]){
        req(pdfPlot1())
        print(pdfPlot1())
      }
      
      if(input$RIOtreatmentGroup != levels(survivalDF()$treatment)[1]){
        req(pdfPlot2())
        print(pdfPlot2())
      }
      
  
      
    })
    
    # RIO cdf plots ----
    
    cdfPlot1 <- reactive({
      req(myfit1(), v1(), p1(), input$RIOdist1)
      makeCDFPlot(lower = 0, 
                  v = v1(),
                  p = p1(),
                  upper = 1,
                  fit = myfit1(),
                  dist = input$RIOdist1,
                  showFittedCDF = TRUE,
                  fontsize = input$fs,
                  xaxisLower = xaxis1()[1], xaxisUpper = xaxis1()[2])
    })
    
    cdfPlot2 <- reactive({
      req(myfit2(), v2(), p2())
      makeCDFPlot(lower = 0, 
                  v = v2(),
                  p = p2(),
                  upper = 1,
                  fit = myfit2(),
                  dist = input$RIOdist2,
                  showFittedCDF = TRUE,
                  fontsize = input$fs,
                  xaxisLower = xaxis2()[1], xaxisUpper = xaxis2()[2])
    })
    
    
    
    output$cdfPlot <- renderPlot({
      req(survivalDF(), input$RIOtreatmentGroup)
      
      
      if(input$RIOtreatmentGroup == levels(survivalDF()$treatment)[1]){
        req(cdfPlot1())
        print(cdfPlot1())
      }
      
      if(input$RIOtreatmentGroup != levels(survivalDF()$treatment)[1]){
        req(cdfPlot2())
        print(cdfPlot2())
      }
      
      
      
    })
    
    quantileValues <- reactive({
      req(fq(), input$RIOtreatmentGroup, survivalDF())
      
      if(input$RIOtreatmentGroup == levels(survivalDF()$treatment)[1]){
        req(myfit1())
        myfit <- myfit1()
        dist <- input$RIOdist1
      }
      
      if(input$RIOtreatmentGroup != levels(survivalDF()$treatment)[1]){
        req(myfit2())
        myfit <- myfit2()
        dist <- input$RIOdist2
      }
      
      
      
      
      if(min(fq())<=0 | max(fq())>=1){
        return(NULL)
      }else{
        
        FB <- feedback(myfit, 
                       quantiles = fq(),
                       ex = 1)
        
        if(dist == "best"){
          values <- FB$fitted.quantiles[, 
                                        as.character(myfit$best.fitting[1,
                                                                          1])]
        }else{
          values <- FB$fitted.quantiles[, dist]
          
        }
        
        return(values)
      }
      
    }) 
    
    probabilityValues <- reactive({
      
      req(survivalDF(), input$RIOtreatmentGroup,
          input$RIOfeedbackProbability)
      
      if(input$RIOtreatmentGroup == levels(survivalDF()$treatment)[1]){
        req(myfit1())
        myfit <- myfit1()
        dist <- input$RIOdist1
      }
      
      if(input$RIOtreatmentGroup != levels(survivalDF()$treatment)[1]){
        req(myfit2())
        myfit <- myfit2()
        dist <- input$RIOdist2
      }
      
      FB <- feedback(myfit, 
                     values = input$RIOfeedbackProbability,
                     ex = 1)
      
      if(dist == "best"){
        probs <- FB$fitted.probabilities[, 
                                         as.character(myfit$best.fitting[1,
                                                                           1])]
      }else{
        probs <- FB$fitted.probabilities[, dist]
        
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
   
    # Extrapolation plot tab ----
    
    output$extrapolationPlot <- renderPlot({
      req(survivalDF(),
          input$targetTime, myfit1() )
      myplot <- KMextrapolate(tLower = 0,
                              tUpper = input$truncationTime,
                              yl =0 ,
                              yu = 1,
                              showExp = FALSE,
                              tTarget = input$targetTime,
                              breakTime = input$targetTime/4,
                              survDf = survivalDF(),
                              groups = levels(survivalDF()$treatment),
                              xl = paste0("Time (", input$timeUnit, ")"),
                              includeEbar = FALSE,
                              includeExpRibbon = TRUE,
                              KMCI = TRUE,
                              fontsize = input$fs)
      
      if(input$RIOdist1 == "best"){
        fqDist1 <- myfit1()$best.fitting[1, 1]
      }else{
        fqDist1 <- input$RIOdist1
      }
      
      fq1<- feedback(myfit1(),
                     quantiles=c(0.025, 0.5, 0.975))$fitted.quantiles[, fqDist1]
     
      if(length(levels(survivalDF()$treatment)) == 1){
        jitter <- 1
      }else{
        jitter <- 1.005
      }
      
      
      
      p1 <- myplot + annotate("errorbar", x = jitter*input$targetTime,
                             y = fq1[2],
                             ymin = fq1[1],
                             ymax = fq1[3],
                             colour = "#F8766D",
                             linewidth =2,
                             width = 0.02*input$targetTime) 
      
      if(length(levels(survivalDF()$treatment)) > 1){
        
        if(input$RIOdist2 == "best"){
          fqDist2 <- myfit2()$best.fitting[1, 1]
        }else{
          fqDist2 <- input$RIOdist2
        }
        
        fq2<- feedback(myfit2(),
                       quantiles=c(0.025,0.5,
                                   0.975))$fitted.quantiles[, fqDist2]
        p1 <- p1 +
        annotate("errorbar", x = 0.995*input$targetTime,
                 y = fq2[2],
                 ymin = fq2[1],
                 ymax = fq2[3],
                 colour = "#00BFC4",
                 linewidth =2,
                 width = 0.02*input$targetTime)
      }
        
      print(p1)

    })

   

   
    
    observeEvent(input$exit, {
      stopApp()
    }) 
    
    
    
    # Bivariate tab ----
    
    m1 <- reactive({
      req(p1(), v1())
      approx(p1(), v1(), 0.5)$y
    })
    
    m2 <- reactive({
      req(p2(), v2())
      approx(p2(), v2(), 0.5)$y
    })
    
    df1 <- reactive({
      req(myfit1(), myfit2(), input$concProb > 0, input$concProb < 1)
      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- input$concProb
      data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs, 
                              n = input$sampleSize, 
                              d = c(input$RIOdist1, input$RIOdist2)))
    })
    
    output$bivariate <- renderUI({
      req(survivalDF())
      if(length(levels(survivalDF()$treatment)) == 1){
        h5("Single treatment group only: no joint distribution to elicit")
      }else{
        sidebarLayout(
          sidebarPanel(
            wellPanel(
              numericInput("concProb", h5("Concordance probability"),
                           value = 0.5,
                           min = 0, max = 1),
              numericInput("sampleSize", h5("Sample size"),
                           value = 1000,
                           min = 1),
              downloadButton("downloadData", "Download sample")
            )
            
          ),
          mainPanel(
            helpText("Specify the concordance probability: the probability that 
                     the two uncertain quantities are either both below their medians,
                     or both above their medians. The plot below is a sample from 
                     the joint distribution, using the two RIO fitted distributions."),
            plotOutput("bivariatePlot")
            
          )
        )
          
      }
    })
   
    
    output$bivariatePlot <- renderPlot({
      req(df1(), m1(), m2(), survivalDF(),
          input$concProb > 0, input$concProb < 1 )
      
      
      theme_set(theme_grey(base_size = input$fs))
      
      annotations <- data.frame(
        xpos = c(Inf,Inf,-Inf,-Inf),
        ypos =  c(Inf, -Inf,-Inf,Inf),
        annotateText = as.character(c(input$concProb / 2, 
                                      0.5 - input$concProb /2,
                                      input$concProb / 2, 
                                      0.5 - input$concProb /2)),
        hjustvar = c(1.5, 1.5, -0.5, -0.5) ,
        vjustvar = c(1.5, -0.5, -0.5, 1.5))
      
      
      p1 <- ggplot(data = df1(), aes(x = X1, y = X2))
    
        p1 <- p1 + geom_point(alpha=0.15, colour = "red")
      
      p1 <- p1 + 
        geom_hline(yintercept = m2())+
        geom_vline(xintercept = m1())+
        labs(x=levels(survivalDF()$treatment)[1],
             y = levels(survivalDF()$treatment)[2])+
        geom_text(data = annotations, aes(x = xpos,
                                          y = ypos,
                                          hjust = hjustvar,
                                          vjust = vjustvar,
                                          label = annotateText),
                  size = input$fs / 2) +
        xlim(0.95*xaxis1()[1], 1.05*xaxis1()[2])+
        ylim(0.95*xaxis2()[1], 1.05*xaxis2()[2])
      
   
        
        suppressWarnings(suppressMessages(ggExtra::ggMarginal(p1, type = "histogram",
                                                              fill = "red")))
      
      
      
    })
    
    output$downloadData <- downloadHandler(
      filename = "joint-sample.csv",
      content = function(file) {
        utils::write.csv(df1(), file, row.names = FALSE)
      }
    )
    
    # Report writing ----
    
    output$reportGroup1 <- renderUI({
      req(survivalDF())
      checkboxInput("reportGroup1", paste0('Results for treatment group "',
                                           levels(survivalDF()$treatment)[1], '"'), value = TRUE)
    })
    
    output$reportGroup2 <- renderUI({
      req(survivalDF())
        if(length(levels(survivalDF()$treatment)) >1){
        checkboxInput("reportGroup2", paste0('Results for treatment group "',
                                             levels(survivalDF()$treatment)[2], '"'), value = TRUE)
      }
    })
    
    output$report <- downloadHandler(
      filename = function(){switch(input$outFormat,
                                   html_document = "extrapolation-report.html",
                                   pdf_document = "extrapolation-report.pdf",
                                   word_document = "extrapolation-report.docx")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "elicitationShinySummarySurvivalExtrapolation.Rmd")
        file.copy(system.file("shinyAppFiles", "elicitationShinySummarySurvivalExtrapolation.Rmd",
                              package="SHELF"),
                  tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        
        dist1 <- input$RIOdist1
        dist2 <- NULL
        reportGroup1 <- input$reportGroup1
        reportGroup2 <- FALSE
        
        if(length(levels(survivalDF()$treatment)) == 1){
          allfits <- list(myfit1())
          individual <- list(input$myvals1)
        }
        if(length(levels(survivalDF()$treatment)) == 2){
          allfits <- list(myfit1(), myfit2())
          individual <- list(input$myvals1, input$myvals2)
          reportGroup2 <- input$reportGroup2
          dist2 <- input$RIOdist2
        }
        names(allfits) <- names(individual) <-  levels(survivalDF()$treatment)
        
        params <- list(allfits = allfits, individual = individual,
                       survivalDF = survivalDF(),
                       targetTime = input$targetTime,
                       timeUnit = input$timeUnit,
                       truncationTime = input$truncationTime,
                       breakTime = input$breakTime,
                       reportGroup1 = reportGroup1,
                       reportGroup2 = reportGroup2,
                       reportDistributions = input$reportDistributions,
                       dist1 = dist1,
                       dist2 = dist2,
                       inputMethod = input$elicMethod,
                       reportScenarioTest = input$reportScenarioTest,
                       expRange = expRange(),
                       reportExtrapolation = input$reportExtrapolation)
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


# quantileValues1 <- reactive({
#   ssq <- myfit1()$ssq[1, is.na(myfit1()$ssq[1,])==F]
#   best.index <- which(ssq == min(ssq))[1]
#   
#   ex <- 1
#   pl <- limits1()[1]
#   pu <- limits1()[2]
#   if(as.numeric(input$radio1)==8){index<-best.index}else{index<-as.numeric(input$radio1) - 1}
#   if(as.numeric(input$radio1)==1){
#     if(pl == -Inf & myfit1()$limits[ex,1] > -Inf){pl <- myfit1()$limits[ex,1]}
#     if(pu == Inf & myfit1()$limits[ex,2] < Inf){pu <- myfit1()$limits[ex,2] }
#     if(pl == -Inf & myfit1()$limits[ex,1] == -Inf){
#       pl <- qnorm(0.001, myfit1()$Normal[ex,1], myfit1()$Normal[ex,2])}
#     if(pu == Inf & myfit1()$limits[ex,2] == Inf){
#       pu <- qnorm(0.999, myfit1()$Normal[ex,1], myfit1()$Normal[ex,2])}
#     p <- c(0, myfit1()$probs[ex,], 1)
#     x <- c(pl, myfit1()$vals[ex,], pu)
#     values <- qhist(c(0.05, 0.95), x, p)
#   }
#   
#   if(as.numeric(input$radio1)>1){
#     temp<-feedback(myfit1(), quantiles=c(0.05, 0.95), ex=1)
#     values=temp$fitted.quantiles[,index]
#   }
#   data.frame(quantiles=c(0.05, 0.95), values=values)
#   
# }) 
# 
# 
# quantileValues2 <- reactive({
#   ssq <- myfit2()$ssq[1, is.na(myfit2()$ssq[1,])==F]
#   best.index <- which(ssq == min(ssq))[1]
#   
#   ex <- 1
#   pl <- limits1()[1]
#   pu <- limits1()[2]
#   if(as.numeric(input$radio2)==8){index<-best.index}else{index<-as.numeric(input$radio2) - 1}
#   if(as.numeric(input$radio2)==1){
#     if(pl == -Inf & myfit2()$limits[ex,1] > -Inf){pl <- myfit2()$limits[ex,1]}
#     if(pu == Inf & myfit2()$limits[ex,2] < Inf){pu <- myfit2()$limits[ex,2] }
#     if(pl == -Inf & myfit2()$limits[ex,1] == -Inf){
#       pl <- qnorm(0.001, myfit2()$Normal[ex,1], myfit2()$Normal[ex,2])}
#     if(pu == Inf & myfit2()$limits[ex,2] == Inf){
#       pu <- qnorm(0.999, myfit2()$Normal[ex,1], myfit2()$Normal[ex,2])}
#     p <- c(0, myfit2()$probs[ex,], 1)
#     x <- c(pl, myfit2()$vals[ex,], pu)
#     values <- qhist(c(0.05, 0.95), x, p)
#   }
#   
#   if(as.numeric(input$radio1)>1){
#     temp<-feedback(myfit1(), quantiles=c(0.05, 0.95), ex=1)
#     values=temp$fitted.quantiles[,index]
#   }
#   data.frame(quantiles=c(0.05, 0.95), values=values)
#   
# }) 
# 
# 
# output$valuesPDF1 <- renderTable({
#   quantileValues1()
# })
# 
# output$valuesPDF2 <- renderTable({
#   quantileValues2()
# })

# Helper function KMextrapolate ----

KMextrapolate <- function(tLower = 0,
                          tUpper,
                          yl,
                          yu,
                          showExp = FALSE,
                          expLower,
                          expUpper,
                          expGroup,
                          tTarget,
                          survDf,
                          breakTime = NULL,
                          groups,
                          xl,
                          includeEbar = TRUE,
                          includeExpRibbon = TRUE,
                          KMCI = FALSE,
                          fontsize = 16){
  
  x <- ymin <- ymax <- NULL # hack to avoid R CMD check NOTE
  
  # Truncate data frame for plotting
  index <- survDf$time > tUpper
  truncatedDf <- survDf
  truncatedDf$event[index] <- 0
  truncatedDf$time[index] <- tUpper
  
  # Prepare KM plot
  fit <- survival::survfit(survival::Surv(time, event) ~ treatment, data = truncatedDf)
  myplot <- survminer::ggsurvplot(fit, data = truncatedDf, censor = FALSE,
                                  break.time.by = breakTime,
                                  legend = "right",
                                  legend.title = "",
                                  legend.labs = groups,
                                  xlim = c(tLower, tTarget),
                                  xlab = xl,
                                  conf.int = KMCI,
                                  break.y.by = 0.2)
  myplot$plot <- myplot$plot + geom_vline(xintercept = tTarget, linetype="dotted") +theme_bw(base_size = fontsize)
  if(includeEbar){
    myplot$plot <- myplot$plot + annotate("errorbar", x = 8,
                                          y = mean(c(yu, yl)),
                                          ymin = yu,
                                          ymax = yl,
                                          colour = "black")
  }
  
  if(showExp){
    
    
    
    dfExp <- truncatedDf[truncatedDf$treatment == expGroup, ]
    
    expUpper <- min(expUpper, max(dfExp$time))
    
    fitExp <- survival::survfit(survival::Surv(time, event) ~ 1, data = dfExp)
    Plower <- summary(fitExp, times = expLower)$surv
    index <- dfExp$time > expLower 
    eventTruncated <- dfExp$event
    eventTruncated[dfExp$time > expUpper] <- 0
    eventTruncated <- eventTruncated[index]
    timeTruncated <- dfExp$time[index]
    timeTruncated[timeTruncated > expUpper] <- expUpper
    
    expFit <- survival::survreg(survival::Surv(time = timeTruncated-expLower, event = eventTruncated) ~ 1, dist = "exponential")
    
    
    
    lambda <- exp(-expFit$coefficients)
    
    myfun <- function(x) exp(-lambda*(x-expLower))*Plower
    
    
    
    myplot$plot <- myplot$plot + 
      stat_function(fun = myfun, xlim = c(expLower, expUpper), lwd = 1.5, alpha = 0.9) +
      stat_function(fun = myfun, xlim = c(expUpper, tTarget), linetype = "dashed")
    
    tVals <- seq(from = expLower, to = tTarget, length = 100)
    mP <- summary(fitExp, times = expLower)$surv
    sP <- summary(fitExp, times = expLower)$std.err
    
    mLogLambda <- summary(expFit)$table[1]
    sLogLambda <- summary(expFit)$table[2]
    
    expMatrix <- matrix(0, 1000, 100)
    
    for(i in 1:1000){
      randomP <- rnorm(1, mP, sP)
      randomLambda <- exp(-rnorm(1, mLogLambda, sLogLambda ))
      expMatrix[i, ] <- exp(-randomLambda*(tVals-expLower))*randomP
    }
    
    if(includeExpRibbon){
      
      
      ribbon_data <- data.frame(x =tVals,
                                ymax = apply(expMatrix, 2, quantile, probs = 0.975),
                                ymin = apply(expMatrix, 2, quantile, probs = 0.025),
                                surv =tVals)
      
      myplot$plot <- myplot$plot +
        geom_ribbon(data = ribbon_data,
                    aes(x = x, ymin = ymin, ymax = ymax),
                    fill = "gray", alpha = 0.5)
      
    }
   
    
    
  }
  
  
  myplot$plot
}

# Helper function survivalTable ----

scenarioTestInterval <- function(tLower = 0,
                          tUpper,
                        
                          expLower,
                          expUpper,
                          expGroup,
                          tTarget,
                          survDf){
  # Truncate data frame for plotting
  index <- survDf$time > tUpper
  truncatedDf <- survDf
  truncatedDf$event[index] <- 0
  truncatedDf$time[index] <- tUpper
  
  # Prepare KM plot

    dfExp <- truncatedDf[truncatedDf$treatment == expGroup, ]
    fitExp <- survival::survfit(survival::Surv(time, event) ~ 1, data = dfExp)
    Plower <- summary(fitExp, times = expLower)$surv
    index <- dfExp$time > expLower 
    eventTruncated <- dfExp$event
    eventTruncated[dfExp$time > expUpper] <- 0
    eventTruncated <- eventTruncated[index]
    timeTruncated <- dfExp$time[index]
    timeTruncated[timeTruncated > expUpper] <- expUpper
    
    expFit <- survival::survreg(survival::Surv(time = timeTruncated-expLower, event = eventTruncated) ~ 1, dist = "exponential")
    lambda <- exp(-expFit$coefficients)
    
    mP <- summary(fitExp, times = expLower)$surv
    sP <- summary(fitExp, times = expLower)$std.err
    
    mLogLambda <- summary(expFit)$table[1]
    sLogLambda <- summary(expFit)$table[2]
    
    
      randomP <- rnorm(10000, mP, sP)
      randomLambda <- exp(-rnorm(10000, mLogLambda, sLogLambda ))
      tSample <- exp(-randomLambda*(tTarget-expLower))*randomP
   
    quantile(tSample, c(0.025, 0.975))
   
      
}


makeSurvivalTable <- function(survDF, breakTime, truncationTime, timeUnit, dp = 2){
  sv <- survival::survfit(survival::Surv(time, event) ~ treatment, data = survDF)
  truncationTime <- min(truncationTime, min(tapply(survDF$time, survDF$treatment, max)))
  sTimes <- seq(from = breakTime, to = truncationTime, by = breakTime)
  nTimes <- length(sTimes)
  nTreatments <- length(levels(survDF$treatment))
  tNames <- levels(survDF$treatment)
  
  pt <- matrix(round(summary(sv, times = sTimes)$surv , dp),
               nrow = nTimes, ncol = nTreatments)
  wt  <- pt
  if(nrow(wt) > 1){
    wt[-1, ] <- 1 - round(pt[2:nTimes, ]/pt[1:(nTimes - 1), ], dp)
    }
  
  ciLower <- matrix(round(summary(sv, times = sTimes)$lower , dp),
                    nrow = nTimes, ncol = nTreatments)
  ciUpper <- matrix(round(summary(sv, times = sTimes)$upper , dp),
                    nrow = nTimes, ncol = nTreatments)
  ci95 <-  matrix (paste0("(", ciLower, ", ", ciUpper,")"),
                   nrow = nTimes, ncol = nTreatments)
  sTable <- data.frame(paste0("[", c(0, sTimes[1:(nTimes -1)]), ",", sTimes, ")"),
                       pt[, 1], ci95[, 1],  wt[, 1])
  colnames(sTable) <- c(paste0("time interval (", timeUnit,")"),
                        paste0("survivor (", tNames[1], ")"),
                        paste0("survivor 95% CI (", tNames[1], ")"),
                        paste0("hazard (", tNames[1], ")"))
  
  if(nTreatments > 1){
    for(i in 2:nTreatments){
      dfTemp <-  data.frame(pt[, i], ci95[, i],  wt[, i])
      colnames(dfTemp) <- c(paste0("survivor (", tNames[i], ")"),
                            paste0("survivor 95% CI (", tNames[i], ")"),
                            paste0("hazard (", tNames[i], ")"))
      sTable <- cbind(sTable, dfTemp)
    }
  }
  
  sTable
}

makeSurvSummaryTable <- function(survDF, sf = 3){
  
  nTreatments <- length(levels(survDF$treatment))
  
  sTable <- array(NA, c(nTreatments, 5))
  rownames(sTable) <- levels(survDF$treatment)
  colnames(sTable) <- c("n", "events", "minimum", "median", "maximum")
  
  sv <- survival::survfit(survival::Surv(time, event) ~ treatment, data = survDF)
  table_output <- summary(sv)$table
  
  # Need to keep array format with column names if only one treatment group:
  if(nTreatments == 1){
    dnames <- names(table_output)
    dim(table_output) <- c(1, 9)
    colnames(table_output) <- dnames
  }
  
  sTable[, c("n", "events", "median")] <- table_output[, c("records", "events", "median")]
  sTable[, c("minimum")] <- tapply(survDF$time, survDF$treatment, min)
  sTable[, c("maximum")] <- tapply(survDF$time, survDF$treatment, max)
  
  sTable[, c("minimum", "maximum", "median") ] <- 
    signif( sTable[, c("minimum", "maximum", "median")  ], sf)
  
  sTable

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