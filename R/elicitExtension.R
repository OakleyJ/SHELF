#' Elicitation with the extension method
#' 
#' Opens up a web browser (using the shiny package), from which you can specify
#' judgements, fit distributions, and produce various plots. Judgements are 
#' specified for the distribution of the conditioning variable Y, the median 
#' function (median of X given Y), and the distribution of X given that Y takes
#' its median value. Plots are provided for the two elicited distributions, the
#' median function, the conditional distribution of X for any specified Y, and
#' the marginal distribution of X.  
#' 
#' Click the "Quit" button to exit the app and return
#' the results from the \code{fitdist} command. Click "Download report" to generate a report
#' of all the fitted distributions for each uncertain quantity, and "Download sample" to
#' generate a csv file with a sample from the marginal distribution of X.
#'   
#' @return A list, with two objects of class \code{elicitation}.
#'  See \code{\link{fitdist}} for details.
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' 
#' elicitExtension()
#' 
#' }
#' @import shiny
#' @export
elicitExtension<- function(){
  runApp(list(
  ui = shinyUI(fluidPage(
    
    # Application title
    titlePanel("SHELF: Extension method elicitation"),
    
   # sidebarLayout(
  mainPanel(tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
                       ),
      
              tabsetPanel(
                tabPanel("Y distribution",
                         wellPanel(
                           h4("Elicit judgements about the extension variable: instructions"),
                           tags$ol(
                             tags$li("Specify lower and upper parameter limits.
                             These will be used to set the axes ranges in the
                             plots. Note that the gamma, log normal and log t
                             distributions are shifted to have support
                             (lower limit, Infinity), and the beta distribution is
                             scaled and shifted to have support (lower limit,
                             upper limit)."),
                             tags$li("Elicit at least two probabilities for the 
                             extension variable Pr( Y < y ) = p. Enter the values
                             y in the 'Parameter Y values' box, and the
                             corresponding probabilities p in the 'Cumulative
                             probabilities box'. The smallest probability must
                             be less than 0.4, and the largest probability must
                             be greater than 0.6."),
                             tags$li("Choose which distribution to fit to the elicited
                             judgements about the extension variable."))),
                         fluidRow(
                           column(4, 
                                  textInput("limits1", label = h5("Parameter Y limits"), 
                                            value = "-6, 20")
                                  ),
                           column(4,
                                  textInput("values1", label = h5("Parameter Y values"), 
                                            value = "-3, 7, 17")
                                  ),
                           column(4,
                                  textInput("probs1", label = h5("Cumulative probabilities"), 
                                            value = "0.05, 0.5, 0.95")
                                  )
                           ),
                         fluidRow(
                           column(4, 
                                  selectInput("dist1", label = h5("Distribution"), 
                                              choices =  list(Histogram = "hist",
                                                              Normal = "normal", 
                                                              'Student-t' = "t",
                                                              Gamma = "gamma",
                                                              'Log normal' = "lognormal",
                                                              'Log Student-t' = "logt",
                                                              Beta = "beta", 
                                                              'Best fitting' = "best"),
                                              #choiceValues = 1:8,
                                              selected = "normal"
                                  )),
                           column(4,conditionalPanel(
                             condition = "input.dist1 == 't' || input.dist1 == 'logt'",
                             numericInput("tdf1", label = h5("Student-t degrees of freedom"),
                                          value = 3)
                           )
                           )
                           
                           ),
                         
                         
                         
                         
                         plotOutput("distPlot1")
                         #tableOutput("valuesPDF1")
                         ),
                tabPanel("Median model",
                         fluidRow(
                           wellPanel(
                             h4("Elicit the median model: instructions"),
                             tags$ol(
                               tags$li("Specify hypothetical values of the extension 
                                       variable in the Conditioning points box."),
                               tags$li("For each hypothetical value, specify the 
                                       corresponding elicited median for the target variable,
                                       in the Conditional medians box."),
                               tags$li("Select a transformation. If the target variable
                                       must be positive, try a log transformation, and
                                       if the target variable is constrained to be between
                                       0 and 1, try a logit transformation."))),
                           
                           
                           column(4, 
                                  textInput("yCP", label = h5("Conditioning points"), 
                                            value = "-3, 3, 7, 11, 17")
                           ),
                           column(4,
                                  textInput("xMed", label = h5("Conditional medians"), 
                                            value = "88,  64, 49, 46, 44")
                           )
                           ),
                         fluidRow(
                           column(4, 
                                  radioButtons("link", "Transformation",
                                               c("identity",
                                                 "log",
                                                 "logit"))
                                  )
                           
                         ),
                        
                         plotOutput("medianFunction")
                        # tableOutput("valuesPDF2")
                        ),
                tabPanel("c-distribution",
                         fluidRow(
                           wellPanel(
                             h4("Elicit the c-distribution: instructions"),
                             tags$ol(
                               tags$li("The c-distribution is the distribution of
                                       the target variable X, conditional on the
                                       extension variable Y taking its median value."),
                               tags$li("The median value of Y needs to be specified
                                       in the appropriate box."),
                               tags$li("Specify the elicited judgements about X|Y in 
                                       the same way as that used for the Y distribution. "))),
                           column(4, 
                                  textInput("limits2", label = h5("Parameter X limits"), 
                                            value = "0, 100")
                           ),
                           column(4,
                                  textInput("values2", label = h5("Parameter X values"), 
                                            value = "40, 50, 60")
                           ),
                           column(4,
                                  textInput("probs2", label = h5("Cumulative probabilities"), 
                                            value = "0.25, 0.5, 0.75")
                           )
                         ),
                         fluidRow(
                           column(4, 
                                  selectInput("dist2", label = h5("Distribution"), 
                                              choices =  list(Histogram = "hist",
                                                              Normal = "normal", 
                                                              'Student-t' = "t",
                                                              Gamma = "gamma",
                                                              'Log normal' = "lognormal",
                                                              'Log Student-t' = "logt",
                                                              Beta = "beta", 
                                                              'Best fitting' = "best"),
                                              #choiceValues = 1:8,
                                              selected = "gamma"
                                  )),
                           column(4,conditionalPanel(
                             condition = "input.dist1 == 't' || input.dist1 == 'logt'",
                             numericInput("tdf2", label = h5("Student-t degrees of freedom"),
                                          value = 3)
                           )
                           ),
                           column(4,
                                  numericInput("medianY", label = h5("Median of Y"), 
                                            value = 7)
                           )
                           
                         ),
                         
                         
                         
                         
                         plotOutput("distPlot2")
                         #tableOutput("valuesPDF1")
                ),
                
                tabPanel("Conditional distributions",
                         fluidRow(
                           wellPanel(
                           h4("Display conditional distributions: instructions"),
                           tags$ol(
                             tags$li("Specify any set of hypothetical values for the
                                     extension variable Y."),
                             tags$li("Density plots will be displayed for the target variable
                                     X, conditional on each hypothetical value of Y."))),
                           column(4,
                                  textInput("valuesY", label = h5("Parameter Y values"), 
                                            value = "-3, 7, 17")
                           )
                         ),
                         plotOutput("conditionalPlot")
                         ),
                tabPanel("Marginal distribution",
                         fluidRow(
                           wellPanel(
                             h4("Marginal distribution: instructions"),
                             tags$ol(
                               tags$li("A sample from the marginal distribution
                                       of the target variable is generated,
                                       and a kernel density estimate is displayed.
                                       Tick marks on the x-axis indicate the 5th, 50th
                                       and 95th percentiles."),
                               tags$li("Specify the desired sample size."),
                               tags$li("Click on 'Download sample' to save
                                       the sampled values in .csv format."))),
                         column(4,
                                numericInput("n", label = h5("sample size"), 
                                             value = 10000)
                         )),
                         plotOutput("marginalPlot")
                )
                
                # ),
                #          
                # tabPanel("Help",
                #          includeHTML(system.file("shinyAppFiles", "helpBivariate.html",
                #                                  package="SHELF"))
                # )
                
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
                column(3, downloadButton("downloadData", "Download sample")
                ),
                column(3, actionButton("exit", "Quit")
                )
              )
              
            )
            
  )
  )
  ),
   
  server = function(input, output) {
    
    # Hack to avoid CRAN check NOTE
    
    X1 <- X2 <- xpos <- ypos <- hjustvar <- vjustvar <- annotateText <- NULL
    
    limits1 <- reactive({
      eval(parse(text = paste("c(", input$limits1, ")")))
    })
    
    limits2 <- reactive({
      eval(parse(text = paste("c(", input$limits2, ")")))
    })
    
    yCP <- reactive({
      eval(parse(text = paste("c(", input$yCP, ")")))
    })
    
    p1 <- reactive({
      eval(parse(text = paste("c(", input$probs1, ")")))
    })
    
    p2 <- reactive({
      eval(parse(text = paste("c(", input$probs2, ")")))
    })
    
    xMed <- reactive({
      eval(parse(text = paste("c(", input$xMed, ")")))
    })
    
    v1 <- reactive({
      eval(parse(text = paste("c(", input$values1, ")")))
    })
    
    v2 <- reactive({
      eval(parse(text = paste("c(", input$values2, ")")))
    })
    
    yHyp <- reactive({
      eval(parse(text = paste("c(", input$valuesY, ")")))
    })
    
    
    
    m1 <- reactive({
      approx(p1(), v1(), 0.5)$y
    })
    
    m2 <- reactive({
      approx(p2(), v2(), 0.5)$y
    })
    
    
  
    myfit1 <- reactive({
        fitdist(vals = v1(), probs = p1(), lower = limits1()[1],
              upper = limits1()[2], 
              tdf = input$tdf1)
    })
    
    myfit2 <- reactive({
      fitdist(vals = v2(), probs = p2(), lower = limits2()[1],
              upper = limits2()[2], 
              tdf = input$tdf2)
    })
    
    
    
    output$distPlot1 <- renderPlot({
      
  
      #d = dist[as.numeric(input$radio1)]
     # dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      suppressWarnings(plotfit(myfit1(), d = input$dist1,
                               int = F, ql = 0.05, qu = 0.95,
                               xl = limits1()[1], xu = limits1()[2], 
                               fs = input$fs))
    
      
    })
    
    output$distPlot2 <- renderPlot({
      
      
      #d = dist[as.numeric(input$radio1)]
      # dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      suppressWarnings(plotfit(myfit2(), d = input$dist2,
                               int = F, ql = 0.05, qu = 0.95,
                               xl = limits2()[1], xu = limits2()[2], 
                               fs = input$fs))
      
      
    })
    
    
    output$medianFunction <- renderPlot({
      
      validTransform <- TRUE
     
      if(min(xMed()) <= 0 & input$link == "log"){
        showNotification("To use the log transformation, all the conditional
                         medians must be greater than 0", 
                         type = "error",
                         duration = 60)
        validTransform <- FALSE
      }
      
      if((min(xMed()) <= 0 | max(xMed())>=1) & input$link == "logit"){
        showNotification("To use the logit transformation, all the conditional
                         medians must be greater than 0 and less than 1", 
                         type = "error",
                         duration = 60)
        validTransform <- FALSE
      }
      
     if(validTransform){
      print(plotConditionalMedianFunction(yCP = yCP(),
                                    xMed = xMed(),
                                    yLimits = limits1(),
                                    link = input$link,
                                    fs = input$fs))
     }
      
      
    })
    
    output$conditionalPlot <- renderPlot({
      
      validTransform <- TRUE
      
      if(min(xMed()) <= 0 & input$link == "log"){
        showNotification("Median model (log transformation) is not valid. 
        Return to the median model tab, and select the identify transformation,
        or adjust the conditional medians.", 
                         type = "error",
                         duration = 60)
        validTransform <- FALSE
      }
      
      if((min(xMed()) <= 0 | max(xMed())>=1) & input$link == "logit"){
        showNotification("Median model (logit transformation) is not valid. 
        Return to the median model tab, and select the identify transformation,
        or adjust the conditional medians.", 
                         type = "error",
                         duration = 60)
        validTransform <- FALSE
      }
      
      if(validTransform){
      
      
      plotConditionalDensities(y = yHyp(),
                               fitX = myfit2(),
                               yCP = yCP(),
                               xMed = xMed(),
                               link = input$link,
                               medianY = input$medianY,
                               dist = input$dist2,
                               fs = input$fs)
      }
      
      
    })
    
    output$marginalPlot <- renderPlot({
      
      validTransform <- TRUE
      
      if(min(xMed()) <= 0 & input$link == "log"){
        showNotification("Median model (log transformation) is not valid. 
        Return to the median model tab, and select the identify transformation,
        or adjust the conditional medians.", 
                         type = "error",
                         duration = 60)
        validTransform <- FALSE
      }
      
      if((min(xMed()) <= 0 | max(xMed())>=1) & input$link == "logit"){
        showNotification("Median model (logit transformation) is not valid. 
        Return to the median model tab, and select the identify transformation,
        or adjust the conditional medians.", 
                         type = "error",
                         duration = 60)
        validTransform <- FALSE
      }
      
      if(validTransform){
      
      X <- ..density.. <- NULL
      xQuantiles <- signif(quantile(df1()$X,
                                   c(0.05, 0.5, 0.95)),
                          3)
      attr(xQuantiles, "names") <- NULL
      ggplot(df1(), aes(x = X, y = ..density..))+
        geom_density(fill = "steelblue") +
        scale_x_continuous(breaks = xQuantiles,
                           minor_breaks = NULL) +
        theme_grey(base_size = input$fs)
      
      }
      
    })
    
    df1 <- reactive({
      ry <- sampleFit(myfit1(), n = input$n)[, input$dist1]
      xSample <- sampleMarginalFit(myfit2(), 
                                   sampleY = ry,
                                   medianY = input$medianY,
                                   yCP = yHyp(),
                                   xMed = xMed(),
                                   link = input$link,
                                   dist = input$dist2
      )
      data.frame(X = xSample)
    })
    
    
    
   

   
    
    observeEvent(input$exit, {
      stopApp(list(yDistribution = myfit1(), cDistribution = myfit2()))
    }) 
    
    output$downloadData <- downloadHandler(
      filename = "marginal-sample.csv",
      content = function(file) {
        utils::write.csv(df1(), file, row.names = FALSE)
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
        tempReport <- file.path(tempdir(), "elicitationShinySummaryExtension.Rmd")
        file.copy(system.file("shinyAppFiles", "elicitationShinySummaryExtension.Rmd",
                              package="SHELF"),
                  tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(fit1 = myfit1(), fit2 = myfit2(), cp = input$concProb,
                       d = c(input$dist1, input$dist2), m1 = m1(), m2 = m2(),
                       link = input$link, yLimits = limits1(),
                       yCP = yCP(), xMed = xMed(),
                       df1 = df1())
        
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
  ))
}


