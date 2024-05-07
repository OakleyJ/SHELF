#' Elicit a bivariate distribution using a Gaussian copula
#' 
#' Opens up a web browser (using the shiny package), from which you can specify
#' judgements, fit distributions, plot the fitted density functions, and plot samples 
#' from the joint distributions. A joint distribution is constructed using a Gaussian
#' copula, whereby the correlation parameter is determined via the elicitation of a 
#' concordance probability (a probability that the two uncertain quantities are either
#' both greater than their medians, or both less than their medians.)
#' 
#' Click on the "Help" tab for instructions. Click the "Quit" button to exit the app and return
#' the results from the \code{fitdist} command. Click "Download report" to generate a report
#' of all the fitted distributions for each uncertain quantity, and "Download sample" to
#' generate a csv file with a sample from the joint distribution.
#'
#' @aliases elicitBivariate elicitConcProb   
#' @return A list, with two objects of class \code{elicitation}, and the 
#' elicited concordance probability. See \code{\link{fitdist}} for details.
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' 
#' \dontrun{
#' 
#' elicitBivariate()
#' 
#' }
#' @import shiny
#' @export
elicitBivariate<- function(){
  runApp(list(
  ui = shinyUI(fluidPage(
    
    # Application title
    titlePanel("SHELF: bivariate distribution"),
    
   # sidebarLayout(
  mainPanel(tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
                       ),
      
              tabsetPanel(
                tabPanel("Parameter 1",
                         fluidRow(
                           column(4, 
                                  textInput("limits1", label = h5("Parameter 1 limits"), 
                                            value = "0, 100")
                                  ),
                           column(4,
                                  textInput("values1", label = h5("Parameter 1 values"), 
                                            value = "25, 50, 75")
                                  ),
                           column(4,
                                  textInput("probs1", label = h5("Cumulative probabilities"), 
                                            value = "0.25, 0.5, 0.75")
                                  )
                           ),
                         fluidRow(
                           column(4, 
                                  selectInput("dist1", label = h5("Distribution"), 
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
                                              selected = 1
                                  )),
                           column(4,conditionalPanel(
                             condition = "input.dist1 == 't' || input.dist1 == 'logt' || input.dist1 == 'mirrorlogt'",
                             numericInput("tdf1", label = h5("Student-t degrees of freedom"),
                                          value = 3)
                           )
                           )
                           
                           ),
                         
                         
                         
                         
                         plotOutput("distPlot1")
                         #tableOutput("valuesPDF1")
                         ),
                tabPanel("Parameter 2",
                         fluidRow(
                           column(4, 
                                  textInput("limits2", label = h5("Parameter limits"), 
                                            value = "0, 200")
                           ),
                           column(4,
                                  textInput("values2", label = h5("Parameter values"), 
                                            value = "30, 40, 60")
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
                                              selected = 1
                                  )),
                           column(4,
                                  conditionalPanel(
                                    condition = "input.dist2 == 't' || input.dist2 == 'logt' || input.dist1 == 'mirrorlogt'",
                                    numericInput("tdf2", label = h5("degrees of freedom"),
                                                 value = 3)
                                    
                                    
                                  ))
                           
                         ),
                        
                         plotOutput("distPlot2")
                        # tableOutput("valuesPDF2")
                        ),
                
                tabPanel("Joint distribution",
                         fluidRow(
                           column(4, 
                         numericInput("concProb", h5("Concordance probability"),
                                      value = 0.5,
                                      min = 0, max = 1)
                         ),
                         column(4,
                                numericInput("sampleSize", h5("Sample size"),
                                             value = 1000,
                                             min = 1)
                                ),
                         column(4, 
                                checkboxInput("showSample", "Show sample")
                                )
                         ),
                         plotOutput("bivariatePlot")
                         ),
                tabPanel("Help",
                         includeHTML(system.file("shinyAppFiles", "helpBivariate.html",
                                                 package="SHELF"))
                )
                
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
      tryCatch(eval(parse(text = paste("c(", input$limits1, ")"))),
               error = function(e){NULL})
    })
    
    limits2 <- reactive({
      tryCatch(eval(parse(text = paste("c(", input$limits2, ")"))),
               error = function(e){NULL})
    })
    
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
    
    m1 <- reactive({
      req(p1(), v1())
      approx(p1(), v1(), 0.5)$y
    })
    
    m2 <- reactive({
      req(p2(), v2())
      approx(p2(), v2(), 0.5)$y
    })
  
    myfit1 <- reactive({
      
      req(limits1(), v1(), p1(), input$tdf1)
      
      check <- checkJudgementsValid(probs = p1(), vals = v1(),
                                    tdf = input$tdf1,
                                    lower = limits1()[1],
                                    upper= limits1()[2])
      if(check$valid == TRUE){
      
        fitdist(vals = v1(), probs = p1(), lower = limits1()[1],
              upper = limits1()[2], 
              tdf = input$tdf1)
      }
    })
    
    myfit2 <- reactive({
      req(limits2(), v2(), p2(), input$tdf2)
      
      check <- checkJudgementsValid(probs = p2(), vals = v2(),
                                    tdf = input$tdf2,
                                    lower = limits2()[1],
                                    upper= limits2()[2])
      if(check$valid == TRUE){
      fitdist(vals = v2(), probs = p2(), lower = limits2()[1],
              upper = limits2()[2], 
              tdf = input$tdf2)
      }
    })
    
    output$distPlot1 <- renderPlot({
      
      req(myfit1(), limits1())
      
  
      #d = dist[as.numeric(input$radio1)]
     # dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      suppressWarnings(plotfit(myfit1(), d = input$dist1,
                                ql = 0.05, qu = 0.95,
                               xl = limits1()[1], xu = limits1()[2], 
                               fs = input$fs))
    
      
    })
    
    
    output$distPlot2 <- renderPlot({
      
      req(myfit2(), limits2())
        
      #  dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
        suppressWarnings(plotfit(myfit2(), d = input$dist2,
                                  ql = 0.05, qu = 0.95,
                                 xl = limits2()[1], xu = limits2()[2], 
                                 fs = input$fs))
      
      
    })
    
    df1 <- reactive({
      req(myfit1(), myfit2(), input$concProb > 0, input$concProb < 1)
      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- input$concProb
      data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs, 
                                     n = input$sampleSize, 
                                     d = c(input$dist1, input$dist2)))
    })
    
    output$bivariatePlot <- renderPlot({
      req(df1(), m1(), m2(), input$concProb > 0, input$concProb < 1 )
      
      
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
      if(input$showSample){
        p1 <- p1 + geom_point(alpha=0.15, colour = "red")}
      
      p1 <- p1 + 
        geom_hline(yintercept = m2())+
        geom_vline(xintercept = m1())+
        labs(x=expression(X[1]), y = expression(X[2]))+
        geom_text(data = annotations, aes(x = xpos,
                                          y = ypos,
                                          hjust = hjustvar,
                                          vjust = vjustvar,
                                          label = annotateText),
                  size = input$fs / 2) +
        xlim(0.95*limits1()[1], 1.05*limits1()[2])+
        ylim(0.95*limits2()[1], 1.05*limits2()[2])
      
      if(input$showSample){

        suppressWarnings(suppressMessages(ggExtra::ggMarginal(p1, type = "histogram",
                                                              fill = "red")))
      }else{
        suppressWarnings(suppressMessages(p1))
      }
      
      
    })
  

   
    
    observeEvent(input$exit, {
      stopApp(list(parameter1 = myfit1(), parameter2 = myfit2(), 
                   cp = input$concProb))
    }) 
    
    output$downloadData <- downloadHandler(
      filename = "joint-sample.csv",
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
        tempReport <- file.path(tempdir(), "elicitationShinySummaryBivariate.Rmd")
        file.copy(system.file("shinyAppFiles", "elicitationShinySummaryBivariate.Rmd",
                              package="SHELF"),
                  tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(fit1 = myfit1(), fit2 = myfit2(), cp = input$concProb,
                       d = c(input$dist1, input$dist2), m1 = m1(), m2 = m2())
        
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
