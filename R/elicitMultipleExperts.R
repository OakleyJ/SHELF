#' Elicit individual judgements and fit distributions for multiple experts
#' 
#' Opens up a web browser (using the shiny package), from which you can specify
#' judgements, fit distributions and plot the fitted density functions with
#' additional feedback.
#' 
#' Click the Finish button to quit the elicitation session.
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
#' @import rhandsontable
#' @export

elicitMultiple <- function(){
  
  dist<-c("hist", "normal", "t", "gamma", "lognormal", "logt","beta", "best")
  
  
  ui <- shinyUI(fluidPage(
    
    titlePanel("Group elicitation"),
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          numericInput("nExperts", label = h5("Number of experts"),
                       value = 2, min = 1),
          textInput("probs", label = h5("Cumulative probabilities"), 
                    value = "0.25, 0.5, 0.75"),
          radioButtons("radio", label = h5("Distribution"), 
                       choices =  list("Histogram" = 1, 
                                       "Normal" = 2, 
                                       "Student t" = 3, 
                                       "Gamma" = 4, 
                                       "Log normal" = 5, 
                                       "Log Student t" = 6, 
                                       "Beta" = 7, 
                                       "Best fitting" =8),
                       selected = 1 )
          ),
          wellPanel(
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
            numericInput("fq1", label = h5("lower feedback quantile"), 
                         value = 0.05,min=0,max=1),
            numericInput("fq2", label = h5("upper feedback quantile"),
                         value = 0.95,min=0,max=1)
            )
          )
        ),
        numericInput("fs", label = h5("font size"), value = 12)
        
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Judgements",
                   helpText("Enter the judgements in the table below,
                            one column per expert. Enter lower plausible limits in the first row,
                            upper plausible limits in the last row, and quantile values in between, 
                            corresponding to the cumulative probabilities."),
                   #rHandsontableOutput("hot"),
                   fluidRow(
                     column(2, tableOutput("ShowJudgements")),
                     column(7, offset = 1,  uiOutput("EnterJudgements"))
                 
                   )
                   ),
          tabPanel("PDF",
                   plotOutput("distPlot"),
                   conditionalPanel(
                     condition = "input.showfeedback == true",
                     tableOutput('lpquantiles'))),
          tabPanel("Tertiles",
                   plotOutput("Tertiles"),
                   helpText("The coloured bars divide the plausible range for each expert
into three equally likely regions, as specified by each expert's tertiles. Each expert's median
is shown by a dashed line. The tertiles displayed will either be the elicited tertiles, if they have been provided,
or estimates obtained by linear interpolation of the elicited probabilities, with zero probability assumed
outside the plausible range.")),
          tabPanel("Quartiles",
                   plotOutput("Quartiles"),
                   helpText("The coloured bars divide the plausible range for each expert
into four equally likely regions, as specified by each expert's quartiles. The quartiles displayed will either be the elicited quartiles,
if they have been provided,
                            or estimates obtained by linear interpolation of the elicited probabilities, with zero probability assumed
                            outside the plausible range."))
        )
        
      )
    )
  ))
  
  server <- shinyServer(function(input, output) {
    
    p <- reactive({
      tryCatch(eval(parse(text = paste("c(",
                                       input$probs, ")"))),
               error = function(e){c(NaN, NaN, NaN)})
    })
    
    lpweights <- reactive({
      eval(parse(text=paste("c(",input$weights,")")))
    })
    
    nExp <- reactive({
      req(input$nExperts)
      max(c(input$nExperts, 1))
    })
  
    l <- reactive({
      input$myvals[1, ] 
    })
    
    u <- reactive({
      as.numeric(tail(input$myvals, 1))
    })
    
    v <- reactive({
      n <- nrow(input$myvals)
      as.matrix(input$myvals[2:(n - 1), ])
    })
    
    myfit <- reactive({
      myfit<-fitdist(vals = v(),
                     probs = p(),
                     lower = l(),
                     upper = u())
    })
    
    output$setLPWeights <- renderUI({
      textInput("weights", label = h5("Linear pool weights"), 
                paste(rep(1, input$nExperts), collapse = ", "))
    })
    
    output$EnterJudgements <- renderUI({
      initialdf <- data.frame(matrix(c(0, 10, 20, 30, 100,
                                       0, 20, 30, 50, 150), 2 + length(p()), nExp()))
      
      shinyIncubator::matrixInput("myvals", "Expert judgements", initialdf)
    })
    
    output$ShowJudgements <- renderTable({
      data.frame(Judgement = c("L", p(), "U"))
    })
    
    quantileValues <- reactive({
      values <- qlinearpool(myfit(), c(input$fq1, input$fq2), 
                            d=dist[as.numeric(input$radio)], 
                            w = lpweights())
      data.frame(quantiles=c(input$fq1, input$fq2), values=values)
      
    }) 
    output$lpquantiles <- renderTable({
      quantileValues()
    })
    
    output$distPlot <- renderPlot({
      req(myfit())
      xlimits <- c(min(l()), max(u()))

            if(is.null(input$lp)){
        
          print(makeGroupPlot(myfit(), pl = xlimits[1], 
                              pu = xlimits[2], 
                              d=dist[as.numeric(input$radio)],
                              lwd = 1,
                              xlab = "x",
                              ylab = expression(f[X](x)),
                              fs = input$fs))}else{
        print(makeLinearPoolPlot(myfit(), xl = xlimits[1], 
                                 xu = xlimits[2], 
                                 d=dist[as.numeric(input$radio)], w = lpweights(), lwd = 1,
                                 xlab = "x",
                                 ylab = expression(f[X](x)), legend_full = input$leg ==1,
                                 ql = quantileValues()[1, 2], 
                                 qu = quantileValues()[2, 2],
                                 addquantile = input$showfeedback,
                                 fs = input$fs))
      }
      
    })
    
    output$Tertiles <- renderPlot({
      req(myfit())
      tertilevals <- matrix(0, 3, input$nExperts)
      for(i in 1:input$nExperts){
        tertilevals[, i] <- approx(c(0, p(), 1), 
                            input$myvals[,  i],
                            c(1/3, 0.5, 2/3))$y
      }
      plotTertiles(tertilevals, l(), u(), fs = input$fs)
      
    })
    
    output$Quartiles <- renderPlot({
      req(myfit())
      quartilevals <- matrix(0, 3, input$nExperts)
      for(i in 1:input$nExperts){
        quartilevals[, i] <- approx(c(0, p(), 1), 
                            input$myvals[,  i],
                            c(0.25, 0.5, 0.75))$y
      }
      plotQuartiles(quartilevals, l(), u(), fs = input$fs)
      
    })
    
    
  })
  
  ## run app 
  runApp(list(ui=ui, server=server))
  return(invisible())
}
