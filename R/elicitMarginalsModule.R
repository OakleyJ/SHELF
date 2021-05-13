# Module UI function
elicitMarginalsInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("nTheta"), h5("Number of categories"),
                     value = 3, min = 3, step = 1),
        uiOutput(ns("enterThetaLabels")),
        textInput(ns("probs"), label = h5("Cumulative probabilities"), 
                  value = "0.25, 0.5, 0.75"),
        uiOutput(ns("categoryToDisplay"))
      ),
      mainPanel(
        helpText("Enter the percentiles of your marginal distributions 
        in the table below, one column per category. The values in each column
        should correspond to the cumulative probabilities on the left, e.g., in
        the first column (with the default probabilities), enter the
        25th, 50th and 75th percentiles of your marginal distribution
                 for the population proportion in the first category."),
        uiOutput(ns("EnterJudgements")),
        plotOutput(ns("betaPlot"))
      )
    )
  )
  
}

elicitMarginals <- function(input, output, session, fs){
  
  
  
  thetaNames <- reactive({
    req(input$thetaLabels)
    temp <- unlist(strsplit(input$thetaLabels, ","))
    trimws(temp)
  })
  
  p <- reactive({
    eval(parse(text = paste("c(", input$probs, ")")))
  })
  
  output$enterThetaLabels <- renderUI({
    ns <- session$ns
    textInput(ns("thetaLabels"), h5("Category labels"),
              value = paste(LETTERS[1:input$nTheta], collapse = ", "))
  })
  
  output$categoryToDisplay <- renderUI({
    ns <- session$ns
    selectInput(ns("categoryDisplay"), label = h5("Category to display"),
                choices = c("No display", thetaNames()),
                selected = "No display")
  })
  
  
  
  
  output$EnterJudgements <- renderUI({
    req(thetaNames(), p(), input$nTheta)
    pvec <- (1 / input$nTheta) * p() / 0.5
    pvec[pvec>=1] <- 0.999
    pvec[pvec<=0] <- 0.001
    initialdf <- matrix(pvec,
                        length(p()),
                        input$nTheta)
    if(length(thetaNames()) == input$nTheta){
      colnames(initialdf) <- thetaNames()}
    rownames(initialdf) <- p()
    ns <- session$ns
    shinyMatrix::matrixInput(inputId = ns("myvals"), value =  initialdf,
                             class = "numeric",
                             cols = list(names = TRUE),
                             rows = list(names = TRUE))
  })
  
  allFits <- reactive({
    marginalFits <- vector("list", length = input$nTheta)
    for(i in seq_along(marginalFits)){
      marginalFits[[i]] <- fitdist(vals = input$myvals[, i],
                                   probs = p(),
                                   lower = 0, 
                                   upper = 1)
    }
    marginalFits
  })
  
  output$betaPlot <- renderPlot({
    req(input$categoryDisplay)
    if(input$categoryDisplay != "No display"){
      index <- which(input$categoryDisplay == thetaNames())
      plotfit(allFits()[[index]], d = "beta", ql = 0.05, qu = 0.95,
              xlab = paste0('Proportion in category "',
                            input$categoryDisplay,
                            '"'),
              ylab = "density",
              fs = fs())
    }
  })
  

  

  list(allFits = reactive({allFits()}),
       categoryLabels = reactive({thetaNames()}),
       thetaMatrix = reactive({input$myvals}),
       quantiles = reactive({p()}))
}