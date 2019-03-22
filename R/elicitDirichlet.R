#' Elicit a Dirichlet distribution interactively
#' 
#' Opens up a web browser (using the shiny package), from which you can elicit a
#' Dirichlet distribution
#' 
#' Click on the "Help" tab for instructions. Click the "Quit" button to exit the app and return
#' the results from the \code{fitdist} command. Click "Download report" to generate a report
#' of all the fitted distributions.
#'   
#' @return The parameters of the fitted Dirichlet distribution, which are 
#' returned once the Quit button has been clicked. 
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

elicitDirichlet <- function(){
  

  runApp(list(
    ui = fluidPage(
  
  # Application title
  titlePanel("SHELF: eliciting a Dirichlet distribution"),
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
    tabPanel("Elicit marginals",
             elicitMarginalsInput("marginals")
    ),
    tabPanel("Fitted Dirichlet",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("nFitted", h5("Fitted parameter sum"),
                              choices = list("best fitting" = "opt",
                                             "conservative" = "min",
                                             "median" = "med",
                                             "mean" = "mean")
                 ),
                 textInput("fq", h5("Feedback quantiles"),
                           value = "0.05, 0.95")
               ),
               
               mainPanel(
                 helpText("The plot below shows the marginal distributions from 
                          the fitted Dirichlet, together with the directly 
                          elicited marginal distributions."),
                 plotOutput("DirichletPlot"),
                 tableOutput("feedback")
               )
             )
             
             
    ),
    tabPanel("Conditional distributions",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("categoryToFix"),
                 numericInput("obsValue", h5("Observed value"),
                              min = 0, max = 1, value = 0.5)
               ),
               mainPanel(
                 helpText("Choose one category to fix, and a hypothetical value
                          to observe as the true propotion in that category. The
                          plot shows the marginal distributions from the 
                          Dirichlet fit, together with the updated conditional
                          distributions, given the hypothetical 
                          observed value."),
                 plotOutput("conditionalPlot")
               )
             )
             
             
    ),
    tabPanel("Help",
             includeHTML(system.file("shinyAppFiles", "DirichletHelp.html",
                                     package="SHELF")))
    
  )
  
),


server = function(input, output) {
  
  # Hack to avoid CRAN check NOTE
  
  Category <- fx <- x <- parameters <- NULL
  
  fittedDirichlet <- reactive({
    fitDirichlet(theta$allFits(), categories = theta$categoryLabels(),
                 n.fitted = input$nFitted,
                 plotBeta = FALSE, silent = TRUE)
  })
  
  
  marginalFs <- reactive(input$fs)

  theta <- callModule(elicitMarginals, "marginals", marginalFs)
  
  
  output$DirichletPlot <- renderPlot({
    fitDirichlet(theta$allFits(), categories = theta$categoryLabels(),
                 n.fitted = input$nFitted, silent = TRUE,
                 fs = input$fs)
  })
  
  output$categoryToFix <- renderUI({
    selectInput("categoryFixed", label = h5("Category to fix"),
                choices = theta$categoryLabels(),
                selected = theta$categoryLabels()[1])
  })
  
  output$conditionalPlot <- renderPlot({
    req(input$categoryFixed)
    d<- fittedDirichlet()
    selected <- which(input$categoryFixed == names(d))
    
    getbetadensity <- function(x) {
      dbeta((0:100) / 100, x[1], x[2])
    }
    
    m1 <- matrix(c(d, sum(d) - d), nrow = 2, byrow = TRUE)
    
    df1 <- data.frame((0:100) / 100,
                      sapply(data.frame(m1),
                             getbetadensity))
    
    names(df1) <- c("x", names(d))
    df1 <- tidyr::gather(df1,
                         key = Category,
                         value = fx,-x,
                         factor_key = TRUE)
    df1 <- data.frame(df1, parameters = "marginal")
    
    getbetaConditionaldensity <- function(x, y) {
      dbeta(seq(from = 0, to = 1, length = 101),
            x[1], x[2]) / (1-input$obsValue)
    }
    
    m2 <- abs(matrix(c(d, sum(d[- selected])- d), nrow = 2, byrow = TRUE)) 
    # abs to avoid warnings from negative beta parameters
    # 'density' with negative parameters will not be plotted in any case
    
    
    
    df2 <- data.frame(seq(from = 0 , to = (1-input$obsValue),
                          length = 101),
                      sapply(X = data.frame(m2), 
                             FUN = getbetaConditionaldensity, 
                             y = input$obsValue))
    
    names(df2) <- c("x", names(d))
    df2 <- tidyr::gather(df2,
                         key = Category,
                         value = fx,-x,
                         factor_key = TRUE)
    df2 <- data.frame(df2, parameters = "conditional")
    
    df.all <- rbind(df1, df2)
    index<-df.all$Category == names(d)[selected] &
      df.all$parameters == "conditional"
    df.all[index, 1] <- 0.5
    df.all[index, 3] <- 0
    
    
    p1 <- ggplot(df.all, aes(x = x, y = fx)) +
      geom_line(aes(colour = parameters)) +
      facet_wrap(~ Category, ncol = 1) +
      labs(colour = " Distribution") +
      ylab(expression(f[X](x)))+
      geom_vline(data = subset(df.all, Category == names(d)[selected]),
                 aes(xintercept=input$obsValue),  colour="#00BFC4") +
      theme_grey(base_size = input$fs)

    print(p1)
    
  })
  
  output$feedback <- renderTable({
    
    feedbackDirichlet(fittedDirichlet(),
                      quantiles = 
                        eval(parse(text = paste("c(", input$fq, ")")))
    )
  })
  
  observeEvent(input$exit, {
    stopApp(fittedDirichlet())
  }) 
  
  output$report <- downloadHandler(
    filename = function(){switch(input$outFormat,
                                 html_document = "Dirichlet-report.html",
                                 pdf_document = "Dirichlet-report.pdf",
                                 word_document = "Dirichlet-report.docx")},
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "DirichletShinySummary.Rmd")
      file.copy(system.file("shinyAppFiles", "DirichletShinySummary.Rmd",
                            package="SHELF"),
                tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(allFits = theta$allFits(),
                     categories = theta$categoryLabels(),
                     n = input$nFitted,
                     thetaMatrix = theta$thetaMatrix(),
                     quantiles = theta$quantiles())
      
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

# Run the application 



))}