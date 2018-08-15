#' Plot conditional distributions from an elicited Dirichlet prior
#' 
#' Opens up a web browser (using the shiny package), from which you can choose to condition
#' on one of the category probability values, and then display the resulting conditional marginal
#' distributions for the remaining categories
#' 
#' 
#' Press Esc in the R console window to exit the elicitation session.
#' @param d A fitted Dirichlet distribution, produced from a \code{fitDirichlet} command.
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' p1 <- c(0.25, 0.5, 0.75)
#' v1 <- c(0.5, 0.55, 0.6)
#' v2 <- c(0.22, 0.3, 0.35)
#' v3 <- c(0.11, 0.15, 0.2)
#' myfit1 <- fitdist(v1, p1, 0, 1)
#' myfit2 <- fitdist(v2, p1, 0, 1)
#' myfit3 <- fitdist(v3, p1, 0, 1)
#' d <- fitDirichlet(myfit1, myfit2, myfit3,
#'                   categories = c("A","B","C"),
#'                   n.fitted = "opt")
#' condDirichlet(d)
#' }
#' @import shiny
#' @import ggplot2
#' @export
condDirichlet<- function(d){
  
  Category <- x <- fx <- parameters<- NULL # hack to avoid R CMD check NOTE
  
  
  runApp(list(
    ui = shinyUI(fluidPage(
      
      # Application title
      titlePanel("Elicitation"),
      
      # Sidebar with a slider input for the number of bins
      sidebarLayout(
        sidebarPanel(
          selectInput('category', 'Category',
                      choices = names(d)),
          numericInput("x", label = "Category probability value", min = 0, 
                      max = 1, value = round(d[1]/sum(d), 1), step = 0.01)
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      )
    )),
    
    server = function(input, output) {
      
      output$distPlot <- renderPlot({
        
        selected <- which(input$category == names(d))
        
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
                x[1], x[2]) / (1-input$x)
        }
        
        m2 <- abs(matrix(c(d, sum(d[- selected])- d), nrow = 2, byrow = TRUE)) 
        # abs to avoid warnings from negative beta parameters
        # 'density' with negative parameters will not be plotted in any case
        
        
        
        df2 <- data.frame(seq(from = 0 , to = (1-input$x),
                              length = 101),
                          sapply(X = data.frame(m2), 
                                 FUN = getbetaConditionaldensity, 
                                 y = input$x))
        
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
                     aes(xintercept=input$x),  colour="#00BFC4")
        print(p1)
        
        
        
      })
     
      
    }
  ))
}

