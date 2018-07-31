
shinyplotgroup<- function(fit, xl, xu, lpw, lwd, xlab, ylab, legend_full){
  
  #if(length(unique(fit$limits[,1]))>1 | length(unique(fit$limits[,2]))>1 ){stop("Parameter limits must be the same for each expert")}
  
  plotlimits <- paste(xl, xu , sep = ",")
  
  # Determine set of suitable distributions
  if(fit$limits[1, 1]>=-Inf & fit$limits[1, 2] < Inf){
    distributionchoices <- list("Histogram" = 1, "Normal" = 2, "Student t" = 3, "Gamma" = 4, "Log normal" = 5, "Log Student t" = 6, "Beta" = 7, "Best fitting" =8)
  }
  if(fit$limits[1, 1]>=-Inf & fit$limits[1, 2] == Inf){
    distributionchoices <- list("Histogram" = 1, "Normal" = 2, "Student t" = 3, "Gamma" = 4, "Log normal" = 5, "Log Student t" = 6, "Best fitting" =8)
  }
  if(fit$limits[1, 1]==-Inf & fit$limits[1, 2] == Inf){
    distributionchoices <- list("Histogram" = 1, "Normal" = 2, "Student t" = 3, "Best fitting" =8)
  }
 # if(fit$limits[1, 1]>-Inf & fit$limits[1, 1] < 0 & fit$limits[1, 2] < Inf){
#    distributionchoices <- list("Histogram" = 1, "Normal" = 2, "Student t" = 3, "Beta" = 7, "Best fitting" =8)
#  }
  
 
  
  ###
  
  
  runApp(list(
  ui = shinyUI(fluidPage(
    
    # Application title
    titlePanel("Individual fitted distributions"),
    
    
    sidebarLayout(
      sidebarPanel(
        textInput("xlimits", label = h5("x-axis limits"), value = plotlimits),
        radioButtons("radio", label = h5("Distribution"), choices = distributionchoices, selected = 1 ),
   
   checkboxGroupInput("lp", label = h5("Linear pool"), 
                      choices = list("Display linear pool" = 1)),
   textInput("weights", label = h5("Linear pool weights"), 
             paste(rep(1, nrow(fit$vals)), collapse = ", ")),
   radioButtons("leg", label = h5("Linear pool legend"),
                choices = list("full" = 1, "reduced" = 2), selected = 1 )
                     
      ),
            mainPanel(
        plotOutput("distPlot"),
        
          tableOutput("values")
        
      )
    )
  )),
   
  server = function(input, output) {
    
    lpweights <- reactive({
      eval(parse(text=paste("c(",input$weights,")")))
    })
    
    
    fit <- get("fit")
    output$distPlot <- renderPlot({
      xlimits<-eval(parse(text=paste("c(",input$xlimits,")")))
      dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      if(is.null(input$lp)){
        if(input$radio == 0){
          print(plotTertiles(fit$vals, fit$limits[, 1], fit$limits[, 2]))
          
        }else{
        print(makeGroupPlot(fit, pl = xlimits[1], 
                                 pu = xlimits[2], 
                                 d=dist[as.numeric(input$radio)],
                            lwd, xlab, ylab))}
      }else{
        print(makeLinearPoolPlot(fit, xl = xlimits[1], 
                                 xu = xlimits[2], 
                                 d=dist[as.numeric(input$radio)], w = lpweights(), lwd, 
                                 xlab, ylab, legend_full = input$leg ==1))
      }
     
    })
}
))
}
