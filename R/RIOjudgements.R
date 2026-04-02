# Helper functions for RIO judgements in Shiny apps

RIOJudgementsPlot <- function(L, U, nRIOprobs = 20, X1 = NA, X2 = NA, X3 = NA,
                              P1 = NA, P2 = NA, P3 = NA,
                              show_X2 = FALSE,
                              show_X3 = FALSE,
                              fs = 12){
  
  # Hack to avoid CRAN check NOTE
  
  x <- y <- label <- NULL
  
  
  # shiny apps may send input values as NULL - switch to NA
  if(is.null(X1)){X1 <- NA}
  if(is.null(P1)){P1 <- NA}
  if(is.null(X2)){X2 <- NA}
  if(is.null(X3)){X3 <- NA}
  if(is.null(P2)){P2 <- NA}
  if(is.null(P3)){P3 <- NA}
  
  
  if(L>=U){
    return(NULL)
  }
  
  # set up nullPlot if errors in input
  
  breaks <- c(L, U)
  axisLabels <- c(paste0(L, "\n\nL"), paste0(U, "\n\nU"))
  n_rem <- round(nRIOprobs)
  pool_data <- data.frame()
  x_range <- U - L
  pool_x_mid <- mean(c(L, U))
  cols <- ceiling(nRIOprobs/2) 
  rows <- 2 
  gap <- (U-L)/(cols + 1)
  pool_data <- expand.grid(x = seq(from  = gap, to = U-gap, by = gap), row = 1:rows)[1:n_rem, ]
  pool_data$y <- 24.5 + pool_data$row * 1.5
  
  
  
  nullPlot <-  ggplot() +
    geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
    annotate("rect", xmin = L,
             xmax = U, ymin = 24, ymax = 29.5, 
             fill = "grey98", color = "grey80", linetype = "dashed")+
    annotate("text", x = mean(c(L, U)), y = 30.5,
             label = paste0("Unallocated probs: ", n_rem), fontface = "italic") +
    scale_x_continuous(breaks = breaks, labels = axisLabels, limits = c(L, U)) +
    scale_y_continuous(limits = c(-1, 32), expand = c(0,0)) +
    theme_minimal(base_size = fs) +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold",
                                      margin = margin(t = 20))) +
    labs(x = "Value X") +
    geom_point(data = pool_data, aes(x = x, y = y), color = "grey75", size = 6)
    
  
  xVals <- na.omit(c(L, X1, X3, X2, U))
  if(any(diff(xVals)<=0)){
    nullPlot <- nullPlot+ 
      annotate("text", x =pool_x_mid, y = 10, label = "check that X1 < X3 < X2")
    return(nullPlot)
  }
  pVals <- na.omit(c(P1, P2, P3))
  if(any(pVals<0) | any(pVals >1) | sum(pVals) >1){
    nullPlot <- nullPlot+ 
      annotate("text", x =pool_x_mid, y = 10, label = "check probabilities are between 0 and 1 and sum does not exceed 1.")
    return(nullPlot)
  }

  
  # Define vertical lines and axis labelling
  breaks <- L
  vLines <- c()
  axisLabels <- paste0(L, "\n\nL")
  if(!is.na(X1)){breaks <- c(breaks, X1)
  vLines <- X1
  axisLabels <- c(axisLabels, paste0(X1, "\n\nX1"))
  } 
  if(show_X2 && !is.na(X2)) {breaks <- c(breaks, X2)
  vLines <- c(vLines, X2)
  axisLabels <- c(axisLabels, paste0(X2, "\n\nX2"))
  } 
  if(show_X3 && !is.na(X3)) {breaks <- c(breaks, X3)
  vLines <- c(vLines, X3)
  axisLabels <- c(axisLabels, paste0(X3, "\n\nX3"))
  } 
  breaks <-c(breaks, U)
  axisLabels <- c(axisLabels, paste0(U, "\n\nU"))
  
  
  counter_data <- data.frame(x = numeric(), y = numeric())
  label_data <- data.frame(x = numeric(), y = numeric(), label = character())
  
  # Logic for Pool Remainder: 
  # Pool exists if X3 hasn't been fully specified yet.
  p_assigned_to_pool <- 0
  if(!is.na(P1)) p_assigned_to_pool <- p_assigned_to_pool + P1
  if(show_X2 && !is.na(P2)) p_assigned_to_pool <- p_assigned_to_pool + P2
  
  # If X3 is shown and filled, the remainder is 0 because the whole range is now partitioned
  full_partition <- show_X3 && !is.na(X3) && !is.na(P3)
  p_rem <- if(full_partition) 0 else max(0, 1 - p_assigned_to_pool)
  
  # --- Pool of Remaining Discs ---
  n_rem <- round(p_rem * nRIOprobs)
  pool_data <- data.frame()
  if(n_rem > 0) {
    x_range <- U - L
    pool_x_mid <- mean(c(L, U))
    cols <- ceiling(nRIOprobs/2) 
    rows <- 2 
    gap <- (U-L)/(cols + 1)
    pool_data <- expand.grid(x = seq(from  = L + gap, to = U-gap, by = gap), row = 1:rows)[1:n_rem, ]
    #pool_data <- expand.grid(col = 1:cols, row = 1:rows)[1:n_rem, ]
    #pool_data$x <- pool_x_mid - (x_range*0.2) + (pool_data$col * (x_range*0.4/(cols+1)))
    pool_data$y <- 24.5 + pool_data$row * 1.5
  }
  
  # Helper function for stacks
  add_stack <- function(df, x1, x2, p, labs) {
    n <- round(p * nRIOprobs)
    mid_x <- (x1 + x2) / 2
    if(n > 0 & n < 11) {
      new_df <- data.frame(x = rep(mid_x, n),
                           y = seq(1, by = 2, length.out = n))
      df <- rbind(df, new_df)
      labs <- rbind(labs, data.frame(x = mid_x, y = 22,
                                     label = paste0(round(p*100), "%")))
    }
    if(n >= 11){
      ncol <- ceiling(n / 10)
      gap <- (x2 - x1) / (ncol + 1)
      new_df <- expand.grid(x = seq(x1 + gap, by = gap, length.out = ncol),
                            y = seq(1, by = 2, length.out = n) )[1:n, ]
      df <- rbind(df, new_df)
      labs <- rbind(labs, data.frame(x = mid_x, y = 22, label = paste0(round(p*100), "%")))
    }
    return(list(df = df, labs = labs))
  }
  
  # --- Process specified intervals ---
  # 1. [L, X1]
  if(!is.na(X1) && !is.na(P1)) {
    res <- add_stack(counter_data, 
                     L, X1, P1, label_data)
    counter_data <- res$df; label_data <- res$labs
  }
  
  # 2. [X2, U]
  if(show_X2 && !is.na(X2) && !is.na(P2)) {
    res <- add_stack(counter_data, X2, U,
                     P2, label_data)
    counter_data <- res$df; label_data <- res$labs
  }
  
  # 3. Handle X3 splits
  if(full_partition) {
    # Interval [X1, X3] 
    res <- add_stack(counter_data, X1, X3,
                     P3, label_data)
    counter_data <- res$df; label_data <- res$labs
    
    # Interval [X3, X2] is 1 - P1 - P2 - P3
    p_mid_high <- 1 - P1 - P2 - P3
    res <- add_stack(counter_data, X3, X2, p_mid_high, label_data)
    counter_data <- res$df; label_data <- res$labs
  }
  
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
    geom_text(data = label_data, aes(x = x, y = y, label = label), fontface = "bold", size = fs/2) +
    # Pool UI
    annotate("rect", xmin = L,
             xmax = U, ymin = 24, ymax = 29.5, 
             fill = "grey98", color = "grey80", linetype = "dashed") +
    annotate("text", x = mean(c(L, U)), y = 30.5,
             label = paste0("Unallocated probs: ", n_rem)) +
    scale_x_continuous(breaks = breaks, labels = axisLabels, limits = c(L, U)) +
    scale_y_continuous(limits = c(-1, 32), expand = c(0,0)) +
    theme_minimal(base_size = fs) +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold",
                                      margin = margin(t = 20))) +
    labs(x = "Value X")
  
  if(nrow(pool_data) > 0) {
    p <- p + geom_point(data = pool_data, aes(x = x, y = y), color = "grey75", size = 6)
  }
  if(nrow(counter_data) > 0){
    p <- p +  geom_point(data = counter_data, aes(x = x, y = y), color = "steelblue", size = 8) 
  }
  
  if(length(vLines) > 0){
    p <- p + geom_segment(aes(x = vLines, xend = vLines, y = 0, yend = 23), color = "black", linewidth = 1)
  }
  p
}