#'@title Plot grouped frequency graph
#'
#'@description Produce bar chart (plotly) for frequency data with grouping variable. 
#'
#'@param table Frequency table (data frame). 3 columns - cateogry names, groups and frequencies. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default.
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_grouped <- function(table, xlab, ylab, n, font_size = 12, orientation = "v", ...) {
  
  # Set default bar colours
  n <- length(unique(table[[2]]))
  c <- get_2colour_scale(n)
  colours <- unlist(lapply(c, function(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))) 
  colours <- unlist(colours)
  colours <- rep(colours, c(unlist(table(table[[2]]))))
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) != 3) {
    stop("Unexpected input - table does not contain 3 columns.")
  } else if (!is.numeric(table[[3]])) {
    stop("Unexpected input - table column 3 is not numeric.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate n
  if ((!is.numeric(n) & !is.character(n)) | length(n) > 1) {
    stop("Unexpected input - n is not a single number or string")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }
  
  # Validate orientation
  if (!(orientation %in% c("h", "v"))) {
    stop("Unexpected input - orientation should be set to 'h' or 'v'")
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  y <- list(
    title = ylab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  if (orientation == "v") {
    fig <- plotly::plot_ly(
      x = table[[1]],
      y = table[[3]],
      color = table[[2]],
      marker = list(color = colours),
      type = "bar",
      ...
    )
    
    fig <- plotly::config(fig, displayModeBar = F)
    fig <- plotly::layout(fig,  
                          xaxis = x, 
                          yaxis = y, 
                          hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                          annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                             showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                             xref='paper', yref='paper', font=list(size = font_size))
    )
                          
  } else if (orientation == "h") {
    fig <- plotly::plot_ly(
      x = table[[3]],
      y = table[[1]],
      color = table[[2]],
      marker = list(color = colours),
      type = "bar",
      ...
    )
    
    fig <- plotly::config(fig, displayModeBar = F)
    fig <- plotly::layout(fig,  
                          orientation = "h",
                          xaxis = y, 
                          yaxis = x, 
                          hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                          legend = list(traceorder = "reversed"),
                          margin = list(b = 100),
                          annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                             showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                             xref='paper', yref='paper', font=list(size = font_size))
                          )
  } 

  
  return(fig)
  
}