#'@title Plot likert graph
#'
#'@description Produce likert stacked bar chart (plotly). 
#'
#'@param table Frequency table for likert quesitons (data frame). 2+ columns - question names in column 1 with answer options in subsequent columns. 
#'@param mid the mid-point of the scale
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param font_size minimum font size for the plot (numeric).
#'
#'@return bar chart
#'
#'@export

plot_likert <- function(table, mid, xlab, ylab, font_size = 12) {
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) < 2) {
    stop("Unexpected input - table should have at least two columns")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    Stop("Unexpected input - font_size is not numeric.")
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2),
    range = list(-1, 1), 
    tickformat = "%", title = "Percent"
  )
  
  y <- list(
    title = ylab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  # Reshape data
  longdata <- reshape2::melt(data)
  
  # Calculate bases for bars
  bases <- c()
  
  for (i in 2:mid) {
    bases <- c(bases, (-(rowSums(data[c(i:mid)]) + data[mid + 1] / 2)))
  }
  
  bases <- c(bases, -(data[mid + 1] / 2), (data[mid + 1] / 2))
  
  if (mid + 2 < length(table)) {
    for (i in (mid + 2):(ncol(data)-1)) {
      bases <- c(bases, (data[mid + 1] / 2 + data[i]))
    }
  }
  
  bases <- unname(unlist(bases))
  
  # Get bar colours
  colours <- get_2colour_gradients(ncol(data)-1, mid = mid)
  colours <- lapply(colours, function(x) rgb(x[1], x[2], x[3], max = 255))
  colours <- lapply(colours, function(x) rep(x, ncol(data)-1))
  colours <- unlist(colours)
  
  fig <- plotly::plot_ly(y = longdata[[1]], 
                         x=longdata[[3]], 
                         type="bar", 
                         color = longdata[[2]], 
                         orientation = "h", 
                         base = bases,
                         hoverinfo = "text",
                         text = longdata[[3]],
                         marker = list(color = colours))
  
  fig <- plotly::config(fig, displayModeBar = F)
  
  
  fig <- plotly::layout(fig, 
                        barmode = "stack", 
                        clickmode = "none",
                        legend = list(orientation = "h",   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      x = 0.5,
                                      y = 1.02,
                                      traceorder = "normal",
                                      font = list(size = font_size)), 
                        xaxis = x, 
                        yaxis = y, 
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)))
  
  return(fig)
  
}