#'@title Plot likert graph
#'
#'@description Produce likert stacked bar chart (plotly). At least 2 questions per plot.
#'
#'@param table Frequency table for likert quesitons (data frame). 4+ columns - question names in column 1 with answer options in subsequent columns. Frequencies should proportions, between 0 and 1. 
#'@param mid the mid-point of the scale. should be higher than 2 and lower than the number of answers.
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
  } else if (ncol(table) < 4 | nrow(table) < 2) {
    stop("Unexpected input - table should have at least four columns and two rows.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }
  
  # Validate mid
  if (!is.numeric(mid)) {
    stop("Unexpected input - mid is not numeric.")  
  } else if (mid < 2) {
    stop("Unexpected inout - mid is smaller than 2.")
  } else if (mid > ncol(table)-2) {
    stop("Unexpected input - mid >= the number of answers.")
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
  longdata <- reshape2::melt(table)
  
  # Calculate bases for bars
  bases <- apply(table[2:ncol(table)], 1, cumsum)
  bases <- as.vector(apply(bases, 1, function(x) return(unname(unlist(x)))))
  bases <- utils::head(bases, -nrow(table))
  bases <- c(rep(0, nrow(table)), bases)
  
  negative_bases <- rowSums(table[c(2:mid)]) + table[mid + 1]/2
  negative_bases <- unname(unlist(negative_bases))
  bases <- bases - negative_bases
  
  # Get bar colours
  colours <- get_2colour_gradients(ncol(table)-1, mid = mid)
  colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255))
  colours <- lapply(colours, function(x) rep(x, nrow(table)))
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
                                      traceorder = "normal",
                                      font = list(size = font_size)), 
                        xaxis = x, 
                        yaxis = y, 
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)))
  
  return(fig)
  
}