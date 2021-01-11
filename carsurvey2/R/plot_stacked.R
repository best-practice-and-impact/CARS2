#'@title Plot stacked bar graph
#'
#'@description Produce stacked bar chart (plotly). 
#'
#'@param table Frequency table for stacked bar chart (data frame). 3+ columns - sub-question names in column 1 with answer options in subsequent columns.. 
#'@param colour_scale type of colour scale ("gradient", "scale" or "2gradients"). See get_gradient(), get_2colour_scale() and get_2colour_gradients(). 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_stacked <- function(table, colour_scale = "2gradients", xlab, ylab, n, font_size = 12, ...) {
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) < 3) {
    stop("Unexpected input - table should have at least three columns")
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
  
  # Validate colour_scale
  if (length(colour_scale) > 1 | !colour_scale %in% c("gradient", "scale", "2gradients")) {
    stop("Unexpected input - colour_scale should be set to 'gradient', 'scale' or '2gradients'.")
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
  
  # Reshape data
  longdata <- reshape2::melt(table)
  
  # Get bar colours
  ncolours <- ncol(table) - 1
  if (colour_scale == "gradient") {
    colours <- get_gradient(ncolours)
  } else if (colour_scale == "scale") {
    colours <- get_2colour_scale(ncolours)
  } else if (colour_scale == "2gradients") {
    mid <- ceiling(ncolours/2)
    colours <- get_2colour_gradients(ncol(table)-1, mid = mid)
  }
  
  colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255))
  colours <- lapply(colours, function(x) rep(x, nrow(table)))
  colours <- unlist(colours)
  
  fig <- plotly::plot_ly(y = longdata[[1]], 
                         x=longdata[[3]], 
                         type="bar", 
                         color = longdata[[2]], 
                         orientation = "h", 
                         hoverinfo = "text",
                         text = longdata[[3]],
                         marker = list(color = colours),
                         ...)
  
  fig <- plotly::config(fig, displayModeBar = F)
  
  
  fig <- plotly::layout(fig,  
                        barmode = "stack", 
                        clickmode = "none",
                        legend = list(orientation = "h",   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      yanchor = "bottom",
                                      x = 0.5,
                                      y = 1,
                                      traceorder = "normal",
                                      font = list(size = font_size)),
                        margin = list(b = 100),
                        xaxis = x, 
                        yaxis = y, 
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)
                                           )
  )
  
  return(fig)
  
}