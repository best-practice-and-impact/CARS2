#'@title Plot frequency graph
#'
#'@description Produce bar chart (plotly) for single factor frequency data. 
#'
#'@param table Frequency table (data frame). 2 columns - cateogry names and frequencies. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param bar_colour Colour name. Defaults to blue (see get_gradient())
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_freqs <- function(table, xlab, ylab, bar_colour, font_size = 12, orientation = "v", ...) {
  
  # Set default bar colour
  if (missing(bar_colour)) {
    c <- unlist(get_gradient(1))
    bar_colour <- grDevices::rgb(c[1], c[2], c[3], max = 255)
  } else if (!is.character(bar_colour) | length(bar_colour) != 1) {
    stop("Unexpected input - bar_colour should be a single colour name.")
  }
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) != 2) {
    stop("Unexpected input - table does not contain two columns.")
  } else if (!is.numeric(table[[2]])) {
    stop("Unexpected input - table column 2 is not numeric.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
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
      y = table[[2]],
      marker = list(color = bar_colour),
      type = "bar",
      ...
    )
    
    fig <- plotly::config(fig, displayModeBar = F)
    fig <- plotly::layout(fig,  
                          xaxis = x, 
                          yaxis = y, 
                          hoverlabel = list(bgcolor = "white", font = list(size = font_size)))
  } else if (orientation == "h") {
    fig <- plotly::plot_ly(
      x = table[[2]],
      y = table[[1]],
      marker = list(color = bar_colour),
      type = "bar",
      ...
    )
    
    fig <- plotly::config(fig, displayModeBar = F)
    fig <- plotly::layout(fig, 
                          orientation = "h",
                          xaxis = y, 
                          yaxis = x, 
                          hoverlabel = list(bgcolor = "white", font = list(size = font_size)))
  } 

  
  return(fig)
  
}