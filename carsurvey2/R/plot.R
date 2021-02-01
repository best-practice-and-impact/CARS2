
#'@title Plot frequency graph
#'
#'@description Produce bar chart (plotly) for single factor frequency data. 
#'
#'@param table Frequency table (data frame). 2 columns - cateogry names and frequencies. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param bar_colour Colour name. Defaults to blue (see get_gradient())
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#'@param break_q_names_col applies break_q_names to the column. Not applied by default
#'@param max_lines maximum number of lines. Int, defaults to 2/ See carsurvey::break_q_names()
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_freqs <- function(table, xlab, ylab, bar_colour, n, font_size = 12, orientation = "v", break_q_names_col = NULL, max_lines = 2, ...) {
  
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
  
  # Apply break_q_names to a column
  if(!is.null(break_q_names_col)) {
    # Coerce to character type
    table[[break_q_names_col]] <- as.character(table[[break_q_names_col]])
    
    table[[break_q_names_col]] <- break_q_names(table[[break_q_names_col]], max_lines = max_lines)
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
                          hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                          margin = list(b = 100),
                          annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                             showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                             xref='paper', yref='paper', font=list(size = font_size))
    )
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
                          hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                          margin = list(b = 100),
                          annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                             showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                             xref='paper', yref='paper', font=list(size = font_size))
    )
  } 
  
  
  return(fig)
  
}

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
  suppressMessages(
    longdata <- reshape2::melt(table)
  )

  
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

#'@title Plot likert graph
#'
#'@description Produce likert stacked bar chart (plotly). At least 2 questions per plot.
#'
#'@param table Frequency table for likert quesitons (data frame). 4+ columns - question names in column 1 with answer options in subsequent columns. Frequencies should proportions, between 0 and 1. 
#'@param mid the mid-point of the scale. should be higher than 2 and lower than the number of answers.
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param neutral_mid whether the middle of the scale should be a neutral category (logical). TRUE by default
#'@param break_q_names_col applies break_q_names to the column. Not applied by default
#'@param ... additional plot_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_likert <- function(table, mid, xlab, ylab, n, font_size = 12, neutral_mid = TRUE, break_q_names_col =NULL, ...) {
  
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
  
  # Validate n
  if ((!is.numeric(n) & !is.character(n)) | length(n) > 1) {
    stop("Unexpected input - n is not a single number or string")
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
  } else if (neutral_mid & mid > ncol(table)-2) {
    stop("Unexpected input - mid >= the number of answers.")
  } else if (neutral_mid & mid > ncol(table)-1) {
    stop("Unexpected input - mid >= the number of answers.")
  }
  
  # Validate neutral mid
  if (!is.logical(neutral_mid)) {
    stop("Unexpected input - mid is not logical (TRUE/FALSE)")
  }
  
  # Apply break_q_names to a column
  if(!is.null(break_q_names_col)) {
    table[[break_q_names_col]] <- break_q_names(table[[break_q_names_col]])
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
  suppressMessages(
    longdata <- reshape2::melt(table)
  )
  
  # Calculate bases for bars
  bases <- apply(table[2:ncol(table)], 1, cumsum)
  bases <- as.vector(apply(bases, 1, function(x) return(unname(unlist(x)))))
  bases <- utils::head(bases, -nrow(table))
  bases <- c(rep(0, nrow(table)), bases)
  
  if (neutral_mid) {
    negative_bases <- rowSums(table[c(2:mid)]) + table[mid + 1]/2  
  } else {
    negative_bases <- rowSums(table[c(2:mid)])
  }
  
  negative_bases <- unname(unlist(negative_bases))
  bases <- bases - negative_bases
  
  # Get bar colours
  if (neutral_mid) {
    colours <- get_2colour_gradients(ncol(table)-1, mid = mid, neutral_mid = neutral_mid)
  } else {
    colours <- get_2colour_gradients(ncol(table)-1, mid = mid-1, neutral_mid = neutral_mid)
  }
  
  colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255))
  colours <- lapply(colours, function(x) rep(x, nrow(table)))
  colours <- unlist(colours)
  
  hovertext <- paste0(
    paste0(longdata[[2]], "<br>"), 
    paste0(round(abs(longdata[[3]]) * 100, 1), "%")
  )
  
  fig <- plotly::plot_ly(y = longdata[[1]], 
                         x=longdata[[3]], 
                         type="bar", 
                         color = longdata[[2]], 
                         orientation = "h", 
                         base = bases,
                         hoverinfo = "text",
                         text = hovertext,
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
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)),
                        xaxis = x, 
                        yaxis = y, 
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)))
  
  # Disable interactive legend
  
  id <- paste0("plot", stringi::stri_rand_strings(1, 10))
  javascript <- paste0(id, ".on('plotly_legenddoubleclick', function(d, i) {return false});",
                       id, ".on('plotly_legendclick', function(d, i) {return false});")
  
  fig$elementId <- id
  fig <- htmlwidgets::prependContent(fig, htmlwidgets::onStaticRenderComplete(javascript), data=list(''))
  
  return(fig)
  
}

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
  n_groups <- length(unique(table[[2]]))
  c <- get_2colour_scale(n_groups)
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