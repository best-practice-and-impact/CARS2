#' Create single colour gradient
#'
#' Creates a list of colours for plotting. The list contains n shades of the original colour, increasing in brightness.
#'
#' @param n the number of colours needed
#' @param colour the first colour of the gradient - numeric vector with three values representing red, blue and green (max 255)
#'
#' @return RGB colours
#'
#' @export

get_gradient <- function(n, colour = c(32, 96, 149)) {
  
  if (!is.numeric(n) | length(n) > 1) {
    stop("Unexpected value - n is not a numeric value")
  } else if (!is.numeric(colour) | length(colour) != 3) {
    stop("colour is not a vector of three integers")
  }  
  
  if (n == 1){
    return(list(colour))
  } else {
    # Calculate lighter shade of original colour
    c2 <- colour + (255 - colour) * 0.5
    
    step <- (c2 - colour) / (n - 1)
    
    colours <- unname(
      lapply(c(1:(n - 1)),
             function(x){
               step * x + colour
             })
    )
    
    colours <- append(list(colour), colours)
    return(colours)
  }
}
