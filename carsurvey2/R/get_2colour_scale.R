#' Create 2 colour scale palette
#'
#' Creates a list of  colour names (RGB). Generates a scale between two colours. By default these are the
#' shades of orange and blue used in the analysis function colour scheme.
#'
#' @param n the number of colours needed
#' @param colour1 the first colour in the scale - a numeric vector representing red, green and blue (max 255)
#' @param colour2 the last colour in the scale - numeric vector representing red, green and blue (max 255).
#'
#' @return vector of hexadecimal colours containing shades between the two selected colours
#'
#' @export

get_2colour_scale <- function(n, colour1 = c(0, 69, 86), colour2 = c(255, 105, 0)) {
  
  if (!is.numeric(n) | length(n) > 1) {
    stop("n is not a numeric value")
  } else if (n < 2) {
    stop("Unexpected value - n should be >= 2")
  } else if (class(colour1) != "numeric" | class(colour2) != "numeric") {
    stop("colours are not a vector of three integers")
  } else if (length(colour1) != 3 | length(colour2) != 3) {
    stop("colours must contain three values")
  }
  
  if (n != 2) {
    step  <-  (colour2 - colour1) / (n - 1)
    colours <- unname(
      lapply(c(1:(n - 1)),
             function(x){
               step * x + colour1
             }
      )
    )  
    colours <- c(list(colour1), colours)
  } else {
    colours <- list(colour1, colour2)
  }
  return(colours)

}
