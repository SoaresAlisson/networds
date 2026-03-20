#' Change the vector min and max values proportionally
#'
#' @description
#' Used to change the display of values in plot_graph2
#'
#' @param x vector of values
#' @param new_min new minimum value. Default 0.15
#' @param new_max new maximum value. Default 8
#'
#' @export
#'
#' @examples
#' 1:5 |> scale_to_range(0, 1)
#' 1:5 |> scale_to_range(2, 12)
scale_to_range <- function(x, new_min = 0.15, new_max = 8) {
  # Error handling
  x <- unlist(x)

  # if (length(x) < 2) stop("Vector must have at least 2 values")
  if (new_min >= new_max) {
    stop("new_min must be less than new_max")
  }

  # Check if all values are the same
  if (max(x) == min(x)) {
    warning("All values are identical. Returning all values as new_min")
    return(rep(new_min, length(x)))
  }
  # First normalize to 0-1
  normalized <- (x - min(x)) / (max(x) - min(x))

  # Then scale to new range
  scaled <- normalized * (new_max - new_min) + new_min

  return(scaled)
}
