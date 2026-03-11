#' Filter both nodes at the same time
#'
#' @description
#' filter the first two columns of graph dataframe
#'
#' @param DF Dataframe with at least two columns
#' @param query the term to be filtered
#' @param invert if TRUE, invert the filter. Default FALSE.
#'
#' @export
#'
#' @examples
#' graph <- data.frame(n1 = c("A", "B", "C"), n2 = c("B", "C", "D"))
#' filter_graph(graph, "B")
#' filter_graph(graph, "B", invert = TRUE)
filter_graph <- function(DF, query, invert = FALSE) {
  if (invert) {
    DF |>
      dplyr::filter(!(n1 == query) | (n2 == query))
  } else {
    DF |>
      dplyr::filter((n1 == query) | (n2 == query))
  }
}


#' Filter both nodes at same time using grep
#'
#' @description
#' filter the first two columns of graph dataframe
#'
#' @param DF Dataframe with at least two columns
#' @param query the term to be filtered
#' @param ic ignore case (default TRUE)
#' @param invert if TRUE, invert the filter. Default FALSE.
#'
#' @export
#'
#' @examples
#' graph <- data.frame(n1 = c("A", "B", "C"), n2 = c("B", "C", "D"))
#' filter_graph_g(graph, "b")
filter_graph_g <- function(DF, query, ic = T, invert = FALSE) {
  if (invert) {
    DF |>
      dplyr::filter(
        !(grepl(pattern = query, x = n1, ignore.case = ic) |
          grepl(pattern = query, x = n2, ignore.case = ic))
      )
  } else {
    DF |>
      dplyr::filter(
        (grepl(pattern = query, x = n1, ignore.case = ic) |
          grepl(pattern = query, x = n2, ignore.case = ic))
      )
  }
}
