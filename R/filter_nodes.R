#' filter nodes in both columns, at the same time
#'
#' Given a dataframe with at least two columns, filter the first two columns
#' at the same time. It uses grepl not equal comparision `==`, so it is
#' possible to use grepl parameters like ignore.case, etc.
#'
#' @param DF Dataframe with at least two columns
#' @param query String to filter the columns
#' @param invert if TRUE, invert the filter. Default FALSE.
#' @param ... parameters for grepl
#'
#' @export
#'
#' @examples
#' txt <- "Lorem Ipsum. The Ipsum John. Dolor est Lorem"
#' DF <- txt |> cooccur()
#' DF
#' DF |> filter_nodes()
filter_nodes <- function(DF, query, invert = FALSE, ...) {
  col_names <- colnames(DF)
  col1 <- col_names[1]
  col2 <- col_names[2]

  if (invert) {
    DF <- DF[!grepl(pattern = query, x = unlist(DF[, col1]), ...), ]
    DF <- DF[!grepl(pattern = query, x = unlist(DF[, col2]), ...), ]
  } else {
    DF <- DF[
      grepl(pattern = query, x = unlist(DF[, col1]), ...) |
        grepl(pattern = query, x = unlist(DF[, col2]), ...),
    ]
  }
  return(DF)
}
