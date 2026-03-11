# DF <- data.frame(n1 = c("Sample1", "Sample2", "Sample2"), n2 = c("A1", "B1", "A2"), l = c("Value1", "Value2", "Value3"))

#' Count the graph frequency of co-occurences/Co-mentioning of triplets
#'
#' @param graph a graph of co-occurrence
#' @param loop if TRUE, include loops. Default=FALSE
#'
#' @export
#'
#' @examples
#' DF <- data.frame(n1 = c("Sample1", "Sample1", "Sample2", "Sample2"), n2 = c("A1", "Sample1", "B1", "B1"))
#' DF |> count_graphs()
count_graphs <- function(graph, loop = FALSE) {
  gr <- graph |>
    # dtplyr::lazy_dt() |> # do dtplyr to improve performance. # commented for a moment: returning error in "."
    dplyr::count(n1, n2, sort = TRUE) |>
    tibble::as_tibble()

  if (loop) {
    return(gr)
  } else {
    gr <- gr |>
      dplyr::mutate(loop = (n1 == n2)) |>
      dplyr::filter(!loop) |>
      dplyr::select(-loop)

    return(gr)
  }
}
