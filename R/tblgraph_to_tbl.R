# TODO preserve other columns, If create new column with betweeness.

#' Transform a tbl_graph into a labeled tibble
#'
#' @description
#' From a tbl_graph from tidygraph, transform it into a tibble with labeled nodes
#'
#' @param tbl_graph tbl_graph object, from the package tidygraph
#'
#' @export
#'
#' @examples
#' G <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit." |> cooccur_words()
#' G
#' G <- G |> tidygraph::as_tbl_graph()
#' G
#' G |> tblgraph_to_tbl()
tblgraph_to_tbl <- function(tbl_graph) {
  if (any(!class(tbl_graph) %in% c("tbl_graph", "igraph"))) {
    stop("Input must be a tbl_graph")
  }

  tbl_graph |>
    tidygraph::activate(edges) |>
    # dplyr::filter(n > min_threshold) |>
    dplyr::mutate(
      n1 = tidygraph::.N()$name[from],
      n2 = tidygraph::.N()$name[to]
    ) |>
    tibble::as_tibble() |>
    dplyr::select(-from, -to)
}
