# functions to plot graphs, statically and interactively.
#
# options(browser="firefo@")

#' quick word graph
#'
#' Plot a network of co-ocurrence of terms. For more options, see `net_wordcloud()`.
# #' @param edge_df a dataframe of co-occurrence, extracted with `get_cooc_entities()`
#' @param edge_df a dataframe of co-occurrence, extracted with `extract_graph()`
#' @param color color of the edges. Default: "lightblue".
#'
#' @export
#'
#' @examples
#' library(networds)
#'
#' g <- ex_txt_wiki[2:44] |>
#'   filter_by_query("Police") |>
#'   parsePOS()
#' g <- get_cooc_entities(g)
#' q_plot(g)
q_plot <- function(graph_list, color = "lightblue") {
  # graph_list <- g
  try(graph <- graph_list$edges)
  try(graph <- graph_list$graphs)

  graph |>
    tidygraph::as_tbl_graph() |>
    ggraph::autograph(
      node_label = name,
      node_size = graph_list$nodes$freq,
      edge_colour = color,
      edge_width = freq
    )
  # #' g <- ex_txt_wiki[2:44] |> filter_by_query("Police") |> parsePOS() |
  # #' g <- get_cooc_entities(g)
  # #' q_plot(g)
  # q_plot <- function(edge_df) {
  #   edge_df$edges |>
  #     tidygraph::as_tbl_graph() |>
  #     ggraph::autograph(node_label= name)
}







#' viz graph interactively
#'
#' Visualize graphs interactively (package visNetwork). The columns must be named:
#' "from", "label", "to" and "value" (frequency of the triplet) OR the 1st, 2nd
#' and 3rd columns will be taken as such.
#'
#' @param graph_df a dataframe with the graph data
#' @param nodesIdSelection a boolean value to enable node selection. Default: TRUE.
#'
#' @export
#'
#' @examples
#' x <- ex_txt_wiki[2:44] |>
#'   filter_by_query("Brian") |>
#'   parsePOS()
#' g <- get_cooc_entities(x)
#' g$edges |>
#'   dplyr::rename(from = n1, to = n2) |>
#'   viz_graph()
#' g$edges |> viz_graph()
interactive_graph <- function(graph_df, nodesIdSelection = TRUE, height = "900px") {
  # viz_graph <- function(graph_df, nodesIdSelection = TRUE, height = "900px") {
  # graph_df <- g$edges
  #
  # if column names are not: from to
  col_names <- graph_df |> colnames()

  if (!col_names %in% c("from", "to") |> any()) {
    col_names[1] <- "from"
    col_names[2] <- "to"
    col_names[3] <- "value"

    colnames(graph_df) <- col_names
  }

  # Create a unique list of all nodes (vertices)
  unique_nodes <- unique(c(graph_df$from, graph_df$to))

  node_mapping <- data.frame( # Create a mapping between node names and indices
    id = seq_along(unique_nodes),
    label = unique_nodes
  )

  graph_data <- graph_df |> # Replace node names with indices in the graph data
    dplyr::mutate(
      from = purrr::map_chr(from, ~ node_mapping$id[node_mapping$label == .x] |> as.character()),
      to = purrr::map_chr(to, ~ node_mapping$id[node_mapping$label == .x] |> as.character()),
      arrows = "to"
      # color = purrr::map_chr(, ~ node_mapping$id[node_mapping$label == .x]),
    )

  # https://cran.r-project.org/web/packages/visNetwork/visNetwork.pdf
  visNetwork::visNetwork(node_mapping, graph_data, width = "100%", height = height) |>
    visNetwork::visOptions(
      highlightNearest = TRUE,
      nodesIdSelection = nodesIdSelection
    )
}
