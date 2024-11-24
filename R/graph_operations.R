# functions to basic operations with graphs


#' a sample graph dataframe
#'
#' @export
g <- tidygraph::tbl_graph(
  nodes = data.frame(name = c(
    "Alice", "Bob", "Charlie",
    "David", "John", "Mary"
  )),
  edges = data.frame(
    from = c(1, 1, 2, 3, 2, 6),
    to = c(2, 3, 4, 4, 5, 5)
  )
)



#' search for the id of a node in a graph
#'
#' @param graph a tbl_graph
#' @param querie the name of the node
#'
#' @export
#'
#' @examples
#' # Building a graph dataframe
#' # Node data frame:
#' nodes <- data.frame(name = c("Alice", "Bob", "Charlie", "David", "John", "Mary"))
#' # Edge data frame
#' edges <- data.frame(from = c(1, 1, 2, 3, 2, 6), to = c(2, 3, 4, 4, 5, 5))
#' g <- tidygraph::tbl_graph(nodes = nodes, edges = edges)
#' get_node_id(g, "Bob")
get_node_id <- function(graph, query) {
  graph |>
    tidygraph::activate(nodes) |>
    data.frame() |>
    dplyr::mutate(id = dplyr::row_number()) |>
    # dplyr::filter(name == querie) |>
    dplyr::filter(grepl(x = name, query)) |>
    dplyr::pull(id)
}

#' get the neighbors of a site
#'
#' @description
#' creates a sub graph with the node of reference and its Nth neighbors
#'
#' @export
#'
#' @examples
#' g
#' g |> plot()
#' get_neighbors(g, "Charlie", 1)
#' get_neighbors(g, "Charlie", 2) |> plot()
#' get_neighbors(g, "Alice", 1) |> plot()
#' get_neighbors(g, "Alice", 2) |> plot()
get_neighbors <- function(graph, query, n = 1) {
  library(tidygraph)

  graph |>
    tidygraph::activate(nodes) |>
    tidygraph::convert(to_local_neighborhood,
      # node = 1,
      node = get_node_id(graph, query),
      order = n, mode = "all"
    )
}


#' filter a graph / create an ego graph by term and by the number of its neighbors
#'
#' @param edges an edge dataframe
#' @param nodes a node dataframe
#' @param filter_by a term to filter the ego graph
#' @param n_neighbours the number of neighbors
#'
#' @export
#'
#' @examples
#' # creating sample data
#' nodes <- data.frame(id = 1:5, name = LETTERS[1:5])
#' edges <- data.frame(from = c(1, 1, 2, 3, 4, 1, 6, 7), to = c(2, 3, 4, 5, 5, 4, 7, 5))
#' filter_ego(edges, nodes, filter_by = 1, n_neighbours = 1)
#' filter_ego(edges, nodes, filter_by = "jojo", n_neighbours = 2)
filter_ego <- function(edges, nodes = NULL, filter_by, n_neighbours = 1) {
  if (is.null(nodes)) {
    message("Nodes are empty. Extracting it from edge dataframe")
    nodes <- unique(c(edges[["from"]], edges[["to"]]))
    nodes <- data.frame(id = 1:length(nodes), name = nodes)
  }

  g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes) |>
    igraph::make_ego_graph(order = n_neighbours, nodes = filter_by, mode = c("all"))

  as_tbl_graph(g[[1]])
}
