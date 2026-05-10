# functions to basic operations with graphs

#' Search for the id of a node in a tbl_graph
#'
#' @description
#' get the id of node from a tbl_graph as created with tidygraph::tbl_graph()
#' or dataframe converted with tidygraph::as_tbl_graph()
#'
#' @param graph a tbl_graph
#' @param querie the name of the node
#'
#' @export
#'
#' @examples
#' # Building a graph dataframe
#' # Node data frame:
#'  <- data.frame(name = c("Alice", "Bob", "Charlie", "David", "John", "Mary"))
#' # Edge data frame
#' edges <- data.frame(from = c(1, 1, 2, 3, 2, 6), to = c(2, 3, 4, 4, 5, 5))
#' g <- tidygraph::tbl_graph(nodes = nodes, edges = edges)
#' g
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

#' Get the neighbors of a node
#'
#' @description
#' given a graph and a query, creates a sub graph with the node of reference
#' and its Nth neighbors
#'
#' @param graph a tibble graph created with tidygraph::tbl_graph() or converted
#' with tidygraph::as_tbl_graph()
#' @param query a query to be searched
#' @param n number of node neighbors since the query
#'
#' @export
#'
#' @examples
#' # Building a graph dataframe
#' # Node data frame:
#' nodes <- data.frame(name = c("Alice", "Bob", "Charlie", "David", "John", "Mary"))
#' edges <- data.frame(from = c(1, 1, 2, 3, 2, 6), to = c(2, 3, 4, 4, 5, 5))
#' g <- tidygraph::tbl_graph(nodes = nodes, edges = edges)
#' g
#' get_neighbors(g, "Charlie", 1)
#' get_neighbors(g, "Charlie", 2)
#' get_neighbors(g, "Alice", 1)
#' get_neighbors(g, "Alice", 2)
get_neighbors <- function(graph, query, n = 1) {
  # graph <- g
  # graph <- g$edges
  # query <- "Charlie"
  graph |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate(edges) |>
    tidygraph::convert(
      tidygraph::to_local_neighborhood,
      # node = 1,
      node = get_node_id(graph, query),
      order = n,
      mode = "all"
    )
}


#' Get the neighbors of a word in a dataframe
#'
#' @description
#' given a graph dataframe, find the neighbour nodes of the actual nodes,
#' increasing the size of the graph
#'
#' @export
get_neighbors_df <- function(DF, query) {
  column_names <- colnames(DF)

  df_ <- DF |>
    dplyr::filter(
      # from == word | to == word
      grepl(query, .data[[column_names[1]]]) | # row-wise value of 1st column
        grepl(query, .data[[column_names[2]]]) # row-wise value of 2nd column
    ) |>
    as.data.frame()

  if (nrow(df_) == 0) {
    stop('Word not found: "', word, '"')
  }

  column_names <- colnames(df_)
  # neighbors <- c(df_$to, df_$from) |> unique()
  neighbors <- c(df_[1], df_[2]) |> unique()

  # triplets |>
  df_ |>
    dplyr::filter(
      # from %in% neighbors | to %in% neighbors)
      .data[[column_names[1]]] %in%
        neighbors |
        .data[[column_names[1]]] %in% neighbors
    )
}

# trip <- get_neighbors_df(triplets, "covid-19")
# get_neighbors_df(triplets, "bambole")

#' Filter a graph / create an ego graph
#'
#' filter a graph / create an ego graph by term and by the number of its neighbors
#'
#' @param edges an edge dataframe
#' @param nodes a node dataframe
#' @param filter_by a term to filter the ego graph
#' @param n_neighbours the number of neighbors
#' @param as_tbl if TRUE, return a tbl_graph, if FALSE, return an igraph object
#'
#' @export
#'
#' @examples
#' # creating sample data
#' nodes <- data.frame(id = 1:7, name = LETTERS[1:7])
#' nodes
#' edges <- data.frame(
#'   from = c(1, 1, 2, 3, 4, 1, 6, 7),
#'   to = c(2, 3, 4, 5, 5, 4, 7, 5)
#' )
#' edges
#' # filtering:
#' filter_ego(edges, nodes, filter_by = 1, n_neighbours = 1)
#' filter_ego(edges, nodes, filter_by = 1, n_neighbours = 2)
filter_ego <- function(
  edges,
  nodes = NULL,
  filter_by,
  n_neighbours = 1,
  as_tbl = TRUE
) {
  if (is.null(nodes)) {
    message("Nodes are empty. Extracting it from edge dataframe")
    nodes <- unique(c(edges[["from"]], edges[["to"]]))
    nodes <- data.frame(id = 1:length(nodes), name = nodes)
  }

  if (as_tbl) {
    gr <- tidygraph::as_tbl_graph(g[[1]])
  } else {
    gr <- igraph::graph_from_data_frame(
      edges,
      directed = TRUE,
      vertices = nodes
    ) |>
      igraph::make_ego_graph(
        order = n_neighbours,
        nodes = filter_by,
        mode = c("all")
      )
  }
  return(gr)
}
