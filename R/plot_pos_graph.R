#' plot static graph from POS list
#'
#' @description
#' plot a graph of co-occurrence of terms, as returned by `get_cooc_entities()`.
#' The function uses ggraph, so some color options can not work as expected due
#' to some package incompatibilities.
#' The plot_pos_graph is based in ggraph, that is based on ggplot2. That means that
#' is possible to customize the graph with ggplot2 functions, like `labs()`.
#'
#' @param pos_list a list of POS, as returned by `get_cooc_entities()`
# TODO
#' @param n_head maximum number of edges to show. The freq column must be ordered.
#' Default 30, but values near 150 maybe can be a better fit.
#' @param edge_color color of the edges
#' @param edge_alpha transparency of the edges. Values between 0 and 1.
#' @param edge_norm edge normalization if TRUE (default), normalize the result,
#' turning column into values between 0 and 10.
#' @param font_size integer. font size of the nodes. Values between 1 and 2.
#' @param font_color color of the nodes.
#' @param point_fill color of the nodes
#' @param point_alpha transparency of the nodes
#' @param point_color color of the nodes
#' @param graph_layout layout of the graph
#'
#' @export
#'
#' @examples
#' gr <- txt_wiki[2:44] |>
#'   filter_by_query("Police") |>
#'   parsePOS()
#'
#' gr <- gr |> get_cooc_entities()
#' plot_pos_graph(gr)
#'
#' Another example:
#' gr <- txt_wiki[2:44] |>
#'   filter_by_query("Brian") |>
#'   parsePOS()
#' gr <- gr |> get_cooc_entities()
#' plot_pos_graph(gr)
plot_pos_graph <- function(
  pos_list,
  n_head = 30,
  edge_color = "lightblue",
  edge_alpha = 0.1,
  edge_norm = TRUE,
  font_size = 2,
  font_color = "black",
  point_fill = "firebrick4",
  point_alpha = 0.3,
  point_color = "firebrick4",
  graph_layout = "graphopt"
) {
  try(edges <- pos_list$edges)
  try(edges <- pos_list$graph)
  try(edges <- pos_list$graphs)
  # ifd_edges == 0) {
  if (!exists("edges")) {
    stop("No edges found")
  }

  # to head or not to head
  if (n_head == "") {
    graph <- edges
  } else {
    graph <- edges |> head(n_head)
  }

  # head_nodes <- c(graph$n1, head_edges$n2) |> unique()
  if (edge_norm) {
    edge_width <- scales::rescale(graph$freq, to = c(0, 10))
  } else {
    edge_width <- graph$freq
  }
  # node_size <- pos_list$nodes |> dplyr::filter(node %in% head_nodes)
  # node_size <- graph  #|> dplyr::filter(!node %in% pos_list$isolated_nodes$node)
  # nrow(pos_list$nodes)
  g <- igraph::graph_from_data_frame(graph, directed = FALSE)

  # Find the number of nodes
  num_nodes <- length(igraph::V(g))
  node_size <- graph[1:num_nodes, "freq"] |> unlist()

  graph |>
    tidygraph::as_tbl_graph() |>
    ggraph::ggraph(layout = graph_layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        # edge_width = pos_list$edges$freq,
        # edge_width = graph$freq,
        edge_width = edge_width,
        edge_alpha = 0.5
      ),
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      color = edge_color,
      end_cap = ggraph::circle(6, "mm")
    ) + # afastamento do nó
    ggraph::geom_node_point(
      # ggplot2::aes(
      size = node_size,
      alpha = point_alpha,
      # alpha = 0.3,
      fill = point_fill,
      color = point_color
      # )
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(
        # Stashed changes
        label = name,
        # alpha = 0,
        size = font_size,
      ),
      # size = node_size$freq),
      # fill = "mediumpurple4",
      color = font_color,
      repel = TRUE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
