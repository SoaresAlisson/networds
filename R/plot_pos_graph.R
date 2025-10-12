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
#' gr <- ex_txt_wiki[2:44] |>
#'   filter_by_query("Police") |>
#'   parsePOS()
#' gr <- gr |> get_cooc_entities()
#' plot_pos_graph(gr)
#'
#' gr <- ex_txt_wiki[2:44] |>
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
    graph_layout = "graphopt") {
  # #  Updated upstream
  # pos_list <- graph
  # pos_list <-  graph_wiki
  # # node_size <- pos_list$nodes |>
  #   dplyr::filter(! node %in% pos_list$isolated_nodes$node)
  #
  # pos_list$edges |>
  #   tidygraph::as_tbl_graph() |>
  #     ggraph::ggraph(layout = graph_layout ) +
  #     ggraph::geom_edge_link(ggplot2::aes(
  #       edge_width = pos_list$edges$freq,
  #       edge_alpha = 0.5),
  #       angle_calc = "along",
  #       label_dodge = grid::unit(4.5, "mm"),
  #       color = edge_color,
  #       end_cap = ggraph::circle(6, "mm")
  #       ) + # afastamento do nó
  #     ggraph::geom_node_point(ggplot2::aes(
  #       size = node_size$freq,
  #       alpha = point_alpha,
  #       # alpha = 0.3,
  #       fill = point_fill ,
  #       color = point_color )) +
  #     ggraph::geom_node_text(ggplot2::aes(
  # # =======
  # pos_list <- graph_ppn
  # pos_list <- uber_g

  try(head_edges <- pos_list$edges |> head(n_head))
  try(head_edges <- pos_list$graph |> head(n_head))
  try(head_edges <- pos_list$graphs |> head(n_head))
  if (head_edges == 0) {
    stop("No edges found")
  }
  # head_nodes <- c(head_edges$n1, head_edges$n2) |> unique()
  if (edge_norm) {
    edge_width <- scales::rescale(head_edges$freq, to = c(0, 10))
  } else {
    edge_width <- head_edges$freq
  }
  # node_size <- pos_list$nodes |> dplyr::filter(node %in% head_nodes)
  # node_size <- head_edges  #|> dplyr::filter(!node %in% pos_list$isolated_nodes$node)
  # nrow(pos_list$nodes)
  g <- igraph::graph_from_data_frame(head_edges, directed = FALSE)

  # Find the number of nodes
  num_nodes <- length(igraph::V(g))
  node_size <- head_edges[1:num_nodes, "freq"] |> unlist()

  head_edges |>
    tidygraph::as_tbl_graph() |>
    ggraph::ggraph(layout = graph_layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        # edge_width = pos_list$edges$freq,
        # edge_width = head_edges$freq,
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
