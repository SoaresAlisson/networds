#' Plot simple graph
#'
#' @description
#' From a dataframe of graph frequence, plot a graph plot a graph of
#' co-occurrence of terms, as returned by get_cooc_entities, extract_graph.
#' It displays a simple graph to check the cooccurences.
#' To display the weights of edges and different node values, see plot_graph2.
#'
#' @param text an input text
#' @param df a dataframe of co-occurrence, extracted with `extract_graph()` and
#' `count(n1, n2)`
#' @param head_n number of nodes to show - the more frequent
#' @param edge_color color of the edges
#' @param edge_width edge of the edges
#' @param edge_cut in mm, how much you want that the edge stop before reach the node. Can improve readability.
#' @param edge_alpha transparency of the edges. Values between 0 and 1.
#' @param edge_type the type of link between nodes, if straight line or banded
#' arcs. Values: "arc" (or "curved") and "line". Default = "arc"
#' @param edge_bend the degree of arc bending when edge_type = "arc". The values oscillates between -1 and 1, where 0 is a straight line, and negative numbers invert the arc.
#' @param node_alpha transparency of the nodes
#' @param text_color color of the text in nodes
#' @param text_size font size of the nodes
#' @param layout the layout of the plot. Options are bipartite, star, circle,
#' nicely, dh, gem, graphopt, grid, mds, sphere, randomly, fr, kk, drl, lgl.
#' More info at ?displays or at ggraph documentation.
#'
#' @export
#'
#' @examples
#' # plot_graph(df = graph_count, head_n = 50, scale_graph = "log2")
plot_graph <- function(
    DF,
    head_n = 30,
    edge_color = "lightblue",
    edge_alpha = 0.5,
    edge_width = 1,
    edge_type = "arc",
    edge_bend = 0.5,
    edge_cut = 0,
    node_alpha = 0.5,
    text_color = "black",
    text_size = 3,
    text_contour_color = NA,
    layout = "kk"
    # scale_graph = "scale_values"
    ) {
  # to head or not to head
  graph <- to_head_or_not_not_head(DF, head_n)
  # if (head_n == "") {
  #   graph <- df
  # } else {
  #   graph <- df |> head(head_n)
  # }

  graph |>
    # head(head_n) |>
    tidygraph::as_tbl_graph() |>
    ggraph::ggraph(layout = layout) +
    geom_link_type(
      edge_type = edge_type,
      edge_width = edge_width,
      edge_color = edge_color,
      edge_alpha = edge_alpha,
      edge_bend = edge_bend,
      edge_cut = edge_cut
    ) +
    # ggraph::geom_edge_link(
    #   ggplot2::aes(
    #     edge_width = edge_width,
    #   ),
    #   colour = edge_color,
    #   edge_alpha = edge_alpha,
    #   angle_calc = "along",
    #   label_dodge = grid::unit(4.5, "mm"),
    #   end_cap = ggraph::circle(edge_cut, "mm")
    # ) + # afastamento do nó
    ggraph::geom_node_point(
      # ggplot2::aes(
      # normalizando o tamanho do texto
      # size = eval(dplyr::sym(scale_graph))(freqPPN$freq)
      # ),
      # colour = node_color,
      colour = edge_color,
      alpha = node_alpha,
      # repel = TRUE
    ) +
    # ggraph::geom_node_text(
    #   ggplot2::aes(
    #     label = name,
    #     # fill = text_color,
    #   ),
    #   colour = text_color,
    #   size = text_size,
    #   # repel = TRUE
    # ) +
    # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") + {
      if (is.null(text_contour_color)) {
        # if (text_contour) {
        ggraph::geom_node_text(
          ggplot2::aes(
            label = name,
            # size = freqPPN$freq,
          ),
          size = text_size,
          color = text_color,
          repel = TRUE
        )
      } else {
        shadowtext::geom_shadowtext(
          ggplot2::aes(
            label = name,
            x = x,
            y = y,
          ),
          size = text_size,
          bg.r = 0.20,
          bg.color = text_contour_color,
          color = text_color
        ) # TODO ajustar tamanho minimo e máximo
        # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
      }
    }
}
