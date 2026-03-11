#' use straight line or arc link between nodes
#'
#' @param edge_type arc or line.
geom_link_type <- function(
    edge_type = "arc",
    edge_width = 1,
    edge_color,
    edge_alpha,
    edge_cut,
    edge_bend = 0.5) {
  if (length(edge_width) == 1) {
    message(
      "Only one value passed to edge_width. All edges will have the same width."
    )
  }

  if (edge_type %in% c("arc", "curved")) {
    # if (edge_type == "arc") {
    ggraph::geom_edge_arc0(
      ggplot2::aes(
        edge_width = edge_width,
      ),
      colour = edge_color,
      edge_alpha = edge_alpha,
      strength = edge_bend
    )
  } else if (edge_type == "line") {
    ggraph::geom_edge_link(
      ggplot2::aes(
        edge_width = edge_width,
      ),
      colour = edge_color,
      edge_alpha = edge_alpha,
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      end_cap = ggraph::circle(edge_cut, "mm")
    )
  } else {
    (stop('Edge type "', edge_type, '" not found.'))
  }
}
