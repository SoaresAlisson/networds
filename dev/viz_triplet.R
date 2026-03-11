require(tidygraph)
require(ggraph)
gr <- create_notable("bull") %>%
  mutate(class = sample(letters[1:3], n(), replace = TRUE)) %>%
  activate(edges) %>%
  mutate(class = sample(letters[1:3], n(), replace = TRUE))

DF <- tibble::tibble(
  f = letters[1:4],
  t = letters[2:5],
  n = c(1, 1, 3, 2),
  link = c("bla", "ble", "bli", "blo")
)

ggraph(gr, "stress") +
  geom_edge_link(aes(alpha = after_stat(index)))

DF |>
  ggraph("stress") +
  # geom_edge_link(
  geom_edge_arc(
    arrow = arrow(type = "closed", length = unit(3, "mm")),
    aes(label = link, width = n),
    alpha = 0.2,
  ) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position = "none")

viz_triplet <- function(DF, head_n = 20) {
  DF <- to_head_or_not_not_head(DF, head_n)

  DF |>
    ggraph::ggraph("stress") +
    # geom_edge_link(
    ggraph::geom_edge_arc(
      arrow = ggraph::arrow(type = "closed", length = ggplot2::unit(3, "mm")),
      aes(label = link, width = n),
      alpha = 0.2,
    ) +
    ggraph::geom_node_point() +
    ggraph::geom_node_text(aes(label = name), repel = TRUE) +
    ggplot2::theme_graph() +
    ggplot2::theme(legend.position = "none")
}
