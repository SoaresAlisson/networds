#' plot net wordcloud
#'
#' plot a network of co-occurrence of terms, as returned by extract_graph and
#' then by dplyr::count(). The size of words and compound words means the
#' individual frequency of each word/compound word. The thickness of the links
#' indicates how often the pair occur together.

#'
#' @param text the original text used to extract the graph. It is necessary to
#' calculate the individual frequency of the words.
#' @param df a dataframe of co-occurrence, extracted with `extract_graph()` and
#' `count(n1, n2)`
#' @param head_n number of nodes to show - the more frequent. Dedault = 30. To
#' display all, use `n_head = ""`
#' @export
#'
#' @examples
#'
#' # stopwords:
#' my_sw <- c(stopwords::stopwords(language = "en", source = "snowball", simplify = TRUE), "lol")
#'
#' txt_wiki |> # text available in the package
#'   # because it is a vector, let's collapse it into a single element:
#'   paste(collapse = " ") |>
#'   extract_graph(sw = my_sw) |>
#'   networds::count_graphs() |> # counting the graphs
#'   net_wordcloud(ex_txt_wiki, df = _) # plotting
net_wordcloud <- function(
  text,
  df,
  head_n = 30,
  color = "lightblue",
  edge_norm = TRUE
) {
  # to head or not to head
  if (head_n == "") {
    graph <- df
  } else {
    graph <- df |> head(head_n)
  }

  # try(graph <- graph |> dplyr::mutate(n = freq)) # TODO padronizar no extract_graph
  try(graph$n <- graph[[3]])
  vert <- unique(c(graph$n1, graph$n2))

  # frequency of nodes/terms
  freqPPN <- lapply(vert, \(v) {
    text |> stringr::str_extract_all(v)
  }) |>
    unlist() |>
    count_vec()

  # rescale / normalize edge width
  if (edge_norm) {
    edge_width <- scales::rescale(graph$n, to = c(0, 10))
  } else {
    edge_width <- graph$n
  }

  graph |>
    tidygraph::as_tbl_graph() |>
    # igraph::graph_from_data_frame(directed = FALSE, vertices = freqPPN) |>
    ggraph::ggraph(layout = "graphopt") +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_width = edge_width, edge_alpha = 0.5),
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      color = color,
      # c("lightblue", "blue", "royalblue")[1],
      end_cap = ggraph::circle(6, "mm")
    ) + # afastamento do nó
    ggraph::geom_node_text(
      ggplot2::aes(
        label = name,
        size = freqPPN$n,
        # size = freq
      ),
      repel = TRUE
    ) + # TODO ajustar tamanho minimo e máximo
    # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
