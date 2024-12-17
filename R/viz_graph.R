# functions to plot graphs, statically and interactively.
#
# options(browser="firefo@")

#' Plot a network of co-ocurrence of terms
#'
#' plot a graph of co-occurrence of terms, as returned by extract_graph
#'
#' @param text an input text
#' @param df a dataframe of co-occurrence, extracted with `extract_graph()` and `count(n1, n2)`
#' @param head_n number of nodes to show - the more frequent
#' @export
plot_graph <- function(text, df, head_n = 30, color = "lightblue") {
  # graph <-  g_N |> head(head_n)
  graph <- df |> head(head_n)
  vert <- unique(c(graph$n1, graph$n2))

  # frequency of nodes/terms
  freqPPN <- lapply(vert, \(v) {
    text |> stringr::str_extract_all(v)
  }) |>
    unlist() |>
    count_vec()

  graph |>
    igraph::graph_from_data_frame(directed = FALSE, vertices = freqPPN) |>
    ggraph::ggraph(layout = "graphopt") +
    ggraph::geom_edge_link(ggplot2::aes(edge_width = n, edge_alpha = 0.5),
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      color = color,
      # c("lightblue", "blue", "royalblue")[1],
      end_cap = ggraph::circle(6, "mm")
    ) + # afastamento do nó
    ggraph::geom_node_text(ggplot2::aes(label = name, size = freq), repel = TRUE) + # TODO ajustar tamanho minimo e máximo
    # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

#' weighted graph viz
#'
#' Plot a network of co-ocurrence of terms
#'
#' plot a graph of co-occurrence of terms, as returned by extract_graph
#' @param text an input text
#' @param df a dataframe of co-occurrence, extracted with `extract_graph()` and `count(n1, n2)`
#' @param head_n number of nodes to show - the more frequent
#' @param scale name of a function to normalize the result. Sometime, the range of numbers are so wide that the graph becomes unreadable. Applying a function to normalize the result can improve the readability, for example using `scale = "log2"`, or "log10"
#' @export
#'
#' @examples
#' # plot_graph(txt, df = graph_count, head_n = 50, scale = "log2")
plot_graph2 <- function(text, df, head_n = 30, color = "lightblue", scale = "scale_values") {
  # c("log2", "log10"))
  # graph <-  g_N |> head(head_n)
  scale_values <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

  graph <- df |>
    head(head_n) |>
    dplyr::mutate(n = eval(dplyr::sym(scale))(n))

  vert <- unique(c(graph$n1, graph$n2))

  # frequency of nodes/terms
  freqPPN <- lapply(vert, \(v) {
    text |> stringr::str_extract_all(v)
  }) |>
    unlist() |>
    count_vec()

  graph |>
    igraph::graph_from_data_frame(directed = FALSE, vertices = freqPPN) |>
    ggraph::ggraph(layout = "graphopt") +
    ggraph::geom_edge_link(ggplot2::aes(edge_width = n, edge_alpha = 0.5),
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      color = color,
      # c("lightblue", "blue", "royalblue")[1],
      end_cap = ggraph::circle(6, "mm")
    ) + # afastamento do nó
    ggraph::geom_node_text(
      ggplot2::aes(
        label = name,
        # normalizando o tamanho do texto
        size = eval(dplyr::sym(scale))(freq)
      ),
      repel = TRUE
    ) + # TODO ajustar tamanho minimo e máximo
    # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}


#' viz graph interactively
#'
#' Visualize graphs interactively (package visNetwork). The columns must be named:
#' "from", "label", "to" and "value" (frequency of the triplet)
#'
#' @param graph_df a dataframe with the graph data
#' @param nodesIdSelection a boolean value to enable node selection. Default: TRUE.
#'
#' @export
viz_graph <- function(graph_df, nodesIdSelection = TRUE, height = "900px") {
  unique_nodes <- unique(c(graph_df$from, graph_df$to)) # Create a unique list of all nodes (vertices)

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
