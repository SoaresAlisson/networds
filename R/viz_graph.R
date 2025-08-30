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



#' weighted word graph viz
#'
#' Plot a network of co-ocurrence of terms. Because word frequencies can vary
#' significantly, differences in text size can be substantial. Therefore,
#' instead of adjusting text size, we vary the dot/node size, ensuring the
#' text remains consistently sized and maintains readability. It is also
#' possible to normalize the result with log.
#'
#' plot a graph of co-occurrence of terms, as returned by extract_graph
#' @param text an input text
#' @param df a dataframe of co-occurrence, extracted with `extract_graph()` and
#' `count(n1, n2)`
#' @param head_n number of nodes to show - the more frequent
#' @param edge_color color of the edges
#' @param edge_alpha transparency of the edges. Values between 0 and 1.
#' @param node_alpha transparency of the nodes
#' @param text_color color of the text in nodes
#' @param text_size font size of the nodes
#' @param scale_graph name of a function to normalize the result. Sometime, the range
#' of numbers are so wide that the graph becomes unreadable. Applying a function
#' to normalize the result can improve the readability, for example using
#' `scale_graph = "log2"`, or `"log10"`
#' @export
#'
#' @examples
#' # plot_graph(txt, df = graph_count, head_n = 50, scale_graph = "log2")
plot_graph2 <- function(text, df, head_n = 30,
                        edge_color = "lightblue",
                        edge_alpha = 0.5,
                        node_alpha = 0.5,
                        text_color = "black",
                        text_size = 1,
                        scale_graph = "scale_values") {
  scale_values <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

  graph <- df |>
    head(head_n) |>
    dplyr::mutate(n = eval(dplyr::sym(scale_graph))(n))

  vert <- unique(c(graph$n1, graph$n2))

  # frequency of nodes/terms
  freqPPN <- lapply(vert, \(v) {
    text |>
      stringr::str_extract_all(v) |>
      suppressWarnings()
  }) |>
    unlist() |>
    count_vec()

  graph |>
    tidygraph::as_tbl_graph() |>
    # igraph::graph_from_data_frame(directed = FALSE, vertices = freqPPN) |>
    ggraph::ggraph(layout = "graphopt") +
    ggraph::geom_edge_link(
      ggplot2::aes(
        edge_width = n
      ),
      edge_alpha = edge_alpha,
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      colour = edge_color,
      end_cap = ggraph::circle(6, "mm")
    ) + # afastamento do nó
    ggraph::geom_node_point(
      ggplot2::aes(
        # normalizando o tamanho do texto
        size = eval(dplyr::sym(scale_graph))(freqPPN$freq)
      ),
      # colour = node_color,
      colour = edge_color,
      alpha = node_alpha,
      repel = TRUE
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(
        label = name,
        # fill = text_color,
        size = text_size
      ),
      colour = text_color,
      repel = TRUE
    ) +
    # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
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
