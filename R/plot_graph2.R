#' weighted word graph viz
#'
#' Plot a network of co-occurrence of terms. Because word frequencies can vary
#' significantly, differences in text size can be substantial. Therefore,
#' instead of adjusting text size, we vary the dot/node size, ensuring the
#' text remains consistently sized and maintains readability. It is also
#' possible to normalize the result with log.
#'
#' plot a graph of co-occurrence of terms, as returned by extract_graph
#' @param DF a dataframe of co-occurrence, extracted with `extract_graph()` and
#' `count(n1, n2)`
#' @param text an input text
#' @param head_n number of nodes to show - the more frequent
#' @param lower Convert words to lowercase. If the text is passed in all lowercase, it can return false sentence and paragraph tokenization.
#' @param text_color color of the text in nodes
#' @param text_size font size of the nodes
#' @param node_alpha transparency of the nodes
#' @param node_color color of the nodes. If empty (default), it will use the same color of the edges.
#' @param edge_color color of the edges
#' @param edge_alpha transparency of the edges. Values between 0 and 1.
#' @param edge_cut in mm, how much you want that the edge stop before reach the node. Can improve readability.
#' @param scale_graph name of a function to normalize the result. Sometime, the range
#' of numbers are so wide that the graph becomes unreadable. Applying a function
#' to normalize the result can improve the readability, for example using
#' `scale_graph = "log2"`, or `"log10"`
#' @export
#'
#' @examples
#' # plot_graph(txt, df = graph_count, head_n = 50, scale_graph = "log2")
plot_graph2 <- function(
    DF,
    text,
    head_n = 30,
    lower = TRUE,
    text_color = "black",
    text_size = 1,
    node_alpha = 0.5,
    node_color = "",
    edge_color = "lightblue",
    edge_alpha = 0.5,
    edge_cut = 0,
    scale_graph = "scale_values") {
  # to head or not to head
  graph <- to_head_or_not_not_head(DF, head_n)
  # if (head_n == "") {
  #   graph <- df
  # } else {
  #   graph <- df |> head(head_n)
  # }

  scale_values <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

  if (lower) {
    graph <- graph |>
      dplyr::mutate(
        n1 = tolower(n1),
        n2 = tolower(n2)
      )
    text <- tolower(text)
  }
  graph <- graph |>
    # head(head_n) |>
    dplyr::mutate(n = eval(dplyr::sym(scale_graph))(n))

  vert <- unique(c(graph$n1, graph$n2)) |>
    gsub(x = _, "\\.", "\\\\.") # to avoid punct be taken as regex .

  # frequency of nodes/terms
  freqPPN <- lapply(vert, \(v) {
    text |>
      # stringr::str_extract_all(v) |>
      # stringr::str_extract_all(pattern = paste0("\\b(?i)", v, "\\b")) |>
      stringr::str_extract_all(pattern = paste0("\\b", v, "\\b")) |>
      suppressWarnings()
  }) |>
    unlist() |>
    count_vec()

  # If used word like "New_York", it will clean, search in text, and put it back
  # if (length(vert) != nrow(freqPPN)) {
  freqPPN_nodes <- gsub(x = freqPPN$x, "\\.", "\\\\.")
  inVert_not_inFreqPPN <- vert[!vert %in% freqPPN_nodes]

  if (length(inVert_not_inFreqPPN) > 0) {
    missing_nodes <- gsub(x = inVert_not_inFreqPPN, "_", " ")

    missing_nodes_df <- lapply(missing_nodes, \(v) {
      text |>
        stringr::str_extract_all(pattern = paste0("\\b(?i)", v, "\\b"))
    }) |>
      unlist() |>
      count_vec() |>
      dplyr::mutate(x = gsub(x = x, " ", "_"))

    # freqPPN <- rbind(freqPPN, missing_nodes_df) |> suppressWarnings()
    freqPPN <- dplyr::bind_rows(freqPPN, missing_nodes_df) |> suppressWarnings()
  }

  if (node_color == "") {
    node_color <- edge_color
  }

  if (class(text_size) != "numeric") {
    message("No text_size provided. Using size proportional to frequency")
    text_size <- eval(dplyr::sym(scale_graph))(freqPPN$freq)
  }

  graph |>
    tidygraph::as_tbl_graph() |>
    # igraph::graph_from_data_frame(directed = FALSE, vertices = freqPPN) |>
    ggraph::ggraph(layout = "graphopt") +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_width = n),
      edge_alpha = edge_alpha,
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      colour = edge_color,
      end_cap = ggraph::circle(edge_cut, "mm")
    ) + # afastamento do nó
    ggraph::geom_node_point(
      ggplot2::aes(
        # normalizando o tamanho do texto
        size = text_size,
      ),
      colour = node_color,
      alpha = node_alpha,
      # repel = TRUE
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
