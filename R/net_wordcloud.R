#' plot net wordcloud
#'
#' plot a network of co-occurrence of terms, as returned by extract_graph and
#' then by dplyr::count(). The size of words and compound words means the
#' individual frequency of each word/compound word. The thickness of the links
#' indicates how often the pair occur together. Pay attention that if the words
#' doesn't appear in different sizes when plotted, maybe the relative differences
#' in their frequency can be very low.

#'
#' @param DF a dataframe of co-occurrence, extracted with `extract_graph()` and
#' `count(n1, n2)`
#' @param text the original text used to extract the graph. It is necessary to
#' calculate the individual frequency of the words.
#' @param head_n number of nodes to show - the more frequent. Dedault = 30. To
#' display all, use `n_head = ""`
#' @param lower Convert words to lowercase. If the text is passed in all lowercase, it can return false sentence and paragraph tokenization.
#' @param edge_color color of the links
#' @param edge_alpha transparency of the links. Values between 0 and 1.
#' @param text_color color of the text in nodes
#'
#' @export
#'
#' @examples
#'
#' # stopwords:
#' my_sw <- c(stopwords::stopwords(language = "en", source = "snowball", simplify = TRUE), "lol")
#'
#' txt_wiki |> # text available in the package
#'   cooccur(sw = my_sw) |>
#'   net_wordcloud(txt_wiki, DF = _, head_n = 50) # plotting
#'
#' txt_wiki |> # text available in the package
#'   # because it is a vector, let's collapse it into a single element:
#'   paste(collapse = " ") |>
#'   extract_graph(sw = my_sw) |>
#'   networds::count_graphs() |> # counting the graphs
#'   net_wordcloud(txt_wiki, DF = _) # plotting
net_wordcloud <- function(
    DF,
    text,
    head_n = 30,
    lower = TRUE,
    edge_color = "lightblue",
    edge_alpha = 0.5,
    text_color = "black",
    edge_norm = TRUE) {
  # to head or not to head
  if (head_n == "") {
    Graph <- DF
  } else {
    Graph <- DF |> head(head_n)
  }
  if (lower) {
    Graph <- Graph |>
      dplyr::mutate(
        n1 = tolower(n1),
        n2 = tolower(n2)
      )
    text <- tolower(text)
  }
  # getting all node unique names
  # try(Graph <- Graph |> dplyr::mutate(n = freq)) # TODO padronizar no extract_graph
  # rename column
  try(Graph$n <- Graph[[3]])
  vert <- unique(c(Graph$n1, Graph$n2)) |>
    gsub(x = _, "\\.", "\\\\.") # to avoid punct be taken as regex .

  # frequency of nodes/terms
  freqPPN <- lapply(vert, \(v) {
    text |>
      # gsub(x = _, "\\.", "\\\\.") |>
      # stringr::str_extract_all(pattern = paste0("(?i)", v))
      stringr::str_extract_all(pattern = paste0("\\b(?i)", v, "\\b"))
    # stringr::str_extract_all(pattern = paste0("(?i)\\b", v, "\\b"))
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

    freqPPN <- rbind(freqPPN, missing_nodes_df)
  }

  # inVert_not_inFreqPPN <- vert[!vert %in% freqPPN$x]
  # if (length(inVert_not_inFreqPPN) > 0) {
  #   stop(
  #     "Conflict in the number of nodes. ",
  #     length(inVert_not_inFreqPPN), " are missing: ",
  #     paste(inVert_not_inFreqPPN, collapse = ", "), " is/are lacking."
  #   )
  # }

  # rescale / normalize edge width
  if (edge_norm) {
    edge_width <- scales::rescale(Graph$n, to = c(0, 10))
  } else {
    edge_width <- Graph$n
  }

  Graph |>
    tidygraph::as_tbl_graph() |>
    # igraph::graph_from_data_frame(directed = FALSE, vertices = freqPPN) |>
    ggraph::ggraph(layout = "graphopt") +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_width = edge_width, edge_alpha = edge_alpha),
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      color = edge_color,
      # c("lightblue", "blue", "royalblue")[1],
      end_cap = ggraph::circle(6, "mm")
    ) + # afastamento do nó
    ggraph::geom_node_text(
      ggplot2::aes(
        label = name,
        size = freqPPN$freq,
      ),
      color = text_color,
      repel = TRUE
    ) + # TODO ajustar tamanho minimo e máximo
    # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
