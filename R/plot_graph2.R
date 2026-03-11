#' weighted word graph viz
#'
#' @description
#' Plot a network of co-occurrence of terms. Because word frequencies can vary
#' significantly, differences in text size can be substantial. Therefore,
#' instead of adjusting text size, we vary the dot/node size, ensuring the
#' text remains consistently sized and maintains readability. It is also
#' possible to normalize the result with log.
#' plot a graph of co-occurrence of terms, as returned by extract_graph, cooccur
#'
#' @param DF a dataframe of co-occurrence, extracted with `extract_graph()` and
#' `count(n1, n2)`
#' @param text an input text
#' @param head_n number of nodes to show - the more frequent
#' @param lower Convert words to lowercase. If the text is passed in all lowercase, it can return false sentence and paragraph tokenization.
#' @param text_color color of the text in nodes
#' @param text_size font size of the nodes. If empty (default), it will use the frequency of word.
#' @param node_alpha transparency of the nodes
#' @param node_color color of the nodes. If empty (default), it will use the same color of the edges.
#' @param edge_color color of the edges
#' @param edge_alpha transparency of the edges. Values between 0 and 1.
#' @param edge_cut in mm, how much you want that the edge stop before reach the node. Can improve readability.
#' @param edge_fan if TRUE, the edges are drawn in a fan shape. Default FALSE.
#' @param scale_graph name of a function to normalize the result. Sometimes, the range
#' of numbers are so wide that the graph becomes unreadable. Applying a function
#' to normalize the result can improve the readability, for example using
#' `scale_graph = "log2"`, `"sqrt"`,`"log10"` or `none`.
#' @param layout the layout of the plot. Options are bipartite, star, circle,
#' nicely, dh, gem, graphopt, grid, mds, sphere, randomly, fr, kk, drl, lgl.
#' More info at ggraph documentation
#'
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
    text_size = 3,
    text_contour_color = NA,
    node_alpha = 0.5,
    node_color = NULL,
    node_size = NULL,
    edge_color = "lightblue",
    edge_alpha = 0.5,
    edge_cut = 0,
    edge_type = "arc",
    edge_bend = 0.5,
    edge_fan = FALSE,
    scale_graph = "scale_values",
    layout = "kk") {
  # to head or not to head
  graph <- to_head_or_not_not_head(DF, head_n)
  # if (head_n == "") {
  #   graph <- df
  # } else {
  #   graph <- df |> head(head_n)
  # }

  # fun to normalize values
  scale_values <- function(x) {
    ((x - min(x)) / (max(x) - min(x))) + 0.1
  }

  text_length <- length(text)

  if (text_length > 1) {
    message(
      "You provided a vector of ",
      text_length,
      " elements instead of one. No problem, but these will be collapsed into a single element, with a final punctuation mark added to each."
    )
    text <- paste(text, collapse = ".")
  }

  if (lower) {
    graph <- graph |>
      dplyr::mutate(
        n1 = tolower(n1),
        n2 = tolower(n2)
      )
    text <- tolower(text)
  }

  if (!scale_graph %in% c("none", "")) {
    # if (scale_graph != "none") {
    graph <- graph |>
      # dplyr::rename(n = freq)
      dplyr::mutate(n = eval(dplyr::sym(scale_graph))(n))
  } else if (scale_graph == "scale_values"){
    graph <- graph |>
      # dplyr::rename(n = freq)
    dplyr::mutate(n = scale_to_range(n, 0.2, 9)) 

  }

  vert <- unique(c(graph$n1, graph$n2)) |>
    gsub(x = _, "\\.", "\\\\.") # to avoid punct be taken as regex .

  # # frequency of nodes/terms
  # freqPPN <- lapply(vert, \(v) {
  #   text |>
  #     # stringr::str_extract_all(v) |>
  #     # stringr::str_extract_all(pattern = paste0("\\b(?i)", v, "\\b")) |>
  #     stringr::str_extract_all(pattern = paste0("\\b", v, "\\b")) |>
  #     suppressWarnings()
  # }) |>
  #   unlist() |>
  #   count_vec()
  #
  # # If used word like "New_York", it will clean the "_", search it in text, and put the underline back
  # # if (length(vert) != nrow(freqPPN)) {
  # freqPPN_nodes <- gsub(x = freqPPN$x, "\\.", "\\\\.")
  # inVert_not_inFreqPPN <- vert[!vert %in% freqPPN_nodes]
  #
  # if (length(inVert_not_inFreqPPN) > 0) {
  #   missing_nodes <- gsub(x = inVert_not_inFreqPPN, "_", " ")
  #
  #   missing_nodes_df <- lapply(missing_nodes, \(v) {
  #     text |>
  #       stringr::str_extract_all(pattern = paste0("\\b(?i)", v, "\\b"))
  #   }) |>
  #     unlist() |>
  #     count_vec() |>
  #     dplyr::mutate(x = gsub(x = x, " ", "_"))
  #
  #   # freqPPN <- rbind(freqPPN, missing_nodes_df) |> suppressWarnings()
  #   freqPPN <- dplyr::bind_rows(freqPPN, missing_nodes_df) |> suppressWarnings()
  # }

  if (any(is.null(node_color), node_color == "")) {
    node_color <- edge_color
  }

  node_freq <- freq_nodes(vert, text)

  if (any(is.null(node_size), node_size == "")) {
    # if (class(text_size) != "numeric") {
    # normalizando o tamanho do texto
    # node_size <- eval(dplyr::sym(scale_graph))(freqPPN$freq)
    if (!scale_graph %in% c("none", "")) {
      node_size <- eval(dplyr::sym(scale_graph))(node_freq$freq)
    }
    message(
      "Using node_size proportional to word frequency as no node_size was provided in parameters"
    )
    # message(node_size)
  }

  add_edge_layer <- function(edge_fan = FALSE, ...) {
    if (edge_fan) {
      ggraph::geom_edge_fan(...)
    } else {
      ggraph::geom_edge_link(...)
    }
  }

  graph |>
    tidygraph::as_tbl_graph() |>
    # igraph::graph_from_data_frame(directed = FALSE, vertices = freqPPN) |>
    # tidygraph::activate("nodes") |>
    ggraph::ggraph(layout = layout) +
    geom_link_type(
      edge_type = edge_type,
      # edge_width = edge_width,
      edge_width = graph$n,
      edge_color = edge_color,
      edge_alpha = edge_alpha,
      edge_bend = edge_bend,
      edge_cut = edge_cut
    ) +
    # add_edge_layer(
    #   edge_fan,
    #   # ggraph::geom_edge_link(
    #   ggplot2::aes(edge_width = n),
    #   colour = edge_color,
    #   edge_alpha = edge_alpha,
    #   angle_calc = "along",
    #   label_dodge = grid::unit(4.5, "mm"),
    #   end_cap = ggraph::circle(edge_cut, "mm")
    # ) + # afastamento do nó
    # text size. if size constant in all nodes or proportional to its frequency
    {
      if (length(node_size) == 1) {
        ggraph::geom_node_point(
          colour = node_color,
          alpha = node_alpha,
          # size = node_freq$freq,
          size = node_size,
        )
      }
    } +
    {
      if (length(node_size) > 1) {
        ggraph::geom_node_point(
          ggplot2::aes(size = node_size),
          colour = node_color,
          alpha = node_alpha
        )
      }
    } +
    # ggraph::geom_node_text(
    #   ggplot2::aes(
    #     label = name,
    #     # fill = text_color,
    #   ),
    #   size = text_size,
    #   colour = text_color,
    #   repel = TRUE
    # ) +
    # # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
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
          nudge_x = 0,
          nudge_y = 0,
          size = text_size,
          bg.r = 0.20,
          bg.color = text_contour_color,
          color = text_color
        ) # TODO ajustar tamanho minimo e máximo
        # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
      }
    }
  #    text_contour(
  #      text_contour_color,
  #      label = name,
  #      text_size = text_size,
  #      text_color = text_color
  #    )
}
