#' rule based entity graph extractor
#'
#' @description
#' Extract a non directional graph based on co-occurrence in the token.
#' It extracts only if two entities are mentioned in the same token (sentence or paragraph)
#'
#' @param text an input text
#' @param using sentence or paragraph to tokenize
#' @param connect lowercase connectors, like the "von" in "John von Neumann".
#' @param sw stopwords vector.
#' @param count if TRUE (default) count the frequency of nodes and return it in the order of its frequency
#' @param loop if TRUE, it will not remove foops, a node pointing to itself.
#'
#' @export
#'
#' @examples
#' text <- "John Does lives in New York in United States of America. He  is a passionate jazz musician, often playing in local clubs."
#' extract_graph_rb(text)
extract_graph_rb <- function(
  text,
  using = "sentences",
  connect = connectors("misc"),
  sw = c("of", "the"),
  count = TRUE,
  loop = FALSE
) {
  list_ent <- text |> extract_relation(using, connect, sw)
  graph <- tibble::tibble(n1 = as.character(""), n2 = as.character(""))
  # list_length <- list_ent |> length()

  graph <- lapply(list_ent, \(e) {
    items <- e |> combn(2, simplify = FALSE)

    items_length <- length(items)

    lapply(1:items_length, \(x) {
      line <- unlist(c(items[x][1], items[x][2]))
      graph <- rbind(graph, line)
      graph
    }) |>
      dplyr::bind_rows() |>
      dplyr::filter(n1 != "")
  }) |>
    dplyr::bind_rows()

  if (!loop) {
    graph <- graph |>
      dplyr::mutate(loop = (n1 == n2)) |>
      dplyr::filter(loop == FALSE) |>
      dplyr::select(-loop)
  }
  if (!count) {
    return(graph)
  } else {
    graph_counted <- graph |> dplyr::count(n1, n2, sort = TRUE)
    return(graph_counted)
  }
}
