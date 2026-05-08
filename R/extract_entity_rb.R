#' A rule based entity extractor
#'
#' @description
#' extracts the entity from a text using regex. This regex captures all uppercase words, words that begin with upper case. If there is sequence of this patterns together, this function also captures.
#' In the case of proper names with common lower case connectors like "Wwwww of Wwwww" this function also captures the connector and the subsequent uppercase words.
#'
#' @param text an input text
#' @param connect a vector of lowercase connectors. Use use your own, or use the function "connector" to obtain some patterns.
#' @param sw a vector of stopwords
#' @param underscore keep underscore to make compounded words a unique word?
#'
#' @export
#'
#' @examples
#' "John Does lives in New York in United States of America." |> extract_entity()
#' "João Ninguém mora em São José do Rio Preto. Ele esteve antes em Sergipe" |> extract_entity(connect = connectors("pt"))
#' text |> extract_entity()
extract_entity_rb <- function(
    text,
    connect = connectors("misc"),
    sw = "the",
    underscore = TRUE) {
  # connectors <- connectors |> s2v()
  connector <- paste(connect, collapse = "|") |> gsub("(.*)", "(\\1)", x = _)

  # rgx_ppn <- paste0("(", rgx_word, "+ ?)+", "", connector, "? (", rgx_word, " ?)*")
  rgx_ppn <- paste0(
    "(",
    rgx_word,
    "+ ?)+",
    "(",
    connector,
    "? (",
    rgx_word,
    " ?)+)*"
  )

  text_vec <- text |>
    stringr::str_extract_all(rgx_ppn) |>
    unlist() |>
    stringr::str_trim()
  # trimws()

  # deleting stopword elements
  text_vec <- text_vec[!text_vec %in% stringr::str_to_title(sw)]

  if (underscore) {
    gsub(x = text_vec, " ", "_")
  } else {
    text_vec
  }
}
