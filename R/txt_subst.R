#' Replace text using rule based proper name
#'
#' @description
#' given a text and the proper name connectors (`connectors()`), it returns the text
#' with its proper names as one single word, with underscore biding them. For 
#' example, the text "John Does lives in New York in United States of America" 
#' becomes "John_Does lives in New_York in United_States_of_America."
#'
#' @param txt the input text
#' @param connect type of connectors
#' @param sw the stopwords
#' @param underscore default TRUE
# TODO
#'
#' @examples
#' txt_subst("John Does lives in New York in United States of America.")
#'
txt_subst <- function(
    txt,
    connect = connectors("all"),
    sw = "",
    underscore = TRUE) {
  entities <- txt |>
    extract_entity_rb(connect = connect, sw = sw, underscore = underscore) |>
    unique() |>
    sort() |>
    grep(x = _, "_", value = TRUE)

  df_subst <- data.frame(to = entities) |>
    dplyr::mutate(from = gsub(x = to, "_", " ")) |>
    dplyr::select(from, to)

  new_txt <- txt |>
    stringr::str_replace_all(tibble::deframe(df_subst))

  return(new_txt)
}
