# nao gerar rd
s2v <- function(char, sep = " |\\n|\\t|\\r", wss = "_", print = FALSE) {
  vec <- char |>
    strsplit(sep) |>
    unlist() |>
    gsub2(" +", " ") |> # strip extra white spaces
    gsub2("[,;]", " ") |> # strip comma
    stringi::stri_remove_empty() |>
    gsub2("^ | $", "") |> # strip extra white spaces
    gsub2(wss, " ")

  if (!print) {
    return(vec)
  } else {
    vec |>
      paste(collapse = "', '") |>
      gsub2("^", "c('") |>
      gsub2("$", "')")
  }
}


#' count a vector of elements
#'
#' @description
#' count a vector of elements, arrange it or not in frequency order, and returns a tibble
#'
#' @export
#' @examples
#' vec <- s2v("a a b c a b a z z z c d e")
#' vec |> count_vec()
count_vec <- function(vec, sort_n = TRUE) {
  vec_count <- vec |>
    unlist() |>
    plyr::count()
  if (sort_n) {
    vec_count <- vec_count |> dplyr::arrange(-freq)
  }
  tibble::as_tibble(vec_count)
}

