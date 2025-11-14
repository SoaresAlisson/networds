# Functions here are copies of some functions from the packages SoaresAlisson/sto. I copied here to reduce dependencies
# if one day sto will be in CRAN, than this files and functions will be removed.

#' A gsub to be used easily with native pipe |>
#' gsub2 is just a wrapper around gsub
#' @param x data to search
#' @param arg1 pattern
#' @param arg2 replacement
#' @param ... arguments passed to gsub.
#' @noRd
#'
#' @examples
#' c("a", "b", "c", "d") |> gsub2("a", "x")
#' # in the case of character
#' "a b c d" |> gsub2("a", "x")
#' # If no second argument is provided, than it will erase the pattern:
#' "'bla bla1 'bla" |> gsub2("'")
gsub2 <- function(x, arg1, arg2 = "", ic = FALSE, ...) {
  gsub(arg1, arg2, x, ignore.case = ic)
}

#' @noRd
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


#' tokenize words
#'
#' @description
#' A word tokenizer, designed to keep compounded words, like "Covid-19"" or URLs
#'
#' @param txt input text
#' @param lower convert words to lowercase. Default TRUE.
#'
#' @export
#'
#' @examples
#' @examples
#' txt <- "Was ice-cream better in Soviet Union or in New York? Bla bla, ble. The package https://quanteda.io/reference/index.html is very Nice! There are also compounded words like New_Jersey. The Covid-19 pandemic is very bad. Some,text,like;csv"
#' tokenize_words(txt)
#'
#' txt2 <- c("Was ice-cream better before?", "The Covid-19 brought many problems.")
#' tokenize_words(txt2)
tokenize_words <- function(txt, lower = TRUE, unlist = FALSE) {
  txt <- txt |>
    strsplit("[ \n\t\r,;\\|]") |>
    lapply(\(x){
      x |>
        gsub2("[!?,;]") |>
        gsub2("\\W+$") |>
        stringi::stri_remove_empty()
    })

  if (lower) {
    txt <- lapply(txt, tolower)
  }

  if (unlist) {
    txt <- unlist(txt)
  }

  return(txt)
}
