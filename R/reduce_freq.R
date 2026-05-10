#' Set a maximum value to the frequency of pairs
#'
#' Transforms the frequency in a co-occurrence dataframe, setting an stipulated
#' maximum value of words frequency per document.
#' The input must be the tibble from cooccur_words(output=df2).
#' It groups the data by two columns, sums the counts within each group, and
#' arranges the results in descending order.
#' It is useful to reduce the maximum value that each document contribute to
#' the corpus with its node pairs, avoiding that a highly frequent word pairs
#' in a single document or in only a few documents, but rare or absent in
#' other documents be taken as representative of the whole corpus.
#'
#' @param cooc A dataframe from cooccur_words(output=df2).
#' @param threshold The threshold max value above which counts are capped in each document.
#' @return A transformed dataframe with summed counts arranged by their magnitude.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' cooc_data <- data.frame(n1 = c(1, 1, 2), n2 = c(3, 4, 3), n = c(5, 6, 7))
#' cooc_data
#' reduce_freq(cooc_data, threshold = 5)
reduce_freq <- function(cooc, threshold) {
  cooc |>
    dplyr::mutate(n = ifelse(n > threshold, threshold, n)) |>
    dplyr::group_by(n1, n2) |>
    dplyr::summarize(n = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::arrange(-n)
}
