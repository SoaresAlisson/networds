#' reduce the frequency of node pairs per document
#'
#' This function transforms a co-occurrence dataframe by capping counts above a threshold,
#' the input must be the tibble from cooccur_words(output=df2).
#' It groups the data by two columns, sums the counts within each group, and arranges the results in descending order.
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
#' reduce_freq(cooc_data, threshold = 5)
reduce_freq <- function(cooc, threshold) {
  cooc |>
    mutate(n = ifelse(n > threshold, threshold, n)) |>
    group_by(n1, n2) |>
    summarize(n = sum(n)) |>
    ungroup() |>
    arrange(-n)
}
