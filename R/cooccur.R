#' Get cooccurrence of all words
#'
#' @param text Text
#' @param token_by Tokenize by sentence or paragraph
#' @param sw Stopwords to be removed
#' @param lower Convert words to lowercase. If the text is passed in all lowercase, it can return false sentence and paragraph tokenization.
#' @param count Return count of words (default TRUE)
#'
#' text |> cooccur()
cooccur <- function(text, token_by = "sentence",
                    sw = "", lower = TRUE,
                    count = TRUE) {
  # token_by = "sentence"; sw = ""
  # sw <- sto::s2v("of by the a an in and was")

  if (nchar(text) < 1) {
    stop("the text is empty")
  }

  if (token_by == "sentence") {
    tokens <- tokenizers::tokenize_sentences(text)
  } else if (token_by == "paragraph") {
    tokens <- tokenizers::tokenize_paragraphs(text)
  } else {
    stop("Invalid token_type. Must be 'sentence' or 'paragraph'.")
  }

  # Clean stopwords
  word_tokens_list <- unlist(tokens) |> tokenizers::tokenize_words(lowercase = lower)
  cleaned_tokens <- lapply(word_tokens_list, function(x) Filter(function(word) !word %in% sw, x))

  # if list element smaller than 2, then erase it
  list_length_valid <- cleaned_tokens |>
    lapply(length) |>
    lapply(\(x) {
      x > 2
    }) |>
    unlist()

  cleaned_tokens <- cleaned_tokens[list_length_valid]

  if (length(cleaned_tokens) < 1) {
    stop("No words left after stopwords removal")
  }

  comb_list <- cleaned_tokens |>
    lapply(combn, 2, simplify = FALSE) |>
    unlist()

  pairs <- tibble::tibble(
    n1 = comb_list[seq(1, length(comb_list), by = 2)],
    n2 = comb_list[seq(2, length(comb_list), by = 2)]
  )

  if (count) {
    pairs <- pairs |> dplyr::count(n1, n2, sort = TRUE)
  }

  return(pairs)
}
