#' Get cooccurrence of all words
#'
#' @param text Text
#' @param token_by Tokenize by sentence or paragraph
#' @param sw Stopwords to be removed
#' @param lower Convert words to lowercase. If the text is passed in all
#' lowercase, it can return false sentence and paragraph tokenization. It is
#' advised to use lowercase.
# @param compound_words wich words will be united by underscore to be
# considered as a single word.
#' @param loop if FALSE, self referential nodes (e.g. n1=x and also n2=x) will
#' be excluded. Default FALSE.
#' @param output as 1) a single tibble/dataframe ("tlb", "df", "tibble",
#' "datafame"); 2) as a list of dataframes with cooccurrence per vector
#' element ("lst" or "list"); or 3) as raw list. This format is the most raw
#' output of this function; 4) "df2", tibble/dataframe with the doc numbers.
#' @param count Return count of words (default TRUE)
#'
#' @export
#'
#' @examples
#' txt <- "Lorem Ipsum. The Ipsum John. Dolor est"
#' txt |> cooccur_words()
cooccur_words <- function(
  text,
  sw = "",
  token_by = "sentence",
  lower = TRUE,
  # compound_words = "",
  loop = FALSE,
  output = "df",
  count = TRUE
) {
  text_length <- length(text)

  if (text_length > 1 && output %in% df_names) {
    message(
      "You provided a vector of ",
      text_length,
      " elements instead of one. No problem, but these will be collapsed into a single element, with a final punctuation mark added to each, to ensure it is treated as different sentences in the process of tokenization."
    )
    text <- paste(text, collapse = ". ")
  }

  if (length(text) == 1 && any(nchar(text) < 1)) {
    stop("The text provided is empty")
  }

  if (token_by %in% c("sentence", "sent")) {
    # tokens <- tokenizers::tokenize_sentences(text)
    message("tokenizing sentences...")
    tokens <- lapply(text, \(X) {
      tokenizers::tokenize_sentences(X) |> unlist()
    })
  } else if (token_by %in% c("par", "paragraph")) {
    # tokens <- tokenizers::tokenize_paragraphs(text)
    # tokens <- lapply(text, tokenizers::tokenize_paragraphs)
    tokens <- plyr::llply(
      text,
      \(X) {
        # tokens <- lapply(text, \(X) {
        tokenizers::tokenize_paragraphs(X) |> unlist()
      },
      .progress = "text"
      # .parallel = TRUE
    )
  } else {
    stop("Invalid token_type. Must be 'sentence' or 'paragraph'.")
  }

  # Clean stopwords
  # word_tokens_list <- unlist(tokens) |>
  # word_tokens_list <- lapply(tokens, \(X) {
  message("tokenizing words...")
  word_tokens_list <- plyr::llply(
    tokens,
    \(X) {
      # tokenizers::tokenize_words(X, lowercase = lower)
      tokenize_by_words(X, lower = lower)
    },
    .progress = "text"
    # .parallel = TRUE
  )
  # lapply(\(X) {
  #   tokenize_by_words(X, lower = lower)
  # })

  if (length(sw) > 0 || sw != "") {
    # if (sw != "") {
    # cleaned_tokens <- lapply(
    #   word_tokens_list,
    #   \(X) Filter(function(word) !word %in% sw, X)
    # )

    # Apply stopword filtering to each vector at the deepest level
    cleaned_tokens <- lapply(word_tokens_list, function(outer_elem) {
      lapply(outer_elem, function(inner_vector) {
        # Filter out stopwords from each vector
        Filter(function(word) !word %in% sw, inner_vector)
      })
    })
  } else {
    cleaned_tokens <- as.list(word_tokens_list)
    # cleaned_tokens <- word_tokens_list
  }

  # if list element smaller than 2 elements, then erase it
  # in R I have a nested list. I want to remove all list elements that has vectors with lesser than 2 elements. In the example, only [[3]][[1]] must be deleted..
  # list_length_valid <-
  #   lapply(seq_along(cleaned_tokens), \(X) {
  #     length(cleaned_tokens[[X]])
  #   }) |>
  #   lapply(\(x) {
  #     x >= 2
  #   }) |>
  #   unlist()
  #
  # cleaned_tokens <- cleaned_tokens[list_length_valid]
  cleaned_tokens <- filter_nested_list(cleaned_tokens)

  if (length(cleaned_tokens) < 1) {
    stop("No words left after stopwords removal")
  }

  # comb_list <- cleaned_tokens |>
  #   lapply(combn, 2, simplify = FALSE)
  # Apply combn to each vector at the deepest level
  comb_list <- lapply(cleaned_tokens, function(outer_elem) {
    lapply(outer_elem, function(inner_elem) {
      # Generate all pairs of words from the vector
      combn(inner_elem, 2, simplify = FALSE)
    })
  })

  # from the cooccur list, create a tibble
  tbl_from_cooccur_list <- function(aList) {
    tibble::tibble(
      n1 = aList[seq(1, length(aList), by = 2)],
      n2 = aList[seq(2, length(aList), by = 2)]
    ) |>
      sort_columns_alphabetically()
  }

  if (output %in% df_names) {
    # comb_vector <- unlist(cleaned_tokens)
    comb_vector <- unlist(comb_list)

    pairs <- tbl_from_cooccur_list(comb_vector)

    if (!loop) {
      pairs <- pairs |> dplyr::filter(!n1 == n2)
    }
    if (count) {
      pairs <- pairs |> dplyr::count(n1, n2, sort = TRUE)
    }
    return(pairs)
  } else if (output %in% lst_names) {
    my_list <- lapply(seq_along(comb_list), \(X) {
      comb_list[[X]] |>
        unlist() |>
        tbl_from_cooccur_list()
    })

    if (!count) {
      return(my_list)
    } else {
      lapply(my_list, count_graphs)
    }
  } else if (output == "df2") {
    time_init <- Sys.time()
    message(
      "Each vector element will be considered as a different document. The frequency of co-occurrence per document takes much more time than per corpus."
    )
    message("Process began at ", format(time_init, "%H:%M:%S %Y-%m-%d"))

    total <- length(comb_list)
    # lst <- lapply(seq_along(comb_list), \(X) {
    lst <- plyr::llply(
      seq_along(comb_list),
      function(X) {
        percent <- ((X / total) * 100)
        # message(
        # cat(
        #   "\rRunning: ",
        #   X,
        #   " of ",
        #   total,
        #   " - ",
        #   # round(percent, digits = 2),
        #   sprintf("%1.1f%%", percent),
        #   "%"
        # )
        # cat("\n") # New line at the end

        comb_list[[X]] |>
          unlist() |>
          tbl_from_cooccur_list() |>
          count_graphs() |>
          dplyr::mutate(doc = X)
        # dplyr::mutate(doc = as.factor(X))
      },
      .progress = "text"
    )

    message("Binding dataframes")

    df <- dplyr::bind_rows(lst) |>
      dplyr::select(doc, n1, n2, n)

    time_end <- Sys.time()
    elapsed <- (time_end - time_init) |>
      as.numeric(units = "mins") |>
      round(digits = 2)

    # time_diff <- time_end - time_init
    # as.numeric(time_diff, units = "secs")
    message("------------------------------")
    message("Finished in ", elapsed, " minutes.")
    return(df)
  } else if (output == "raw") {
    return(pairs)
  } else {
    stop('Parameter "output" invalid: "', output, '"')
  }
}
