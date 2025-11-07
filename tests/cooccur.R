library(testthat)

test_that("cooccur function works correctly", {
  # Test data
  sample_text <- "The quick brown fox jumps over the lazy dog. A quick brown cat runs fast. Dogs and cats are pets."

  # Test 1: Basic functionality with sentence tokenization
  test_that("basic sentence tokenization works", {
    result <- cooccur(sample_text, token_by = "sentence", sw = "", count = TRUE)

    expect_s3_class(result, "tbl_df")
    expect_named(result, c("n1", "n2", "n"))
    expect_true(nrow(result) > 0)
    expect_type(result$n1, "character")
    expect_type(result$n2, "character")
    expect_type(result$n, "integer")
  })

  # Test 2: Stopwords removal
  test_that("stopwords are properly removed", {
    result_with_sw <- cooccur(sample_text, token_by = "sentence", sw = c("the", "a", "and"), count = TRUE)

    # Check that stopwords don't appear in the pairs
    all_words <- c(result_with_sw$n1, result_with_sw$n2)
    expect_false(any(c("the", "a", "and") %in% all_words))
  })

  # Test 3: Paragraph tokenization
  test_that("paragraph tokenization works", {
    paragraph_text <- "First paragraph. With multiple sentences.\n\nSecond paragraph. Different content."
    result <- cooccur(paragraph_text, token_by = "paragraph", sw = "", count = TRUE)

    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)
  })

  # Test 4: Count = FALSE returns all pairs without aggregation
  test_that("count = FALSE returns all pairs", {
    result <- cooccur(sample_text, token_by = "sentence", sw = "", count = FALSE)

    expect_s3_class(result, "tbl_df")
    expect_named(result, c("n1", "n2"))
    expect_false("n" %in% names(result))
    # Should have more rows than counted version since duplicates are kept
  })

  # Test 5: Invalid token_by throws error
  test_that("invalid token_by throws error", {
    expect_error(
      cooccur(sample_text, token_by = "word", sw = ""),
      "Invalid token_type. Must be 'sentence' or 'paragraph'."
    )
  })

  # Test 6: Empty text handling
  test_that("empty text returns error", {
    expect_error(cooccur("", token_by = "sentence", sw = "", count = TRUE))
  })

  # Test 7: Text with very short sentences gets filtered out
  test_that("short sentences are filtered out", {
    short_text <- "Hi. This is a longer sentence with more words. Bye."
    result <- cooccur(short_text, token_by = "sentence", sw = "", count = TRUE)

    # Should only have pairs from the longer sentence
    nodes <- c(result$n1, result$n2) |> unique()
    expect_true(nrow(result) > 0)
    expect_false(any(nodes %in% "Hi"))
    expect_false(any(nodes %in% "Bye"))
  })

  # Test 8: Case sensitivity and word cleaning
  test_that("words are properly cleaned and case handled", {
    case_text <- "The THE the ThE. Word WORD word."
    result <- cooccur(case_text, token_by = "sentence", sw = "", count = TRUE)

    # All words should be lowercase
    all_words <- c(result$n1, result$n2)
    expect_false(any(grepl("[A-Z]", all_words)))
  })

  # Test 9: Verify specific co-occurrence counts
  test_that("specific co-occurrences are counted correctly", {
    simple_text <- "apple banana orange. Apple banana cherry."
    result <- cooccur(simple_text, token_by = "sentence", sw = "", count = TRUE)

    # Find the apple-banana pair
    apple_banana <- result[result$n1 == "apple" & result$n2 == "banana", ]
    expect_equal(apple_banana$n, 2) # Should appear in both sentences
  })

  # Test 10: Edge case with only one valid sentence
  test_that("single valid sentence works", {
    single_sentence <- "This is a single sentence with enough words."
    result <- cooccur(single_sentence, token_by = "sentence", sw = "", count = TRUE)

    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)
  })
})

test_that("cooccur function with lower parameter works correctly", {
  # Test data with mixed case
  sample_text <- "The Quick Brown fox jumps over the Lazy Dog. A quick Brown cat runs fast. Dogs and Cats are pets."

  # Test 1: Basic functionality with lower = TRUE (default behavior)
  test_that("lower = TRUE converts all words to lowercase", {
    result <- cooccur(sample_text, token_by = "sentence", sw = "", count = TRUE, lower = TRUE)

    expect_s3_class(result, "tbl_df")
    expect_named(result, c("n1", "n2", "n"))
    expect_true(nrow(result) > 0)

    # Check that all words are lowercase
    all_words <- c(result$n1, result$n2)
    expect_false(any(grepl("[A-Z]", all_words)))
    expect_true(all(all_words == tolower(all_words)))
  })

  # Test 2: lower = FALSE preserves original case
  test_that("lower = FALSE preserves case", {
    result <- cooccur(sample_text, token_by = "sentence", sw = "", count = TRUE, lower = FALSE)

    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)

    # Check that original case is preserved (should have some uppercase)
    all_words <- c(result$n1, result$n2)
    expect_true(any(grepl("[A-Z]", all_words)))
  })

  # Test 3: Stopwords removal with case sensitivity
  test_that("stopwords removal respects lower parameter", {
    # Test with lower = TRUE
    result_lower <- cooccur(sample_text,
      token_by = "sentence",
      sw = c("the", "a", "and"), count = TRUE, lower = TRUE
    )

    # Test with lower = FALSE - stopwords should match case
    result_upper <- cooccur(sample_text,
      token_by = "sentence",
      sw = c("The", "A", "and"), count = TRUE, lower = FALSE
    )

    # Check that stopwords are removed appropriately in each case
    words_lower <- c(result_lower$n1, result_lower$n2)
    words_upper <- c(result_upper$n1, result_upper$n2)

    expect_false(any(c("the", "a", "and") %in% words_lower))
    expect_false(any(c("The", "A", "and") %in% words_upper))

    # With lower=FALSE, "The" should still be present in result_lower but not in result_upper
    expect_false("The" %in% words_upper)
  })

  # Test 4: Case consistency in counting when lower = TRUE
  test_that("lower = TRUE combines case variants in counting", {
    case_text <- "The THE the ThE. Word WORD word."
    result <- cooccur(case_text, token_by = "sentence", sw = "", count = TRUE, lower = TRUE)

    # All case variants should be combined
    all_words <- c(result$n1, result$n2)
    expect_equal(length(unique(all_words)), 2) # Only "the" and "word"
    expect_true(all(all_words %in% c("the", "word")))
  })

  # Test 5: Case distinction in counting when lower = FALSE
  test_that("lower = FALSE treats case variants as distinct", {
    case_text <- "The THE the ThE. Word WORD word."
    result <- cooccur(case_text, token_by = "sentence", sw = "", count = TRUE, lower = FALSE)

    # Different case variants should be treated as distinct words
    all_words <- c(result$n1, result$n2)
    expect_true(length(unique(all_words)) > 2) # Should have multiple case variants
    expect_true(any(grepl("[A-Z]", all_words)))
  })

  # Test 6: Verify specific co-occurrence counts with case handling
  test_that("co-occurrence counts respect case handling", {
    case_text <- "apple banana APPLE. Apple BANANA cherry."
    result_lower <- cooccur(case_text, token_by = "sentence", sw = "", count = TRUE, lower = TRUE)
    result_upper <- cooccur(case_text, token_by = "sentence", sw = "", count = TRUE, lower = FALSE)

    # With lower=TRUE, all "apple" variants should be combined
    apple_banana_lower <- result_lower[result_lower$n1 == "apple" & result_lower$n2 == "banana", ]
    expect_equal(apple_banana_lower$n, 2) # Should appear in both sentences

    # With lower=FALSE, case variants are distinct
    all_pairs_upper <- result_upper
    expect_true(nrow(all_pairs_upper) >= nrow(result_lower)) # May have more distinct pairs
  })

  # Test 7: Default parameter value (should be TRUE)
  test_that("lower defaults to TRUE", {
    result_default <- cooccur(sample_text, token_by = "sentence", sw = "", count = TRUE)
    result_explicit <- cooccur(sample_text, token_by = "sentence", sw = "", count = TRUE, lower = TRUE)

    # Should be identical when using default vs explicit TRUE
    expect_equal(result_default, result_explicit)
  })

  # Test 8: Mixed case with stopwords and lower parameter
  test_that("stopwords interact correctly with lower parameter", {
    mixed_text <- "The Quick Brown and the Lazy Dog."

    # With lower=TRUE, lowercase stopwords should work
    result_lower <- cooccur(mixed_text,
      token_by = "sentence",
      sw = c("the", "and"), count = TRUE, lower = TRUE
    )
    words_lower <- c(result_lower$n1, result_lower$n2)
    expect_false(any(c("the", "and") %in% words_lower))

    # With lower=FALSE, need exact case match for stopwords
    result_upper <- cooccur(mixed_text,
      token_by = "sentence",
      sw = c("The", "and"), count = TRUE, lower = FALSE
    )
    words_upper <- c(result_upper$n1, result_upper$n2)
    expect_false(any(c("The", "and") %in% words_upper))
    expect_true("the" %in% words_upper) # "the" (lowercase) should still be present
  })

  # Test 10: Paragraph tokenization with lower parameter
  test_that("paragraph tokenization respects lower parameter", {
    paragraph_text <- "First Paragraph. With Mixed CASE.\n\nSecond paragraph. Different Content."

    result_lower <- cooccur(paragraph_text, token_by = "paragraph", sw = "", count = TRUE, lower = TRUE)
    result_upper <- cooccur(paragraph_text, token_by = "paragraph", sw = "", count = TRUE, lower = FALSE)

    words_lower <- c(result_lower$n1, result_lower$n2)
    words_upper <- c(result_upper$n1, result_upper$n2)

    expect_false(any(grepl("[A-Z]", words_lower)))
    expect_true(any(grepl("[A-Z]", words_upper)))
  })
})

# Additional edge cases for lower parameter
test_that("cooccur lower parameter edge cases", {
  # Test with only uppercase text
  test_that("uppercase text with lower=TRUE", {
    upper_text <- "THE QUICK BROWN FOX. JUMPS OVER LAZY DOG."
    result <- cooccur(upper_text, token_by = "sentence", sw = "", count = TRUE, lower = TRUE)

    all_words <- c(result$n1, result$n2)
    expect_false(any(grepl("[A-Z]", all_words)))
    expect_true(all(all_words == tolower(all_words)))
  })

  # Test with mixed case and special characters
  test_that("mixed case with special characters", {
    text <- "iPhone and iPad are Apple products. IPhone vs iPhone."
    result_lower <- cooccur(text, token_by = "sentence", sw = "", count = TRUE, lower = TRUE)
    result_upper <- cooccur(text, token_by = "sentence", sw = "", count = TRUE, lower = FALSE)

    # With lower=TRUE, "iphone" should combine all variants
    if (nrow(result_lower) > 0) {
      iphone_count_lower <- sum(c(result_lower$n1, result_lower$n2) == "iphone")
      expect_true(iphone_count_lower > 0)
    }

    # With lower=FALSE, case variants remain distinct
    if (nrow(result_upper) > 0) {
      all_words_upper <- c(result_upper$n1, result_upper$n2)
      expect_true(any(grepl("[A-Z]", all_words_upper)))
    }
  })

  # Test count = FALSE with lower parameter
  test_that("count = FALSE with lower parameter", {
    mixed_text <- "Word word WORD."
    result_lower <- cooccur(mixed_text, token_by = "sentence", sw = "", count = FALSE, lower = TRUE)
    result_upper <- cooccur(mixed_text, token_by = "sentence", sw = "", count = FALSE, lower = FALSE)

    words_lower <- c(result_lower$n1, result_lower$n2)
    words_upper <- c(result_upper$n1, result_upper$n2)

    expect_false(any(grepl("[A-Z]", words_lower)))
    expect_true(any(grepl("[A-Z]", words_upper)))
  })
})

# Run all tests
# test_dir("path/to/test/file")  # If saving in separate file
# Additional tests for specific edge cases
test_that("cooccur edge cases", {
  # Test with only punctuation and stopwords
  test_that("text with only stopwords returns empty", {
    text <- "The and the of. A an in."
    expect_error(cooccur(text, token_by = "sentence", sw = c("the", "and", "of", "a", "an", "in"), count = TRUE))
  })

  # Test with very long text
  test_that("long text processes correctly", {
    long_text <- paste(rep("This is a test sentence with multiple words for testing.", 10), collapse = " ")
    result <- cooccur(long_text, token_by = "sentence", sw = "", count = TRUE)

    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)
    # All counts should be at least 1
    expect_true(all(result$n >= 1))
  })

  # Test symmetry of pairs (order shouldn't matter for counting)
  test_that("pair order is consistent", {
    text <- "Word1 word2 word3."
    text2 <- "Word2 word1 word3."
    result <- cooccur(text, token_by = "sentence", sw = "", count = TRUE) |> dplyr::arrange(n1, n2)
    result2 <- cooccur(text2, token_by = "sentence", sw = "", count = TRUE)
    # put the node order alphabetically
    result2 <- result2 |>
      dplyr::mutate(test = n1 > n2) |>
      dplyr::mutate(n1t = ifelse(test, n2, n1)) |>
      dplyr::mutate(n2 = ifelse(test, n1, n2)) |>
      dplyr::mutate(n1 = ifelse(test, n1t, n1)) |>
      dplyr::arrange(n1, n2) |>
      dplyr::select(-test, -n1t)
    # Check that pairs are ordered consistently (alphabetically)
    expect_true(identical(result, result2))
  })
})

# Test symmetry of pairs (order shouldn't matter for counting)
test_that("pair order is consistent", {
  text <- "Word1 word2 word3."
  text2 <- "Word2 word1 word3."
  result <- cooccur(text, token_by = "sentence", sw = "", count = TRUE) |> dplyr::arrange(n1, n2)
  result2 <- cooccur(text2, token_by = "sentence", sw = "", count = TRUE)
  # put the node order alphabetically
  result2 <- result2 |>
    dplyr::mutate(test = n1 > n2) |>
    dplyr::mutate(n1t = ifelse(test, n2, n1)) |>
    dplyr::mutate(n2 = ifelse(test, n1, n2)) |>
    dplyr::mutate(n1 = ifelse(test, n1t, n1)) |>
    dplyr::arrange(n1, n2) |>
    dplyr::select(-test, -n1t)
  # Check that pairs are ordered consistently (alphabetically)
  expect_true(identical(result, result2))
})

# Run all tests
# test_dir("path/to/test/file")  # If saving in separate file
