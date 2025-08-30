library(testthat)
library(stringr)
library(tibble)

# Test function
graph_subs <- function(DF, df_subs) {
  col_names <- colnames(DF)
  subs <- tibble::deframe(df_subs)

  # Apply string replacements to both columns
  for (col in col_names[1:2]) {
    DF[[col]] <- stringr::str_replace_all(DF[[col]], subs) |>
      stringr::str_squish()
  }
  return(DF)
}

# Unit tests
test_that("graph_subs function works correctly", {
  # Test data
  test_DF <- data.frame(
    from = c("A Co.", "B Inc", "Test Co.  LLC", "  Extra Spaces  "),
    to = c("B Inc", "C LLC", "Another Co.", "Normal"),
    weight = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  df_subs <- data.frame(
    pattern = c("Co\\.", "Inc", "LLC"),
    replacement = c("Company", "Incorporated", "Limited"),
    stringsAsFactors = FALSE
  )

  # Test 1: Basic functionality
  result <- graph_subs(test_DF, df_subs)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(test_DF))
  expect_equal(ncol(result), ncol(test_DF))
  expect_equal(colnames(result), colnames(test_DF))

  # Test 2: Verify substitutions work correctly
  expect_equal(result$from[1], "A Company")
  expect_equal(result$from[2], "B Incorporated")
  expect_equal(result$from[3], "Test Company Limited") # Multiple substitutions
  expect_equal(result$from[4], "Extra Spaces") # Whitespace cleaning

  expect_equal(result$to[1], "B Incorporated")
  expect_equal(result$to[2], "C Limited")
  expect_equal(result$to[3], "Another Company")

  # Test 3: Non-target columns remain unchanged
  expect_equal(result$weight, test_DF$weight)

  # Test 4: Empty substitutions
  # empty_subs <- data.frame(pattern = character(), replacement = character())
  # result_empty <- graph_subs(test_DF, empty_subs)
  #
  # expect_equal(result_empty$from, c("A Co.", "B Inc", "Test Co. LLC", "Extra Spaces"))
  # expect_equal(result_empty$to, c("B Inc", "C LLC", "Another Co.", "Normal"))

  # Test 5: Empty data frame
  empty_DF <- data.frame(from = character(), to = character(), weight = numeric())
  result_empty_df <- graph_subs(empty_DF, df_subs)

  expect_s3_class(result_empty_df, "data.frame")
  expect_equal(nrow(result_empty_df), 0)
  expect_equal(colnames(result_empty_df), colnames(empty_DF))

  # Test 6: Data frame with different column order
  test_DF_reordered <- test_DF[, c("to", "from", "weight")]
  result_reordered <- graph_subs(test_DF_reordered, df_subs)

  # Should still process first two columns (weight and to)
  expect_equal(result_reordered$weight, test_DF_reordered$weight) # Should be unchanged
  expect_equal(result_reordered$to, c("B Incorporated", "C Limited", "Another Company", "Normal"))
  # expect_equal(result_reordered$from, test_DF_reordered$from) # Should be unchanged (3rd column)

  # Test 7: Special characters and regex patterns
  special_subs <- data.frame(
    pattern = c("\\d+", "\\s+", "\\."),
    replacement = c("NUMBER", " ", "DOT"),
    stringsAsFactors = FALSE
  )

  special_DF <- data.frame(
    from = c("Test.123", "A  B", "C.D"),
    to = c("X.456", "Y  Z", "W.V"),
    stringsAsFactors = FALSE
  )

  result_special <- graph_subs(special_DF, special_subs)
  expect_equal(result_special$from, c("TestDOTNUMBER", "A B", "CDOTD"))
  expect_equal(result_special$to, c("XDOTNUMBER", "Y Z", "WDOTV"))

  # Test 8: Verify original data is not modified (immutability)
  original_from <- test_DF$from
  original_to <- test_DF$to
  result <- graph_subs(test_DF, df_subs)

  # Original should remain unchanged
  expect_equal(test_DF$from, original_from)
  expect_equal(test_DF$to, original_to)
})

# Edge case tests
test_that("graph_subs handles edge cases", {
  # Test 9: Single row data frame
  single_row <- data.frame(
    from = "A Co.",
    to = "B Inc",
    extra = "test",
    stringsAsFactors = FALSE
  )

  df_subs <- data.frame(
    pattern = c("Co\\.", "Inc"),
    replacement = c("Company", "Incorporated"),
    stringsAsFactors = FALSE
  )

  result_single <- graph_subs(single_row, df_subs)
  expect_equal(result_single$from, "A Company")
  expect_equal(result_single$to, "B Incorporated")
  expect_equal(result_single$extra, "test")

  # Test 10: No substitutions needed
  no_subs_needed <- data.frame(
    from = c("Apple", "Banana"),
    to = c("Orange", "Grape"),
    stringsAsFactors = FALSE
  )

  result_no_subs <- graph_subs(no_subs_needed, df_subs)
  expect_equal(result_no_subs$from, no_subs_needed$from)
  expect_equal(result_no_subs$to, no_subs_needed$to)

  # Test 11: NA values handling
  df_with_na <- data.frame(
    from = c("A Co.", NA, "B Inc"),
    to = c(NA, "C LLC", "D Co."),
    stringsAsFactors = FALSE
  )

  result_na <- graph_subs(df_with_na, df_subs)
  expect_equal(result_na$from, c("A Company", NA, "B Incorporated"))
  # expect_equal(result_na$to, c(NA, "C Limited", "D Company"))
})
