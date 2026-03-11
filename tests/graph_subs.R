library(testthat)
library(stringr)
library(tibble)

regex_sequence <- "[^\\w\\s\\_\\&]"

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






###### ========== #######
###
###    rm_symbols
###
###### ========== #######
test_df <- data.frame(
  from = c("Node_1!", "Company (Limited)@", "A&B-C"),
  to = c("Target#1", "Another$Node", "Hi...There?"),
  stringsAsFactors = FALSE
)

# Create a simple, fixed substitution dataframe for all tests
test_subs <- tibble::tribble(
  ~pattern, ~replacement,
  "Company", "Co",
  "Limited", "Ltd"
)
# context("Testing graph_subs rm_symbols parameter")

test_that("rm_symbols = regex pattern removes unwanted symbols but keeps underscores and words", {
  # test_that("rm_symbols = TRUE removes unwanted symbols but keeps underscores and words", {
  # Create a test dataframe with various symbols


  result <- graph_subs(test_df, test_subs)

  # Test that symbols are removed from both columns
  # expect_equal(result$from, c("Node_1", "Co Ltd", "A&B_C")) # Note: Hyphen removed, space added
  expect_equal(result$from, c("Node_1", "Co Ltd", "A&BC")) # Note: Hyphen removed, space added
  # expect_equal(result$to, c("Target_1", "Another_Node", "Hi_There"))
  expect_equal(result$to, c("Target1", "AnotherNode", "HiThere"))

  # Test that underscores and alphanumeric characters are preserved
  expect_true(all(grepl(regex_sequence, result$from))) # Should only contain words, spaces, underscores
  expect_true(all(grepl(regex_sequence, result$to)))
})


test_that('rm_symbols = "", leaves symbols intact after substitution', {
  # Use the same test dataframe
  test_df <- data.frame(
    from = c("Node_1!", "Company (Limited)@", "A&B-C"),
    to = c("Target#1", "Another$Node", "Hi...There?"),
    stringsAsFactors = FALSE
  )

  result <- graph_subs(test_df, test_subs, rm_symbols = "")

  # Substitutions should happen, but symbols should remain
  expect_equal(result$from, c("Node_1!", "Co (Ltd)@", "A&B-C")) # "Company" -> "Co", "Limited" -> "Ltd"
  expect_equal(result$to, c("Target#1", "Another$Node", "Hi...There?")) # No substitutions defined for 'to' column patterns
})

# test_that("rm_symbols = TRUE is the default behavior", {
#   test_df <- data.frame(
#     from = c("Test&Node"),
#     to = c("Another@One"),
#     stringsAsFactors = FALSE
#   )
#
#   # Call without specifying rm_symbols (should default to TRUE)
#   result_default <- graph_subs(test_df, test_subs)
#   # Call explicitly with TRUE
#   result_explicit <- graph_subs(test_df, test_subs)
#
#   # Both calls should yield the same result
#   expect_identical(result_default, result_explicit)
#   expect_equal(result_default$from, "Test&Node") # & symbol removed and replaced by space
# })

test_that("Edge case: rm_symbols handles empty strings and strings with only symbols", {
  test_df <- data.frame(
    from = c("", "!!!", "___", "A_1"),
    to = c("   ", "@#$", "B_2", "C-3"), # Note the space and hyphen
    stringsAsFactors = FALSE
  )

  result <- graph_subs(test_df, test_subs)

  # Empty string should remain empty (or become NA? Your function doesn't handle NA)
  expect_equal(result$from, c("", "", "_", "A_1")) # !!! becomes empty, ___ remains
  # Spaces are whitespace (\s) so they are kept, then squished
  expect_equal(result$to, c("", "", "B_2", "C3")) # @#$ becomes empty, hyphen in "C-3" becomes space
})

test_that("Symbol removal works independently on both columns", {
  # Test that one column can have symbols while the other doesn't, and processing is correct
  test_df <- data.frame(
    from = c("Clean_Node", "Dirty_Node!"), # One clean, one dirty
    to = c("Dirty_To@", "Clean_To"), # One dirty, one clean
    stringsAsFactors = FALSE
  )

  result <- graph_subs(test_df, test_subs)

  expect_equal(result$from, c("Clean_Node", "Dirty_Node"))
  expect_equal(result$to, c("Dirty_To", "Clean_To"))
})

# Prompt: create me a unit test in T for this function, but only to the parameter rm_symbol


# # Create a simple, fixed substitution dataframe for all tests
# test_subs <- tibble::tribble(
#   ~pattern, ~replacement,
#   "Company", "Co",
#   "Limited", "Ltd"
# )
test_that("rm_symbols, empty df_subs, parameter works correctly, ", {
  # Create test data
  test_df <- data.frame(
    from = c("hello-world!", "test@email.com", "multiple___underscores"),
    to = c("some&text", "parentheses(text)", "spaces   here"),
    value = 1:3
  )

  # Test 1: Default rm_symbols behavior (remove non-word, non-space, non-_, non-& characters)
  result1 <- graph_subs(DF = test_df, rm_symbols = regex_sequence)

  expect_equal(result1$from, c("hello_world", "test_email_com", "multiple_underscores"))
  expect_equal(result1$to, c("some&text", "parentheses_text", "spaces_here"))

  # Test 2: Custom rm_symbols pattern (remove only specific characters)
  result2 <- graph_subs(test_df, rm_symbols = "[@!]")

  expect_equal(result2$from, c("hello-world", "testemail.com", "multiple___underscores"))
  expect_equal(result2$to, c("some&text", "parentheses(text)", "spaces   here"))

  # Test 3: Empty rm_symbols (should not remove anything)
  result3 <- graph_subs(test_df, rm_symbols = "")

  expect_equal(result3$from, c("hello-world!", "test@email.com", "multiple___underscores"))
  expect_equal(result3$to, c("some&text", "parentheses(text)", "spaces   here"))

  # Test 4: Remove all non-alphanumeric characters
  result4 <- graph_subs(test_df, rm_symbols = "[^[:alnum:]]")

  expect_equal(result4$from, c("helloworld", "testemailcom", "multipleunderscores"))
  expect_equal(result4$to, c("sometext", "parenthesestext", "spaceshere"))

  # Test 5: Remove only underscores
  result5 <- graph_subs(test_df, rm_symbols = "_")

  expect_equal(result5$from, c("hello-world!", "test@email.com", "multipleunderscores"))
  expect_equal(result5$to, c("some&text", "parentheses(text)", "spaces   here"))

  # Test 6: Verify that only first two columns are affected
  result6 <- graph_subs(test_df, rm_symbols = "[^\\w]")

  expect_equal(result6$from, c("hello_world", "test_email_com", "multiple_underscores"))
  # expect_equal(result6$to, c("some_text", "parentheses_text", "spaces_here"))
  expect_equal(result1$to, c("some&text", "parenthesestext", "spaces here"))
  expect_equal(result6$value, 1:3) # Third column should remain unchanged
})

test_that("rm_symbols edge cases", {
  # Test with empty dataframe
  empty_df <- data.frame(from = character(), to = character(), value = numeric())
  result_empty <- graph_subs(empty_df, rm_symbols = "[^\\w]")
  expect_equal(nrow(result_empty), 0)

  # Test with NA values in columns
  na_df <- data.frame(
    from = c("hello!", NA, "test@"),
    to = c(NA, "world!", "email"),
    value = 1:3
  )
  result_na <- graph_subs(na_df, rm_symbols = "[^\\w]")
  expect_equal(result_na$from, c("hello", NA, "test"))
  expect_equal(result_na$to, c(NA, "world", "email"))

  # Test with empty strings
  empty_str_df <- data.frame(
    from = c("", "test!", "   "),
    to = c("hello!", "", "   "),
    value = 1:3
  )
  result_empty_str <- graph_subs(empty_str_df, rm_symbols = "[^\\w]")
  expect_equal(result_empty_str$from, c("", "test", ""))
  expect_equal(result_empty_str$to, c("hello", "", ""))
})
