library(testthat)

test_that("escape_regex escapes special characters correctly", {
  # Test individual special characters
  expect_equal(escape_regex("."), "\\.")
  # expect_equal(escape_regex("["), "\\[")
  # expect_equal(escape_regex("("), "\\(")
  # expect_equal(escape_regex("*"), "\\*")
  # expect_equal(escape_regex("+"), "\\+")
  # expect_equal(escape_regex("?"), "\\?")
  # expect_equal(escape_regex("{"), "\\{")
  # expect_equal(escape_regex("|"), "\\|")
  # expect_equal(escape_regex("^"), "\\^")
  # expect_equal(escape_regex("$"), "\\$")

  # Test combinations
  expect_equal(escape_regex("Mr.John"), "Mr\\.John")
})


test_that("escape_regex escapes dot character", {
  expect_equal(escape_regex("file.txt"), "file\\.txt")
})

test_that("escape_regex escapes dot character", {
  expect_equal(escape_regex("file.txt"), "file\\.txt")
})

test_that("escape_regex handles empty strings and non-special strings", {
  expect_equal(escape_regex(""), "")
  expect_equal(escape_regex("abc"), "abc")
  expect_equal(escape_regex("123"), "123")
  expect_equal(escape_regex("abc123"), "abc123")
})

test_that("escape_regex with word_delim = TRUE adds word boundaries", {
  # Basic cases
  expect_equal(escape_regex("Dr", word_delim = TRUE), "\\bDr\\b")
  expect_equal(escape_regex("Dr. John", word_delim = TRUE), "\\bDr\\. John\\b")
  expect_equal(escape_regex("Mr.", word_delim = TRUE), "\\bMr\\.\\b")
  expect_equal(escape_regex("Mr. Smith", word_delim = TRUE), "\\bMr\\. Smith\\b")


  # Multiple strings
  input <- c("Mr.", "Dr")
  expected <- c("\\bMr\\.\\b", "\\bDr\\b")
  expect_equal(escape_regex(input, word_delim = TRUE), expected)
})



test_that("escape_regex works correctly with : Mr. Smith like names", {
  # Test that escaped strings work in regex
  text <- "Mr. Smith and Mrs. Jones. AMr. Smith and another Mrs Smith"

  # Without word boundaries (matches both "Mr." and "AMr.")
  pattern <- escape_regex("Mr. Smith")
  matches <- regmatches(text, gregexpr(pattern, text))
  expect_equal(length(unlist(matches)), 2) # Matches both "Mr." and "AMr."

  # With word_delim )
  pattern_wb <- escape_regex("Mr. Smith", word_delim = TRUE)
  matches_wb <- regmatches(text, gregexpr(pattern_wb, text, perl = TRUE))
  expect_equal(unlist(matches_wb), "Mr. Smith")
  expect_equal(length(unlist(matches_wb)), 1) # Matches both "Mr." and "AMr."
})

test_that("escape_regex works correctly with URLs", {
  # Test that escaped strings work in regex
  text <- "See more at http://example.com"

  # Without word boundaries (matches both "Mr." and "AMr.")
  pattern <- escape_regex("http://example.com")
  # matches <- regmatches(text, gregexpr(pattern, text))
  matches <- stringr::str_extract_all(text, pattern)
  expect_equal(length(unlist(matches)), 1) # Matches both "Mr." and "AMr."
})

test_that("escape_regex works correctly with -", {
  # Test that escaped strings work in regex
  text <- "The Covid-19 pandemy was"

  # Without word boundaries (matches both "Mr." and "AMr.")
  pattern <- escape_regex("Covid-19")
  matches <- stringr::str_extract_all(text, pattern)
  expect_equal(length(unlist(matches)), 1) # Matches both "Mr." and "AMr."
})

