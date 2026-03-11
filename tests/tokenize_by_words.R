library(testthat)
# library(stringr)
# library(tibble)

# tests/testthat/test-tokenize_by_words.R

test_that("tokenize_by_words handles basic text tokenization", {
  # Basic sentence tokenization
  txt <- "This is a simple sentence"
  result <- tokenize_by_words(txt, unlist = TRUE)
  expect_equal(result, c("this", "is", "a", "simple", "sentence"))

  # Multiple sentences
  txt2 <- c("First sentence here", "Second sentence there")
  result2 <- tokenize_by_words(txt2)
  expect_equal(length(result2), 2)
  expect_equal(result2[[1]], c("first", "sentence", "here"))
  expect_equal(result2[[2]], c("second", "sentence", "there"))

  # Multiple vector elements.
  txt2 <- c(
    "First sentence here. Second sentence there",
    "Second element sentence there"
  )
  result2 <- tokenize_by_words(txt2)
  expect_equal(length(result2), 2)
})

test_that("tokenize_by_words preserves compound words", {
  txt <- "Covid-19 and ice-cream are compound words like New_Jersey"
  result <- tokenize_by_words(txt, unlist = TRUE)

  # Check that compound words remain intact
  expect_true("covid-19" %in% result)
  expect_true("ice-cream" %in% result)
  expect_true("new_jersey" %in% result)

  # Check that regular words are also present
  expect_true("and" %in% result)
  expect_true("are" %in% result)
  expect_true("compound" %in% result)
  expect_true("words" %in% result)
  expect_true("like" %in% result)
})

test_that("tokenize_by_words handles URLs correctly", {
  txt <- "Visit https://quanteda.io/reference/index.html for documentation"
  result <- tokenize_by_words(txt, unlist = TRUE)

  # URL should remain intact
  expect_true("https://quanteda.io/reference/index.html" %in% result)
  expect_true("visit" %in% result)
  expect_true("for" %in% result)
  expect_true("documentation" %in% result)
})

test_that("tokenize_by_words handles punctuation correctly", {
  txt <- "Hello! How are you? I'm fine, thanks; goodbye."
  result <- tokenize_by_words(txt, unlist = TRUE)

  # Punctuation should be removed, words should remain
  expected <- c(
    "hello",
    "how",
    "are",
    "you",
    "i'm",
    "fine",
    "thanks",
    "goodbye"
  )
  expect_equal(result, expected)

  # Test with various punctuation at word boundaries
  txt2 <- "Some,text,like;csv|format"
  result2 <- tokenize_by_words(txt2, unlist = TRUE)
  expect_equal(result2, c("some", "text", "like", "csv", "format"))
})

test_that("tokenize_by_words handles the lower parameter correctly", {
  txt <- "UPPER and lower CASE words"

  # Test with lower = TRUE (default)
  result_lower <- tokenize_by_words(txt, lower = TRUE, unlist = TRUE)
  expect_equal(result_lower, c("upper", "and", "lower", "case", "words"))

  # Test with lower = FALSE
  result_preserve <- tokenize_by_words(txt, lower = FALSE, unlist = TRUE)
  expect_equal(result_preserve, c("UPPER", "and", "lower", "CASE", "words"))
})

test_that("tokenize_by_words handles the unlist parameter correctly", {
  txt <- c("First text", "Second text")

  # Test with unlist = FALSE (default)
  result_list <- tokenize_by_words(txt, unlist = FALSE)
  expect_type(result_list, "list")
  expect_length(result_list, 2)
  expect_equal(result_list[[1]], c("first", "text"))
  expect_equal(result_list[[2]], c("second", "text"))

  # Test with unlist = TRUE
  result_unlisted <- tokenize_by_words(txt, unlist = TRUE)
  expect_type(result_unlisted, "character")
  expect_length(result_unlisted, 4)
  expect_equal(result_unlisted, c("first", "text", "second", "text"))
})

test_that("tokenize_by_words handles empty and edge cases", {
  # Empty string
  expect_equal(tokenize_by_words("", unlist = TRUE), character(0))

  # String with only punctuation
  expect_equal(tokenize_by_words("!!!", unlist = TRUE), character(0))

  # String with only spaces
  expect_equal(tokenize_by_words("   ", unlist = TRUE), character(0))

  # Mixed spaces and punctuation
  expect_equal(tokenize_by_words("  ,;  ", unlist = TRUE), character(0))

  # NULL input (should handle gracefully)
  expect_error(tokenize_by_words(NULL)) # or expect_equal() depending on behavior

  # NA input
  expect_equal(tokenize_by_words(NA_character_, unlist = TRUE), character(0))
})

test_that("tokenize_by_words handles special characters and numbers", {
  txt <- "Email: test@email.com, phone: +123-456-7890"
  result <- tokenize_by_words(txt, lower = TRUE, unlist = TRUE)

  # Email should be preserved
  expect_true("test@email.com" %in% result)

  # Phone number with hyphens should be preserved
  expect_true("+123-456-7890" %in% result)

  # Words should be present
  expect_true("email" %in% result)
  expect_true("phone" %in% result)
})

test_that("tokenize_by_words handles multiple delimiters correctly", {
  txt <- "Word1\nWord2\tWord3\rWord4 Word5,Word6;Word7|Word8"
  result <- tokenize_by_words(txt, unlist = TRUE)

  # All delimiters should split words
  expected <- c(
    "word1",
    "word2",
    "word3",
    "word4",
    "word5",
    "word6",
    "word7",
    "word8"
  )
  expect_equal(result, expected)
})

test_that("tokenize_by_words handles leading/trailing punctuation", {
  txt <- "!!!Hello!!! ...World... ???Test??? ---End---"
  result <- tokenize_by_words(txt, unlist = TRUE)

  # Leading/trailing punctuation should be removed
  expect_equal(result, c("hello", "world", "test", "end"))
})

test_that("tokenize_by_words handles complex real-world examples", {
  txt <- paste(
    "The package https://quanteda.io/reference/index.html is very Nice!",
    "There are also compounded words like New_Jersey.",
    "The Covid-19 pandemic is very bad.",
    "Some,text,like;csv"
  )

  result <- tokenize_by_words(txt, unlist = TRUE)

  # Expected tokens
  expect_true("the" %in% result)
  expect_true("package" %in% result)
  expect_true("https://quanteda.io/reference/index.html" %in% result)
  expect_true("is" %in% result)
  expect_true("very" %in% result)
  expect_true("nice" %in% result)
  expect_true("there" %in% result)
  expect_true("are" %in% result)
  expect_true("also" %in% result)
  expect_true("compounded" %in% result)
  expect_true("words" %in% result)
  expect_true("like" %in% result)
  expect_true("new_jersey" %in% result)
  expect_true("covid-19" %in% result)
  expect_true("pandemic" %in% result)
  expect_true("bad" %in% result)
  expect_true("some" %in% result)
  expect_true("text" %in% result)
  expect_true("csv" %in% result)
})

test_that("tokenize_by_words handles multiple input strings consistently", {
  txt <- c(
    "First text with Covid-19",
    "Second text with https://example.com",
    "Third text with New_Jersey"
  )

  result <- tokenize_by_words(txt, lower = TRUE, unlist = FALSE)

  # Check structure
  expect_length(result, 3)

  # Check first element
  expect_equal(result[[1]], c("first", "text", "with", "covid-19"))

  # Check second element
  expect_equal(result[[2]], c("second", "text", "with", "https://example.com"))

  # Check third element
  expect_equal(result[[3]], c("third", "text", "with", "new_jersey"))
})

test_that("tokenize_by_words handles accented characters", {
  txt <- "Café français déjà vu"
  result <- tokenize_by_words(txt, lower = TRUE, unlist = TRUE)

  # Accented characters should be preserved (just lowercased)
  expect_equal(result, c("café", "français", "déjà", "vu"))

  # Test with lower = FALSE
  result_preserve <- tokenize_by_words(txt, lower = FALSE, unlist = TRUE)
  expect_equal(result_preserve, c("Café", "français", "déjà", "vu"))
})
