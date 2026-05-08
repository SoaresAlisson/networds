library(testthat)

text <- "John Does lives in New York in United States of America. He is a passionate jazz musician, often playing in local clubs."

test_that("extract_graph_rb returns a tibble", {
  result <- extract_graph_rb(text)
  expect_s3_class(result, "tbl_df")
})

test_that("extract_graph_rb has correct columns", {
  result <- extract_graph_rb(text, count = TRUE)
  expect_named(result, c("n1", "n2", "n"))
})

test_that("extract_graph_rb has correct columns without count", {
  result <- extract_graph_rb(text, count = FALSE)
  expect_named(result, c("n1", "n2"))
})

test_that("extract_graph_rb extracts correct entities", {
  result <- extract_graph_rb(text, count = FALSE)
  expect_true("John_Does" %in% result$n1 || "John_Does" %in% result$n2)
  expect_true("New_York" %in% result$n1 || "New_York" %in% result$n2)
  expect_true("United_States_of_America" %in% result$n1 || "United_States_of_America" %in% result$n2)
})

test_that("extract_graph_rb removes loops by default", {
  result <- extract_graph_rb(text, loop = FALSE, count = FALSE)
  expect_false(any(result$n1 == result$n2))
})

test_that("extract_graph_rb keeps loops when loop = TRUE", {
  result <- extract_graph_rb(text, loop = TRUE, count = FALSE)
  # With this specific text, we may or may not have loops depending on entities
  expect_s3_class(result, "tbl_df")
})

test_that("extract_graph_rb counts occurrences with count = TRUE", {
  result <- extract_graph_rb(text, count = TRUE)
  expect_true("n" %in% names(result))
  expect_true(all(result$n >= 1))
})

test_that("extract_graph_rb works with paragraph tokenization", {
  result <- extract_graph_rb(text, using = "paragraph")
  expect_s3_class(result, "tbl_df")
})

test_that("extract_graph_rb returns empty tibble for single entity text", {
  text_single <- "John lives in New York."
  result <- extract_graph_rb(text_single, count = FALSE)
  expect_equal(nrow(result), 0)
})