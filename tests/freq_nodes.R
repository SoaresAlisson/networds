# tests/testthat/test-freq_nodes.R

library(testthat)
library(dplyr)

test_that("freq_nodes returns correct frequency for simple words", {
  vert <- c("dog", "cat")
  # vert <- c("dog", "cat", "bird")
  text <- c("The dog and the cat are friends. The dog likes the cat.")

  result <- freq_nodes(vert, text)

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), c("x", "freq"))
  expect_equal(result$x, c("cat", "dog"))
  expect_equal(result$freq, c(2, 2))
})

test_that("freq_nodes handles case insensitivity", {
  vert <- c("Dog", "CAT")
  text <- c("The dog and the cat are friends. The DOG likes the CaT.")

  result <- freq_nodes(vert, text)

  expect_equal(result$x, c("cat", "dog"))
  expect_equal(result$freq, c(2, 2))
})

test_that("words with special character usually used in scape: .", {
  vert <- c("t.me", "youtu.be")
  text <- c("Follow our channel in t.me and youtu.be.")

  result <- freq_nodes(vert, text)

  expect_equal(result$x, vert)
  expect_equal(result$freq, c(1, 1))
})

test_that("freq_nodes handles words with dots", {
  text <- "Mr. Smith and Mrs. Jones went to see Dr. Brown. Mr. Smith is a patient. AMr."
  vert <- c("Mr. Smith", "Mrs. Jones", "Dr. Brown")
  
  result <- freq_nodes(vert, text)

  expect_equal(sort(result$x), tolower(sort(vert)))
  expect_equal(result$freq, c(2, 1, 1))
})


test_that("words with special character : _", {
  vert <- "New_Delhi"
  text <- "The central axis of New_Delhi, which today faces east at India Gate"

  result <- freq_nodes(vert, text)

  expect_equal(result$x, tolower(vert))
  expect_equal(result$freq, 1)
})

test_that("words with special character : -", {
  vert <- "COVID-19"
  text <- "The central axis of COVID-19, lorem ipsum"

  result <- freq_nodes(vert, text)

  expect_equal(result$x, tolower(vert))
  expect_equal(result$freq, 1)
})


test_that("freq_nodes handles word boundaries correctly", {
  vert <- c("cat", "caterpillar")
  text <- c("The cat ate a caterpillar. Amazing cat!")

  result <- freq_nodes(vert, text)

  expect_equal(result$x, c("cat", "caterpillar"))
  expect_equal(result$freq, c(2, 1))
})

test_that("freq_nodes handles phrases with underscores", {
  vert <- c("New_York", "Los_Angeles")
  text <- c("I live in New_York and visited Los_Angeles. New York is great!")

  result <- freq_nodes(vert, text)

  expect_equal(result$x, sort(tolower(vert)))
  expect_equal(result$freq, c(1, 1))
})

test_that("freq_nodes handles mixed simple words and phrases with underscores", {
  vert <- c("New_York", "city", "San_Francisco", "beach")
  text <- "New_York is a big city. I love San_Francisco and the beach. The city is busy."
  
  result <- freq_nodes(vert, text)

  expect_equal(sort(result$x), sort(tolower(vert)))
  expect_equal(result$freq, c(2, 1, 1, 1))
})


test_that("freq_nodes handles text with no matches", {
  vert <- c("dog", "cat")
  text <- c("The bird flew away.")

  expect_error( freq_nodes(vert, text))
})

test_that("freq_nodes stops when nodes are not found in text", {
  vert <- c("dog", "cat", "elephant")
  text <- c("The dog and the cat are friends.")

  expect_error(
    freq_nodes(vert, text),
  )
})

test_that("freq_nodes stops with multiple missing nodes", {
  vert <- c("dog", "cat", "elephant", "lion", "tiger")
  text <- c("The dog and the cat are friends.")

  expect_error(
    freq_nodes(vert, text),
  )
})

test_that("freq_nodes handles special characters in nodes", {
  vert <- c("café", "naïve", "façade")
  text <- c("The café has a naïve façade. The café is nice.")

  result <- freq_nodes(vert, text)

  expect_equal(result$x, c("café", "façade", "naïve"))
  expect_equal(result$freq, c(2, 1, 1))
})

test_that("freq_nodes handles punctuation in text", {
  vert <- c("hello", "world")
  text <- c("Hello! How are you? Hello, world...")

  result <- freq_nodes(vert, text)

  expect_equal(result$x, c("hello", "world"))
  expect_equal(result$freq, c(2, 1))
})

test_that("freq_nodes returns correct order (alphabetical)", {
  vert <- c("zebra", "apple", "banana", "cat")
  text <- c("apple banana cat zebra apple")

  result <- freq_nodes(vert, text)

  expect_equal(result$x, c("apple", "banana", "cat", "zebra"))
  expect_equal(result$freq, c(2, 1, 1, 1))
})
