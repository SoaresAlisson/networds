library(testthat)

df <- data.frame(
  n1 = c("foo", "apple", "cat"),
  n2 = c("bar", "zebra", "bird")
)
# Test 1: Basic alphabetical ordering
test_that("cooccur function works correctly", {
  # test_basic_ordering <- function() {
  result <- sort_columns_alphabetically(df)

  expected <- data.frame(
    n1 = c("bar", "apple", "bird"),
    n2 = c("foo", "zebra", "cat")
  )

  # stopifnot(all.equal(result, expected))
  # print("✓ Test 1 passed: Basic alphabetical ordering")
  expect_equal(result, expected)
})


test_that("handles identical words correctly", {
  df <- data.frame(
    n1 = c("same", "word", "test"),
    n2 = c("same", "word", "test")
  )

  result <- sort_columns_alphabetically(df)

  expect_equal(result, df) # Should remain unchanged
})

test_that("handles already alphabetically ordered pairs", {
  df <- data.frame(n1 = c("ant", "cat", "dog"), n2 = c("bee", "mouse", "zebra"))

  result <- sort_columns_alphabetically(df)

  expect_equal(result, df) # Should remain unchanged
})

test_that("respects case sensitivity (ASCII ordering)", {
  df <- data.frame(
    n1 = c("Apple", "ZEBRA", "banana"),
    n2 = c("banana", "ant", "Apple")
  )

  result <- sort_columns_alphabetically(df)

  # In ASCII, uppercase comes before lowercase
  expected <- data.frame(
    n1 = c("Apple", "ZEBRA", "Apple"),
    n2 = c("banana", "ant", "banana")
  )

  expect_equal(result, expected)
})

test_that("handles words with numbers and special characters", {
  df <- data.frame(
    n1 = c("123", "apple-pie", "cafe"),
    n2 = c("456", "apple", "café")
  )

  result <- sort_columns_alphabetically(df)

  expected <- data.frame(
    n1 = c("123", "apple", "cafe"),
    n2 = c("456", "apple-pie", "café")
  )

  expect_equal(result, expected)
})

test_that("works with single row dataframes", {
  df <- data.frame(n1 = "zoo", n2 = "animal")

  result <- sort_columns_alphabetically(df)

  expected <- data.frame(n1 = "animal", n2 = "zoo")

  expect_equal(result, expected)
})

test_that("handles empty strings correctly", {
  df <- data.frame(n1 = c("", "hello", "world"), n2 = c("world", "", "hello"))

  result <- sort_columns_alphabetically(df)

  expected <- data.frame(
    n1 = c("", "", "hello"),
    n2 = c("world", "hello", "world")
  )

  expect_equal(result, expected)
})

test_that("preserves row order and other columns if present", {
  df <- data.frame(
    id = 1:3,
    n1 = c("dog", "apple", "zebra"),
    n2 = c("cat", "banana", "aardvark"),
    value = c(10, 20, 30)
  )

  result <- sort_columns_alphabetically(df)

  expected <- data.frame(
    id = 1:3,
    n1 = c("cat", "apple", "aardvark"),
    n2 = c("dog", "banana", "zebra"),
    value = c(10, 20, 30)
  )

  expect_equal(result, expected)
})

test_that("maintains column types", {
  df <- data.frame(
    n1 = c("text", "words"),
    n2 = c("strings", "here"),
    stringsAsFactors = FALSE
  )

  result <- sort_columns_alphabetically(df)

  expect_type(result$n1, "character")
  expect_type(result$n2, "character")
})

test_that("handles large number of rows efficiently", {
  set.seed(123)
  words <- c(
    "apple",
    "banana",
    "cat",
    "dog",
    "elephant",
    "fox",
    "goat",
    "horse"
  )

  n1 <- sample(words, 1000, replace = TRUE)
  n2 <- sample(words, 1000, replace = TRUE)

  df <- data.frame(n1 = n1, n2 = n2)

  # Should complete without error
  expect_error(result <- sort_columns_alphabetically(df), NA)

  # Spot check first few rows
  for (i in 1:10) {
    sorted_pair <- sort(c(df$n1[i], df$n2[i]))
    expect_equal(result$n1[i], sorted_pair[1])
    expect_equal(result$n2[i], sorted_pair[2])
  }
})

test_that("handles words with leading/trailing spaces", {
  df <- data.frame(
    n1 = c("  hello", "world  ", " test "),
    n2 = c("world", "hello", "space")
  )

  result <- sort_columns_alphabetically(df)

  # Spaces affect alphabetical ordering
  expect_equal(result$n1[1], "  hello") # Space comes before letters
  expect_equal(result$n2[1], "world")
})

test_that("input validation - requires n1 and n2 columns", {
  df_missing <- data.frame(x = c("a", "b"), y = c("c", "d"))

  expect_error(sort_columns_alphabetically(df_missing))
})


test_that("dataframe with zero rows works", {
  df <- data.frame(n1 = character(0), n2 = character(0))

  result <- sort_columns_alphabetically(df)

  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("n1", "n2"))
})
