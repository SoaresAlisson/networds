library(testthat)

cc <- "Bla ble bli." |> cooccur()
cc
expect_equal(nrow(cc), 3)
expect_equal(nrow(filter_graph(cc, "bla")), 2)
expect_equal(nrow(filter_graph(cc, "bla", invert = TRUE)), 1)

test_that("filter_graph works correctly", {
  # Create test data
  test_df <- data.frame(
    col1 = c("apple", "banana", "Apple", "cherry", "BANANA"),
    col2 = c("fruit", "yellow", "red", "berry", "fruit"),
    weight = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  # Test 1: Basic filtering (case-sensitive)
  result1 <- filter_graph(test_df, "apple", ic = FALSE)
  expect_equal(nrow(result1), 1)
  expect_equal(result1$col1, "apple")

  # Test 2: Case-insensitive filtering
  result2 <- filter_graph(test_df, "apple", ic = TRUE)
  expect_equal(nrow(result2), 2)
  expect_true(all(c("apple", "Apple") %in% result2$col1))

  # Test 3: Invert filtering
  result3 <- filter_graph(test_df, "apple", ic = TRUE, invert = TRUE)
  expect_equal(nrow(result3), 3)
  expect_false(any(c("apple", "Apple") %in% result3$col1))

  # Test 4: Query matches second column
  result4 <- filter_graph(test_df, "berry", ic = TRUE)
  expect_equal(nrow(result4), 1)
  expect_equal(result4$col1, "cherry")

  # Test 5: Query matches both columns
  result5 <- filter_graph(test_df, "fruit", ic = TRUE)
  expect_equal(nrow(result5), 2)
  expect_true(all(c("apple", "BANANA") %in% result5$col1))

  # Test 6: No matches
  result6 <- filter_graph(test_df, "xyz", ic = TRUE)
  expect_equal(nrow(result6), 0)

  # Test 7: All matches with invert TRUE
  result7 <- filter_graph(test_df, "xyz", ic = TRUE, invert = TRUE)
  expect_equal(nrow(result7), nrow(test_df))

  # Test 8: Empty data frame
  empty_df <- data.frame(col1 = character(), col2 = character())
  result8 <- filter_graph(empty_df, "test")
  expect_equal(nrow(result8), 0)

  # Test 9: Special regex characters in query
  result9 <- filter_graph(test_df, "a.p", ic = FALSE) # Should match "apple"
  expect_equal(nrow(result9), 1)
  expect_equal(result9$col1, "apple")

  # Test 10: Preserves all columns
  result10 <- filter_graph(test_df, "apple", ic = TRUE)
  expect_equal(ncol(result10), ncol(test_df))
  expect_true("weight" %in% names(result10))
})

test_that("filter_graph works with cooccur example", {
  # Test with the provided example
  cc <- "Bla ble bli. Ble bli." |> cooccur()

  result1 <- filter_graph(cc, "bla")
  expect_true(any(
    grepl("bla", result1[[1]], ignore.case = TRUE) |
      grepl("bla", result1[[2]], ignore.case = TRUE)
  ))

  result2 <- filter_graph(cc, "bla", invert = TRUE)
  expect_false(any(
    grepl("bla", result2[[1]], ignore.case = TRUE) |
      grepl("bla", result2[[2]], ignore.case = TRUE)
  ))
})
