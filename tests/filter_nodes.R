library(testthat)

txt <- "Lorem Ipsum. The Ipsum John. Dolorem est Lorem"
DF <- txt |> cooccur()
DF
DF |> filter_nodes("dolor")
DF |> filter_nodes("lorem")
DF |> filter_nodes("Dolor", ignore.case = T)
DF |> filter_nodes("dolor", invert = T)

# test_filter_nodes.R


# Load the function (assuming it's in a package or source file)
# source("filter_nodes.R")  # Uncomment if needed

test_that("filter_nodes works correctly with basic functionality", {
  # Create test data
  test_df <- data.frame(
    node1 = c("apple", "banana", "cherry", "date", "elderberry"),
    node2 = c("fruit", "apple pie", "banana split", "cherry tart", "date roll"),
    count = c(5, 3, 2, 4, 1),
    stringsAsFactors = FALSE
  )

  # Test 1: Basic filtering - find "apple" in either column
  result <- filter_nodes(test_df, "apple")
  expected <- test_df[c(1, 2), ]
  expect_equal(result, expected)

  # Test 2: Filter with no matches
  result <- filter_nodes(test_df, "xyz")
  expected <- test_df[0, ]
  expect_equal(result, expected)

  # Test 3: Filter with partial match
  result <- filter_nodes(test_df, "err")
  expected <- test_df[c(3, 4, 5), ] # cherry, elderberry
  expect_equal(result, expected)
})

test_that("filter_nodes handles invert parameter correctly", {
  test_df <- data.frame(
    colA = c("cat", "dog", "catdog", "bird", "fish"),
    colB = c("mouse", "cat", "dogcat", "birdie", "fisher"),
    value = 1:5,
    stringsAsFactors = FALSE
  )

  # Test 1: Invert filter - exclude rows with "cat"
  result <- filter_nodes(test_df, "cat", invert = TRUE)
  expected <- test_df[4:5, ]
  expect_equal(result, expected)

  # Test 2: Normal filter vs inverted filter should be complementary
  normal <- filter_nodes(test_df, "cat")
  inverted <- filter_nodes(test_df, "cat", invert = TRUE)
  combined <- rbind(normal, inverted)
  combined <- combined[order(combined$value), ]
  expect_equal(combined, test_df)
})

test_that("filter_nodes passes ... parameters to grepl correctly", {
  test_df <- data.frame(
    first = c("Apple", "apple", "APPLE", "banana", "Cherry"),
    second = c("pie", "APPLE PIE", "apple tart", "Banana", "cherry"),
    freq = c(10, 5, 3, 7, 2),
    stringsAsFactors = FALSE
  )

  # Test 1: Case-sensitive search (default)
  result <- filter_nodes(test_df, "apple")
  expected <- test_df[2:3, ] # only lowercase "apple"
  expect_equal(result, expected)

  # Test 2: Case-insensitive search
  result <- filter_nodes(test_df, "apple", ignore.case = TRUE)
  expected <- test_df[1:3, ] # all "apple" variants
  expect_equal(result, expected)

  # Test 3: Use fixed = TRUE for exact matching
  result <- filter_nodes(test_df, "apple", fixed = TRUE)
  expected <- test_df[2:3, ] # only exact "apple"
  expect_equal(result, expected)
})

test_that("filter_nodes works with different column names", {
  # Test with non-standard column names
  test_df <- data.frame(
    source = c("java", "python", "r", "javascript"),
    target = c("programming", "python code", "r stats", "js"),
    weight = c(1.5, 2.3, 3.1, 4.7),
    stringsAsFactors = FALSE
  )

  result <- filter_nodes(test_df, "python")
  expected <- test_df[2, ]
  expect_equal(result, expected)

  # Test with numeric values that can be coerced to character
  test_df2 <- data.frame(
    V1 = c("a1", "b2", "c3"),
    V2 = c(123, 456, 789),
    stringsAsFactors = FALSE
  )

  result <- filter_nodes(test_df2, "123")
  expected <- test_df2[1, ]
  expect_equal(result, expected)
})

test_that("filter_nodes handles edge cases", {
  # Test 1: Empty dataframe
  empty_df <- data.frame(col1 = character(), col2 = character())
  result <- filter_nodes(empty_df, "test")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("col1", "col2"))

  # Test 2: Single row dataframe
  single_df <- data.frame(a = "test", b = "example")
  result <- filter_nodes(single_df, "test")
  expect_equal(result, single_df)

  result <- filter_nodes(single_df, "xyz")
  expect_equal(nrow(result), 0)

  # Test 3: All rows match
  test_df <- data.frame(
    x = c("a", "a", "a"),
    y = c("b", "b", "b")
  )
  result <- filter_nodes(test_df, "a")
  expect_equal(result, test_df)

  # Test 4: Regex patterns
  test_df <- data.frame(
    col1 = c("test123", "abc", "def456"),
    col2 = c("xyz", "789ghi", "jkl")
  )

  # Use regex to find digits
  result <- filter_nodes(test_df, "\\d+")
  expected <- test_df[c(1, 2, 3), ] # all except row 2 in col2 has digits
  rownames(expected) <- NULL
  expect_equal(result, expected)
})

test_that("filter_nodes preserves row names and additional columns", {
  test_df <- data.frame(
    node_from = c("A", "B", "C", "D"),
    node_to = c("X", "Y", "Z", "W"),
    weight = c(1.1, 2.2, 3.3, 4.4),
    category = c("type1", "type2", "type1", "type2"),
    row.names = c("row1", "row2", "row3", "row4"),
    stringsAsFactors = FALSE
  )

  result <- filter_nodes(test_df, "A|X")
  expected <- test_df[1, ]
  expect_equal(result, expected)
  expect_equal(rownames(result), "row1")
  expect_equal(result$category, "type1")
  expect_equal(result$weight, 1.1)
})

test_that("filter_nodes handles special characters in query", {
  test_df <- data.frame(
    col1 = c("test.me", "example", "test-me"),
    col2 = c("ok", "test.me", "good"),
    stringsAsFactors = FALSE
  )

  # Test with dot (regex special character)
  result <- filter_nodes(test_df, "test\\.me")
  expected <- test_df[c(1, 2), ] # matches literal "test.me"
  expect_equal(result, expected)

  # Test with hyphen (not special in regex)
  result <- filter_nodes(test_df, "test-me")
  expected <- test_df[3, ]
  expect_equal(result, expected)
})

test_that("filter_nodes works with factor columns", {
  # Test with factor columns (should be converted to character by unlist)
  test_df <- data.frame(
    factor_col = factor(c("level1", "level2", "level3", "level1")),
    char_col = c("text1", "text2", "text3", "text4"),
    stringsAsFactors = FALSE
  )

  result <- filter_nodes(test_df, "level1")
  expected <- test_df[c(1, 4), ]
  expect_equal(result, expected)
})

# Run all tests
# test_file("test_filter_nodes.R")  # Or test_dir() for directory
