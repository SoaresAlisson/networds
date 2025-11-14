library(testthat)
library(dplyr)

DF <- data.frame(a = 1:20, b = letters[1:20])

test_that("head works", {
  expect_equal(to_head_or_not_to_head(DF, 10) |> nrow(), 10)
  expect_equal(to_head_or_not_to_head(DF, "") |> nrow(), nrow(DF))
})
