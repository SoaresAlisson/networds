library(testthat)
library(dplyr)

cooc_data <- data.frame(
  doc = c(1, 1, 2, 2),
  n1 = c(1, 1, 1, 3),
  n2 = c(3, 4, 3, 5),
  n = c(5, 6, 7, 2)
)


testthat::test_that("reduce_freq reduce nrow", {
  result <- reduce_freq(cooc_data, threshold = 1)
  testthat::expect_true(nrow(result) < nrow(cooc_data))
})
testthat::test_that("reduce_freq works", {
  result <- reduce_freq(cooc_data, threshold = 1)
  testthat::expect_equal(nrow(result), 3)
})
testthat::test_that("reduce_freq works", {
  result <- reduce_freq(cooc_data, threshold = 1)
  testthat::expect_equal(result$n, c(2, 1, 1))
})
