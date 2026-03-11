library(testthat)
library(dplyr)
library(tibble)

test_that("count_graphs function works correctly", {
  # Test data
  test_graph <- tibble(
    n1 = c("A", "B", "A", "C", "B", "A", "A", "D"),
    n2 = c("B", "A", "B", "D", "C", "A", "C", "D") # Includes a loop (D-D)
  )

  # Test 1: Default behavior (loop = FALSE) should exclude loops
  result_no_loop <- count_graphs(test_graph)

  expect_s3_class(result_no_loop, "tbl")
  expect_named(result_no_loop, c("n1", "n2", "n"))
  expect_false(any(result_no_loop$n1 == result_no_loop$n2)) # No loops should be present

  # Verify specific counts
  expect_equal(result_no_loop$n[result_no_loop$n1 == "A" & result_no_loop$n2 == "B"], 2)
  expect_equal(result_no_loop$n[result_no_loop$n1 == "B" & result_no_loop$n2 == "A"], 1)
  expect_equal(result_no_loop$n[result_no_loop$n1 == "A" & result_no_loop$n2 == "C"], 1)

  # Test 2: loop = TRUE should include loops
  result_with_loop <- count_graphs(test_graph, loop = TRUE)

  expect_s3_class(result_with_loop, "tbl")
  expect_named(result_with_loop, c("n1", "n2", "n"))
  expect_true(any(result_with_loop$n1 == result_with_loop$n2)) # Loops should be present

  # Verify loop is included
  expect_equal(result_with_loop$n[result_with_loop$n1 == "D" & result_with_loop$n2 == "D"], 1)

  # Test 3: Empty input
  empty_graph <- tibble(n1 = character(), n2 = character())
  result_empty <- count_graphs(empty_graph)

  expect_s3_class(result_empty, "tbl")
  expect_equal(nrow(result_empty), 0)
  expect_named(result_empty, c("n1", "n2", "n"))

  # Test 4: All loops input (should return empty when loop = FALSE)
  all_loops_graph <- tibble(
    n1 = c("A", "B", "C"),
    n2 = c("A", "B", "C")
  )

  result_all_loops_no <- count_graphs(all_loops_graph)
  expect_equal(nrow(result_all_loops_no), 0)

  result_all_loops_yes <- count_graphs(all_loops_graph, loop = TRUE)
  expect_equal(nrow(result_all_loops_yes), 3)

  # Test 5: Verify sorting works
  result_sorted <- count_graphs(test_graph)
  expect_true(all(diff(result_sorted$n) <= 0)) # Should be sorted in descending order

  # Test 6: Single edge case
  single_edge <- tibble(n1 = "X", n2 = "Y")
  result_single <- count_graphs(single_edge)

  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$n, 1)
  expect_equal(result_single$n1, "X")
  expect_equal(result_single$n2, "Y")
})
