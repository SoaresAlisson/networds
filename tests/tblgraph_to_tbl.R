# test-tblgraph_to_tbl.R
# Unit tests for tblgraph_to_tbl function

library(testthat)
library(tidygraph)
library(dplyr)
library(tibble)


Gcooc <- "Lorem ipsum dolor sit amet. Bla ble bli. Bla bli blo." |>
  cooccur()
Gtbl_g <- Gcooc |> tidygraph::as_tbl_graph()
tbl_restored <- Gtbl_g |> tblgraph_to_tbl()
result <- tbl_restored
# Test Suite
test_that("tblgraph_to_tbl function", {
  # Basic test graph
  # test_graph <- create_test_graph()

  test_that("returns correct output structure", {
    result <- tblgraph_to_tbl(G)

    # Should return a tibble
    expect_s3_class(result, "tbl_df")

    # Should have correct columns
    expect_named(result, c("n1", "n2", "n"))

    # Should have correct number of rows
    expect_equal(nrow(tbl_restored), 15)
    expect_equal(nrow(Gcooc), nrow(tbl_restored))

    # Column types
    expect_type(tbl_restored$n1, "character")
    expect_type(tbl_restored$n2, "character")
    expect_type(tbl_restored$n, "integer")
  })

  test_that("correctly maps node names", {
    # result <- tblgraph_to_tbl(test_graph)

    # Check specific name mappings
    expect_equal(result$n1[1], "bla")
    expect_equal(result$n2[1], "bli")

    # Edge 2: from 2 ("B") to 3 ("C")
    expect_equal(result$n1[2], "bla")
    expect_equal(result$n2[2], "ble")
  })

  })

  test_that("handles empty graph", {
    empty_graph <- tbl_graph(
      nodes = tibble(name = character()),
      edges = tibble(from = integer(), to = integer(), n = numeric())
    )

    result <- tblgraph_to_tbl(empty_graph)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
    expect_named(result, c("n1", "n2", "n"))
  })

  test_that("handles graph with nodes but no edges", {
    no_edge_graph <- tbl_graph(
      nodes = tibble(name = c("X", "Y", "Z")),
      edges = tibble(from = integer(), to = integer(), n = numeric())
    )

    result <- tblgraph_to_tbl(no_edge_graph)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
    expect_named(result, c("n1", "n2", "n"))
  })

  test_that("handles single edge graph", {
    single_graph <- tbl_graph(
      nodes = tibble(name = c("Start", "End")),
      edges = tibble(from = 1, to = 2, n = 42)
    )

    result <- tblgraph_to_tbl(single_graph)

    expect_equal(nrow(result), 1)
    expect_equal(result$n1, "Start")
    expect_equal(result$n2, "End")
    expect_equal(result$n, 42)
  })

  test_that("handles self-loops", {
    loop_graph <- tbl_graph(
      nodes = tibble(name = c("Node1", "Node2")),
      edges = tibble(
        from = c(1, 2, 1),
        to = c(1, 2, 2),
        n = c(5, 10, 15)
      )
    )

    result <- tblgraph_to_tbl(loop_graph)

    expect_equal(nrow(result), 3)
    expect_equal(result$n1[1], "Node1")
    expect_equal(result$n2[1], "Node1") # Self-loop
    expect_equal(result$n[1], 5)
  })


  test_that("maintains row order", {
    # Create graph with specific edge order
    ordered_graph <- tbl_graph(
      nodes = tibble(name = LETTERS[1:5]),
      edges = tibble(
        from = c(5, 4, 3, 2, 1),
        to = c(4, 3, 2, 1, 5),
        n = 1:5
      )
    )

    result <- tblgraph_to_tbl(ordered_graph)

    # Should maintain the original edge order
    expect_equal(result$n, 1:5)
    expect_equal(result$n1, c("E", "D", "C", "B", "A"))
    expect_equal(result$n2, c("D", "C", "B", "A", "E"))
  })


test_that("tblgraph_to_tbl error handling", {
  test_that("errors on non-tbl_graph input", {
    expect_error(tblgraph_to_tbl("not a graph"), "tbl_graph")
    expect_error(tblgraph_to_tbl(123), "tbl_graph")
    expect_error(tblgraph_to_tbl(data.frame(x = 1:3)), "tbl_graph")
    expect_error(tblgraph_to_tbl(NULL), "tbl_graph")
  })

  test_that("errors when nodes lack name column", {
    graph_no_names <- tbl_graph(
      nodes = tibble(id = 1:3, value = c(1, 2, 3)), # No 'name' column
      edges = tibble(from = 1, to = 2, n = 5)
    )

    expect_error(
      tblgraph_to_tbl(graph_no_names),
      "name"
    )
  })

  test_that("errors when edges lack n column", {
    graph_no_n <- tbl_graph(
      nodes = tibble(name = c("A", "B")),
      edges = tibble(from = 1, to = 2) # No 'n' column
    )

    expect_error(
      tblgraph_to_tbl(graph_no_n),
      "n"
    )
  })

})

test_that("tblgraph_to_tbl with example from documentation", {
  test_that("works with cooccur example structure", {
    # Create a mock cooccur output similar to example
    mock_cooccur_graph <- tbl_graph(
      nodes = tibble(name = c("Lorem", "ipsum", "dolor", "sit", "amet")),
      edges = tibble(
        from = c(1, 1, 2, 3),
        to = c(2, 3, 3, 4),
        n = c(2, 1, 1, 1)
      )
    )

    result <- tblgraph_to_tbl(mock_cooccur_graph)

    expect_s3_class(result, "tbl_df")
    expect_named(result, c("n1", "n2", "n"))
    expect_equal(nrow(result), 4)

    # Check specific pairs
    expect_true("Lorem" %in% result$n1)
    expect_true("ipsum" %in% result$n2)
  })

  test_that("works with additional edge attributes", {
    # Graph with extra edge attributes should still work
    # (they will be dropped by select(n1, n2, n))
    graph_with_extras <- tbl_graph(
      nodes = tibble(name = c("A", "B", "C")),
      edges = tibble(
        from = c(1, 2),
        to = c(2, 3),
        n = c(10, 20),
        weight = c(0.5, 0.8),
        type = c("strong", "weak")
      )
    )

    result <- tblgraph_to_tbl(graph_with_extras)

    # Should only have n1, n2, n
    expect_equal(ncol(result), 3)
    expect_named(result, c("n1", "n2", "n"))
    expect_equal(result$n, c(10, 20))
  })
})

test_that("tblgraph_to_tbl performance and edge cases", {
  test_that("handles large graphs", {
    # Create a larger graph (100 nodes, 500 edges)
    set.seed(123)
    n_nodes <- 100
    n_edges <- 500

    nodes <- tibble(
      name = paste0("Node_", 1:n_nodes),
      group = sample(1:5, n_nodes, replace = TRUE)
    )

    edges <- tibble(
      from = sample(1:n_nodes, n_edges, replace = TRUE),
      to = sample(1:n_nodes, n_edges, replace = TRUE),
      n = sample(1:100, n_edges, replace = TRUE)
    )

    large_graph <- tbl_graph(nodes = nodes, edges = edges)

    # Test that it runs without error
    expect_silent(result <- tblgraph_to_tbl(large_graph))

    # Check structure
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), n_edges)
    expect_named(result, c("n1", "n2", "n"))

    # All node names should be from the nodes$name
    all_node_names <- unique(c(result$n1, result$n2))
    expect_true(all(all_node_names %in% nodes$name))
  })

  test_that("handles duplicate node names", {
    # Note: In tidygraph, node names should be unique, but let's test edge case
    graph_duplicate_names <- tbl_graph(
      nodes = tibble(name = c("A", "A", "B")), # Duplicate "A"
      edges = tibble(
        from = c(1, 2, 1),
        to = c(2, 3, 3),
        n = c(1, 2, 3)
      )
    )

    result <- tblgraph_to_tbl(graph_duplicate_names)

    # Should still run, though names are ambiguous
    expect_s3_class(result, "tbl_df")
    expect_equal(result$n1[1], "A") # First "A"
    expect_equal(result$n1[2], "A") # Second "A"
  })

test_that("documentation examples work", {
  # Skip if cooccur is not available (it's in the example but not in tests)
  skip_if_not(exists("cooccur"))

  # This would test the exact example from documentation
  G <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit." |> cooccur()
  G <- G |> tidygraph::as_tbl_graph()
  result <- G |> tblgraph_to_tbl()
  expect_s3_class(result, "tbl_df")
})

})

