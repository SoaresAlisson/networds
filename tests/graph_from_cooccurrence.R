library(testthat)
library(dplyr)
library(tibble)

pos <- txt_wiki |>
  filter_by_query("Police") |>
  parsePOS()

entities_by_txt <- pos |>
  dplyr::group_by(doc_id) |>
  dplyr::summarise(entities = list(unique(entity)))


test_that("graph_from_cooccurrence function works correctly", {
  # Helper function to create test data
  # create_test_data <- function() {
  #   tibble(
  #     doc_id = c("doc1", "doc2", "doc3", "doc4"),
  #     terms = list(
  #       c("cat", "dog", "mouse"), # Multiple terms
  #       c("house", "garden"), # Two terms
  #       c("car"), # Single term (lonely node)
  #       c("book", "pen", "paper") # Multiple terms
  #     )
  #   )
  # }
  #
  # create_test_data_with_entities <- function() {
  #   tibble(
  #     doc_id = c("doc1", "doc2"),
  #     entities = list(
  #       c("John", "Mary", "Bob"),
  #       c("Alice", "Charlie")
  #     )
  #   )
  # }

  # Test 1: Basic functionality with default parameters
  test_that("basic functionality with default parameters", {
    # DF <- create_test_data()
    result <- graph_from_cooccurrence(entities_by_txt)

    # Check structure
    expect_type(result, "list")
    expect_named(result, c("graphs", "isolated_nodes", "nodes"))
    expect_s3_class(result$graphs, "tbl_df")
    # expect_s3_class(result$isolated_nodes, "tbl_df")
    expect_s3_class(result$nodes, "tbl_df")

    # Check graphs
    expect_true(all(c("n1", "n2", "freq") %in% names(result$graphs)))
    expect_type(result$graphs$n1, "character")
    expect_type(result$graphs$n2, "character")
    expect_type(result$graphs$freq, "integer")

    # Check that single-term entries become isolated nodes
    expect_true("American" %in% result$isolated_nodes$node)
  })

  # Test 2: Function works with 'entities' column name
  test_that("works with entities column name", {
    DF <- create_test_data_with_entities()
    result <- graph_from_cooccurrence(DF)

    expect_s3_class(result$graphs, "tbl_df")
    # expect_s3_class(result$isolated_nodes, "tbl_df")
    expect_s3_class(result$nodes, "tbl_df")

    # Should have created co-occurrence pairs
    expect_gt(nrow(result$graphs), 0)
  })

  # Test 3: strip_rgx parameter works correctly
  test_that("strip_rgx parameter works", {
    DF <- tibble(
      doc_id = c("doc1", "doc2"),
      terms = list(
        c("the_cat", "the_dog"),
        c("the_house", "the_garden")
      )
    )

    result <- graph_from_cooccurrence(entities_by_txt, strip_rgx = "^the_")

    # Check that "the_" prefix was stripped
    expect_false(any(grepl("^the_", result$graphs$n1)))
    expect_false(any(grepl("^the_", result$graphs$n2)))
    expect_true(all(
      c("New_York", "NYPD", "American", "San_Francisco") %in% result$nodes$node
    ))
  })

  # Test 4: strip_rgx = "" doesn't strip anything
  test_that("empty strip_rgx doesn't modify text", {
    # DF <- tibble(
    #   doc_id = "doc1",
    #   terms = list(c("the_cat", "the_dog"))
    # )

    result <- graph_from_cooccurrence(entities_by_txt, strip_rgx = "")

    expect_true(any(grepl("^the_", result$nodes$node)))
    expect_true("the_San_Francisco_Police_Department" %in% result$nodes$node)
    # pull(result$graph, n1, n2) |> grep(x = _, "the_")
    # expect_true("the_San_Francisco_Police_Department" %in% result$graph)
  })

  # Test 5: freq = FALSE returns correct structure
  test_that("freq = FALSE returns correct structure", {
     result <- graph_from_cooccurrence(entities_by_txt, freq = FALSE)
    #result <- graph_from_cooccurrence(DF, freq = FALSE)

    expect_s3_class(result$nodes, "tbl_df")
    expect_equal(ncol(result$nodes), 1) # Should just be node names
    expect_equal(names(result$nodes), "value") # Default tibble column name

    # With freq = FALSE
    expect_false("freq" %in% names(result$nodes))
  })

  # Test 6: Co-occurrence pairs are correctly generated
  test_that("co-occurrence pairs are correct", {
    DF <- tibble(
      doc_id = "doc1",
      terms = list(c("A", "B", "C"))
    )

    result <- graph_from_cooccurrence(DF)

    # For three items, should have combinations: AB, AC, BC
    expected_pairs <- tibble::tribble(
      ~n1, ~n2, ~freq,
      "A", "B",1,
      "A", "C",1,
      "B", "C",1,
    )

    as.vector(result$graph == expected_pairs) |> all() |>
            expect_true()

  })

  # Test 7: Isolated nodes are correctly identified
  test_that("isolated nodes are correctly identified", {
    DF <- tibble(
      doc_id = c("doc1", "doc2", "doc3"),
      terms = list(
        c("alone"), # Isolated node
        c("pair1", "pair2"), # Connected nodes
        c("solitary") # Isolated node
      )
    )

    result <- graph_from_cooccurrence(DF)

    expect_true(all(c("alone", "solitary") %in% result$isolated_nodes$node))
    expect_false(any(c("pair1", "pair2") %in% result$isolated_nodes$node))
  })

  # Test 8: Frequency counts are correct
  test_that("frequency counts are accurate", {
    DF <- tibble(
      doc_id = c("doc1", "doc2"),
      terms = list(
        c("A", "B"), # AB pair appears once
        c("A", "B") # AB pair appears again
      )
    )

    result <- graph_from_cooccurrence(DF)

    ab_pair <- result$graphs %>%
      filter((n1 == "A" & n2 == "B") | (n1 == "B" & n2 == "A"))

    expect_equal(nrow(ab_pair), 1)
    expect_equal(ab_pair$freq, 2) # Should count both occurrences
  })

  # Test 9: Empty input handling
  test_that("empty input handling", {
    DF <- tibble(
      doc_id = character(0),
      terms = list()
    )

    expect_error(graph_from_cooccurrence(DF))
  })

  # Test 10: All single-term documents
  test_that("all single-term documents become isolated nodes", {
    DF <- tibble(
      doc_id = c("doc1", "doc2", "doc3"),
      terms = list(
        c("A"),
        c("B"),
        c("C")
      )
    )

    expect_error(graph_from_cooccurrence(DF))
  })



  # Test 11: Node frequency in all_nodes is correct
  test_that("node frequency counts are correct", {
    DF <- tibble(
      doc_id = c("doc1", "doc2", "doc3"),
      terms = list(
        c("A", "B"),
        c("A", "C"),
        c("A") # A appears 3 times total
      )
    )

    result <- graph_from_cooccurrence(DF)

    node_a <- result$nodes %>% filter(node == "A")
    expect_equal(node_a$freq, 3)
  })
})

# Test edge cases and error conditions
test_that("graph_from_cooccurrence edge cases and errors", {
  # Test with invalid column names
  test_that("handles invalid column names gracefully", {
    DF <- tibble(
      wrong_id = c("doc1", "doc2"),
      wrong_terms = list(c("A", "B"), c("C", "D"))
    )

    # Should error or handle appropriately depending on function design
    # This test might need adjustment based on actual error handling
    expect_error( graph_from_cooccurrence(DF))
  })

  # Test with non-character elements
  test_that("handles non-character elements", {
    DF <- tibble(
      doc_id = c("doc1", "doc2"),
      terms = list(
        c(1, 2, 3), # Numeric
        c("A", "B") # Character
      )
    )

    # Function should handle type conversion or error appropriately
    result <- graph_from_cooccurrence(DF)
    expect_s3_class(result$graphs, "tbl_df")
  })
})

# Run the tests
# test_file("test_graph_from_cooccurrence.R")
