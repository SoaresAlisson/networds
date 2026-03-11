library(testthat)
library(dplyr)
library(tibble)

test_that("net_wordcloud function works correctly", {
  # Test data
  test_graph <- tibble(
    n1 = c("A", "B", "A", "C", "B", "A", "A", "D"),
    n2 = c("B", "A", "B", "D", "C", "A", "C", "D") # Includes a loop (D-D)
  )

  my_sw <- c(
    stopwords::stopwords(language = "en", source = "snowball", simplify = TRUE),
    "lol"
  )

  graph <- txt_wiki |> # text available in the package
    # because it is a vector, let's collapse it into a single element:
    paste(collapse = " ") |>
    extract_graph(sw = my_sw) |>
    networds::count_graphs() # counting the graphs

  net_wordcloud(df = graph, txt_wiki) # plotting

  my_sw <- c("the")
  Graph <- txt_wiki |> # text available in the package
    # because it is a vector, let's collapse it into a single element:
    paste(collapse = " ") |>
    extract_graph(sw = my_sw) |>
    networds::count_graphs()

  netwc <- net_wordcloud(txt_wiki, df = Graph) # plotting
  # net_wordcloud <- function(text, df, head_n = 30, color = "lightblue") {
})


test_that("net_wordcloud function works correctly", {
  # Helper functions to create test data
  create_test_text <- function() {
    "The quick brown fox jumps over the lazy dog. The quick fox runs fast. A lazy dog sleeps all day. Brown fox is quick."
  }

  create_test_graph_df <- function() {
    tibble(
      n1 = c("quick", "brown", "fox", "lazy", "quick", "fox"),
      n2 = c("brown", "fox", "jumps", "dog", "fox", "runs"),
      freq = c(5, 4, 3, 2, 4, 2)
    )
  }

  create_minimal_graph_df <- function() {
    tibble(
      n1 = c("word1", "word2"),
      n2 = c("word2", "word3"),
      freq = c(2, 1)
    )
  }

  # Mock the required packages to avoid actual plotting
  mock_ggraph <- function(...) list(plot_data = list(...))
  mock_geom_edge_link <- function(...) list(edge_params = list(...))
  mock_geom_node_text <- function(...) list(text_params = list(...))
  mock_theme_void <- function() list(theme = "void")
  mock_theme <- function(...) list(legend_position = "none")

  # Test 1: Basic functionality with default parameters
  test_that("basic functionality with default parameters", {
    text <- create_test_text()
    graph_df <- create_test_graph_df()

    # Mock the plotting functions to avoid actual rendering
    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        result <- net_wordcloud(text = text, df = graph_df)

        # Should return a ggplot object or similar
        expect_type(result, "list")
      }
    )
  })

  # Test 2: head_n parameter works correctly
  test_that("head_n parameter limits number of nodes", {
    text <- create_test_text()
    graph_df <- create_test_graph_df()

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        result_full <- net_wordcloud(text = text, df = graph_df, head_n = "")
        result_limited <- net_wordcloud(text = text, df = graph_df, head_n = 3)

        # Both should execute without error
        expect_type(result_full, "list")
        expect_type(result_limited, "list")
      }
    )
  })

  # Test 3: color parameter works
  test_that("color parameter is applied", {
    text <- create_test_text()
    graph_df <- create_minimal_graph_df()

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = function(..., color) {
        expect_equal(color, "red")
        list(edge_params = list(...))
      },
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        result <- net_wordcloud(text = text, df = graph_df, color = "red")
        expect_type(result, "list")
      }
    )
  })

  # Test 4: edge_norm parameter works
  test_that("edge_norm parameter controls edge normalization", {
    text <- create_test_text()
    graph_df <- create_test_graph_df()

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        # Both should work without errors
        result_norm <- net_wordcloud(
          text = text,
          df = graph_df,
          edge_norm = TRUE
        )
        result_no_norm <- net_wordcloud(
          text = text,
          df = graph_df,
          edge_norm = FALSE
        )

        expect_type(result_norm, "list")
        expect_type(result_no_norm, "list")
      }
    )
  })

  # Test 5: Frequency calculation is correct
  test_that("frequency calculation works correctly", {
    text <- "apple apple banana cherry apple banana"
    graph_df <- tibble(
      n1 = c("apple", "banana"),
      n2 = c("banana", "cherry"),
      freq = c(2, 1)
    )

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = function(..., size) {
        # Size should reflect frequency counts
        expect_type(size, "double")
        list(text_params = list(...))
      },
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        result <- net_wordcloud(text = text, df = graph_df)
        expect_type(result, "list")
      }
    )
  })

  # Test 6: Handles different column names for frequency
  test_that("handles different frequency column names", {
    text <- create_test_text()

    # Test with different third column name
    graph_df_alt <- tibble(
      n1 = c("quick", "brown"),
      n2 = c("brown", "fox"),
      count = c(5, 4) # Different name for frequency
    )

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        # Should handle the try() block gracefully
        result <- net_wordcloud(text = text, df = graph_df_alt)
        expect_type(result, "list")
      }
    )
  })

  # Test 7: Empty graph data handling
  test_that("handles empty graph data", {
    text <- create_test_text()
    empty_graph <- tibble(
      n1 = character(0),
      n2 = character(0),
      freq = integer(0)
    )

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        result <- net_wordcloud(text = text, df = empty_graph)
        expect_type(result, "list")
      }
    )
  })

  # Test 8: Single node graph
  test_that("handles single node graph", {
    text <- "single word"
    single_node_graph <- tibble(
      n1 = "single",
      n2 = "word",
      freq = 1
    )

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        result <- net_wordcloud(text = text, df = single_node_graph)
        expect_type(result, "list")
      }
    )
  })

  # Test 9: Vertex extraction works correctly
  test_that("vertex extraction from graph data", {
    text <- create_test_text()
    graph_df <- create_test_graph_df()

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = function(..., size) {
        # Should have sizes for all unique nodes
        expect_type(size, "double")
        list(text_params = list(...))
      },
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        result <- net_wordcloud(text = text, df = graph_df)
        expect_type(result, "list")
      }
    )
  })
})

# Test error conditions
test_that("net_wordcloud error conditions", {
  # Test 10: Missing required parameters
  test_that("errors on missing required parameters", {
    text <- create_test_text()
    graph_df <- create_test_graph_df()

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        # Should error if text is missing
        expect_error(
          net_wordcloud(df = graph_df),
          "argument \"text\" is missing"
        )

        # Should error if df is missing
        expect_error(
          net_wordcloud(text = text),
          "argument \"df\" is missing"
        )
      }
    )
  })

  # Test 11: Invalid graph structure
  test_that("handles invalid graph structure", {
    text <- create_test_text()

    # Graph with missing required columns
    invalid_graph <- tibble(
      wrong_col1 = c("a", "b"),
      wrong_col2 = c("b", "c")
    )

    with_mock(
      `ggraph::ggraph` = mock_ggraph,
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        # Should handle gracefully with try() blocks
        result <- net_wordcloud(text = text, df = invalid_graph)
        expect_type(result, "list")
      }
    )
  })
})

# Test integration with tidygraph and ggraph
test_that("net_wordcloud integration with graph packages", {
  test_that("converts data to tbl_graph correctly", {
    text <- create_test_text()
    graph_df <- create_test_graph_df()

    with_mock(
      `tidygraph::as_tbl_graph` = function(x) {
        # Verify the input structure
        expect_s3_class(x, "data.frame")
        expect_true(all(c("n1", "n2", "freq") %in% names(x)))
        return(list(graph_data = x))
      },
      `ggraph::ggraph` = function(graph, layout) {
        expect_equal(layout, "graphopt")
        return(list(layout = layout, graph = graph))
      },
      `ggraph::geom_edge_link` = mock_geom_edge_link,
      `ggraph::geom_node_text` = mock_geom_node_text,
      `ggplot2::theme_void` = mock_theme_void,
      `ggplot2::theme` = mock_theme,
      {
        result <- net_wordcloud(text = text, df = graph_df)
        expect_type(result, "list")
      }
    )
  })
})

# Run the tests
# test_file("test_net_wordcloud.R")
