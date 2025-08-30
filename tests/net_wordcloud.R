library(testthat)
library(dplyr)
library(tibble)

test_that("net_wordcloud function works correctly", {
  # Test data
  test_graph <- tibble(
    n1 = c("A", "B", "A", "C", "B", "A", "A", "D"),
    n2 = c("B", "A", "B", "D", "C", "A", "C", "D") # Includes a loop (D-D)
  )

  Graph <- txt_wiki |> # text available in the package
    # because it is a vector, let's collapse it into a single element:
    paste(collapse = " ") |>
    extract_graph(sw = my_sw) |>
    networds::count_graphs()

  netwc <- net_wordcloud(txt_wiki, df = Graph) # plotting
  # net_wordcloud <- function(text, df, head_n = 30, color = "lightblue") {
})
