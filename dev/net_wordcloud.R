head_n <- 30
color <- "lightblue"
edge_norm <- TRUE

graph <- txt_wiki |> # text available in the package
  # because it is a vector, let's collapse it into a single element:
  paste(collapse = " ") |>
  extract_graph(sw = my_sw) |>
  networds::count_graphs() # counting the graphs

df <- graph
graph <- g_N |> head(head_n)
net_wordcloud(df = graph, txt_wiki) # plotting

head_n <- 30
head_n <- ""
if (head_n == "") {
  graph <- df
} else {
  graph <- df |> head(head_n)
}
