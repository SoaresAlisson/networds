# plot_graph2
library(testthat)

head_n <- 30
lower <- TRUE
text_color <- "black"
text_size <- 1
node_alpha <- 0.5
node_color <- NULL
node_size <- NULL
edge_color <- "lightblue"
edge_alpha <- 0.5
edge_type <- "line"
edge_type <- "arc"
edge_cut <- 0
edge_fan <- FALSE
scale_graph <- "scale_values"
layout <- "kk"
edge_bend <- 0.2
text_contour_color <- "black"
text_contour_color <- NULL

text <- text_sample
DF <- text |> cooccur_words(sw = c("the", "a", "of"))

plot_graph(DF)
plot_graph(DF, edge_type = "lorem")
plot_graph(DF, edge_type = "line")
plot_graph(DF, edge_type = "arc", edge_bend = 0.5)

plot_graph2(DF)
plot_graph2(DF, text)
plot_graph2(DF, text, text_contour_color = "white")
plot_graph(DF, edge_type = "line")
