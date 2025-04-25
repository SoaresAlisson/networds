## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup library------------------------------------------------------------
library(txtnet)
library(spacyr)

## ----browser, echo=FALSE------------------------------------------------------
options(browser="firefox")


## ----sample_txt---------------------------------------------------------------
data(package = "txtnet") # list the available dataset in package txtnet

## ----sample_txt head----------------------------------------------------------
# Text_sample of the package
# An example of text. Showing only the firsts lines
txt_wiki[1:5]

# parsing the POS tagging in it
POS <- txt_wiki |> spacyr::spacy_parse()


## ----spacyr::entity_extract---------------------------------------------------
POS |> spacyr::entity_extract()


## ----spacyr::entity_consolidate-----------------------------------------------
POS |> spacyr::entity_consolidate()


## ----filter_by_query----------------------------------------------------------
# tokenizing in sentences and filtering three lines that contains the word "police"
x <- txt_wiki[3:6] |> filter_by_query("Police")
x
class(x)


## ----filter_by_query unlist---------------------------------------------------
x <- txt_wiki[1:12] |> filter_by_query("Police", unlist=TRUE)
x
class(x)


## ----parsePOS-----------------------------------------------------------------
txt_wiki[1:12] |> 
  filter_by_query("Police", unlist=TRUE) |>
  parsePOS() 


## ----wiki get_cooc_entities, eval=T, echo=T-----------------------------------
x <- txt_wiki[2:44] |> 
  filter_by_query("Police") |>
  parsePOS()

x

g <- get_cooc_entities(x)

g


## ----q_plot-------------------------------------------------------------------
g  |> q_plot()


## ----plot_pos_graph-----------------------------------------------------------
graph_wiki <- txt_wiki[2:44] |> 
  filter_by_query("Police") |> 
  parsePOS() |>
  get_cooc_entities()  
  
plot_pos_graph(graph_wiki)


## ----plot_pos_graph + ggplot--------------------------------------------------
plot_pos_graph(graph_wiki, 
               font_size=1.3,
               edge_color = "tomato", 
               point_color = "aquamarine4") +
  ggplot2::labs(title = "Wordnetwork of Nouns in a Wikipedia text",
                caption = "The size of dots shows the frequency of the term.")


## ----viz_graph police, eval=T-------------------------------------------------
graph_wiki$edges |> viz_graph()

## -----------------------------------------------------------------------------
net_wordcloud(txt_wiki, dplyr::rename(g$edges, n = freq )) 
 


## ----Brian, eval=T, echo=T----------------------------------------------------
graph <- txt_wiki[2:44] |> 
  filter_by_query("Brian") |> 
  parsePOS() |>
  get_cooc_entities() 

plot_pos_graph(graph)


## ----Brian viz_graph, eval=T, echo=T------------------------------------------
graph$edges |> viz_graph()


## ----police -cardinal + viz_graph---------------------------------------------
graph_ppn <- filter_by_query(txt_wiki[2:44], "Police") |> 
  parsePOS(only_entities=FALSE)  |> 
  dplyr::filter(entity_type != "CARDINAL") |> # to clean the graph
  dplyr::mutate(token = gsub("Police", "police", token)) |> # normalize the term "police"
  get_cooc()

graph_ppn


## ----police -cardinal + viz_graph 2-------------------------------------------
viz_graph(graph_ppn$edges)


## ----police -cardinal + plot_pos_graph----------------------------------------
graph_ppn |> plot_pos_graph()


## ----eval=FALSE, echo=FALSE---------------------------------------------------
# library(sotu)
# # looking at availabe datasets in the package
# data(package = "sotu")
# sotu_text[237:240] |> filter_by_query("China")
# filter_by_query(sotu_text[237:240], "\\bXi\\b", unlist = TRUE)
# filter_by_query(sotu_text[237:240], "bob") # test
# 
# # Trump sentences from SOTU talking about "Xi"
# filter_by_query(sotu_text[237:240], "\\Xi\\b") |> parsePOS()
# # Trump sentences from SOTU talking about "China"
# filter_by_query(sotu_text[237:240], "China")[2] |> parsePOS()
# # Trump sentences from SOTU talking about "China"     d
# filter_by_query(sotu_text[237:240], "China") |> parsePOS()
# filter_by_query(sotu_text[237:240], "China") |> parsePOS(entities = FALSE)
# 
# filter_by_query(sotu_text[237:240], "China") |>
#   parsePOS() |>
#   get_pairs()
# 
# filter_by_query(sotu_text[237:240], "China") |>
#   parsePOS() |>
#   get_pairs(loop = TRUE)
# 

