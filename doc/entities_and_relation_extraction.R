## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  # dimensiongs for figure
  fig.width = 5,
  fig.height = 3,
  dpi = 150
)
library(txtnet)

## ----ll libs------------------------------------------------------------------
library(txtnet)

## ----NER----------------------------------------------------------------------
"John Does lives in New York in United States of America." |> extract_entity()

## ----connectors---------------------------------------------------------------
connectors("eng")
# or you can also use for english, to get the same result:
connectors("en")
# For portuguese
connectors("pt")
# to get the same result:
connectors("port")

# by default, the functions uses the parameter "misc". meaning "miscellaneous".
connectors("misc")

## ----NER pt-------------------------------------------------------------------
"João Ninguém mora em São José do Rio Preto. Ele esteve antes em Sergipe" |>
  extract_entity(connect = connectors("pt"))

vonNeumann_txt <- "John von Neumann (/vɒn ˈnɔɪmən/ von NOY-mən; Hungarian: Neumann János Lajos [ˈnɒjmɒn ˈjaːnoʃ ˈlɒjoʃ]; December 28, 1903 – February 8, 1957) was a Hungarian and American mathematician, physicist, computer scientist and engineer"
vonNeumann_txt |> extract_entity()

## ----vonNeumann graph---------------------------------------------------------
vonNeumann_txt |> extract_graph()

## ----vonNeumann graph stopwords-----------------------------------------------
my_sw <- c(stopwords::stopwords(language = "en", source = "snowball", simplify = TRUE), "lol")

vonNeumann_txt |> extract_graph(sw = my_sw)

## ----read_html, echo=FALSE, eval=T--------------------------------------------
# pagina <- "https://en.wikipedia.org/wiki/GNU_General_Public_License" |> rvest::read_html()
# writeLines( as.character(pagina), "../inst/wiki_GNU.html" )
# page <- readLines("../inst/wiki_GNU.html") 
# page <- rvest::read_html("inst/wiki_GNU.html")
page <- rvest::read_html("../inst/wiki_GNU.html")

## ----echo=T, eval=FALSE-------------------------------------------------------
# page <- "https://en.wikipedia.org/wiki/GNU_General_Public_License" |> rvest::read_html()

## ----ex wikipedia scrape GNU, eval=TRUE---------------------------------------
text <- page |>
  rvest::html_nodes("p") |>
  rvest::html_text()

# looking at the scraped text:
text[1:2] # seeing the head of the text

## ----ex wikipedia plot GNU----------------------------------------------------
g <- text |> extract_graph(sw = my_sw)
g
g_N <- g |> dplyr::count(n1, n2, sort = T)
g_N

## ----ploting the graph--------------------------------------------------------
plot_graph(text, g_N)

## ----networkD3----------------------------------------------------------------
g_N |>
  head(100) |> # to reduce the amount of nodes and edges in the graph
  networkD3::simpleNetwork(
    height = "10px", width = "30px",
    linkDistance = 50,
    fontSize = 16
  )

## ----read_html hurricane, echo=TRUE, eval=FALSE-------------------------------
# # page <- "https://en.wikipedia.org/wiki/Hurricane_Milton" |> rvest::read_html()

## ----read_html wiki_Hurricane_Milton,  echo=FALSE, eval=T---------------------
# pagina <- "https://en.wikipedia.org/wiki/Hurricane_Milton" |> rvest::read_html()
# writeLines( as.character(pagina), "../inst/wiki_Hurricane_Milton.html" )
# page <- readLines("../inst/wiki_GNU.html") 
page <- rvest::read_html("../inst/wiki_Hurricane_Milton.html")

## ----ex2 wiki Hurricane-------------------------------------------------------
text <- page |>
  rvest::html_nodes("p") |>
  rvest::html_text()

text[1:2] # seeing the head of the tex

## ----ex2 wiki Hurricane2------------------------------------------------------
g <- text |> extract_graph(sw = my_sw)
# option 1: use counting the edge frequency
g_N <- g |> dplyr::count(n1, n2, sort = T)
# option 2: use count_graph function, same results
g_N <- g |> count_graph()

plot_graph(text, g_N, head_n = 50)

## ----networkD3 hurricane------------------------------------------------------
g_N |>
  head(100) |> # to reduce the amount of nodes and edges in the graph
  networkD3::simpleNetwork(
    height = "10px", width = "30px",
    linkDistance = 50,
    fontSize = 16
  )

## ----sotu---------------------------------------------------------------------
library(sotu) #  text examples of US presidents speeches

# checking the DF with the speeches
tibble::as_tibble(sotu_meta)

## ----sotu obama---------------------------------------------------------------
# checking what are the speeches of Obama
sotu_meta |> 
  dplyr::filter(grepl("Obama", president, ignore.case = T),
  grepl("2009", years_active))

# I picked this speech
text_sotu <- sotu_text[229] |> 
  paste(collapse = " ") # turning the vector into a single element
str(text_sotu) # first lines of the text

# As a matter of curiosity, checking the most frequent entities
text_sotu |> 
  extract_entity(sw = my_sw ) |> 
  plyr::count() |> 
  dplyr::arrange(-freq) |>
  head(30)

sotu_g_Ob <- text_sotu |> 
  paste(collapse = " ") |>
  extract_graph(sw = my_sw) 

plot_graph2(
  sotu_g_Ob ,
  dplyr::count(sotu_g_Ob, n1, n2, sort = T),
  head_n = 70,
  edge_color = "blue", edge_alpha = 0.1,
  text_size = 10,
  scale_graph = "log2") +
    ggplot2::labs(title= "Obama SOTU - First Year")

## ----sotu Trump---------------------------------------------------------------
# Trump, first Mandate
sotu_meta |> 
  dplyr::filter(grepl("Trump", president, ignore.case = T)  )

sotu_g_Tr  <- sotu_text[237] |> 
  paste(collapse = " ") |>
  extract_graph(sw = my_sw) 

#  the most frequent entities
sotu_g_Tr  |> 
  extract_entity(sw = my_sw ) |> 
  plyr::count() |> 
  dplyr::arrange(-freq) |>
  head(30)

plot_graph2(
  sotu_g_Tr ,
  dplyr::count(sotu_g_Tr, n1, n2, sort = T),
  head_n = 70,
  edge_color = "red",
  scale_graph = "log2",
  text_size = 10,
) +
    ggplot2::labs(title= "Trump SOTU - First Year")

## ----sotu obama trump graph---------------------------------------------------
# a regex to capture some words/patterns
term <- "\\bChin|Beijing|\\bXi\\b|Jinping"
term_  <- "China"

# checking what are the speeches of Obama
sotu_meta |> 
  dplyr::filter(grepl("Obama", president, ignore.case = T))

# Get all Obama speeches of his first mandate 
text_sotu_Ob  <- sotu_text[229:234]|>
  filter_by_query(term) 

sotu_g_Ob <- text_sotu_Ob |> 
  paste(collapse = " ") |>
  extract_graph(sw = my_sw) 

g_Ob  <- plot_graph2(
  sotu_g_Ob ,
  dplyr::count(sotu_g_Ob, n1, n2, sort = T),
  edge_color = "blue", edge_alpha = 0.1,
  scale_graph = "log2"
) +
    # ggplot2::labs(title= paste("Obama about", term))
    ggplot2::labs(title= "Obama")


# Trump, first Mandate
sotu_meta |> 
  dplyr::filter(grepl("Trump", president, ignore.case = T)  )

text_sotu_Tr <- sotu_text[237:240] |> 
  filter_by_query(term) 

sotu_g_Tr <- text_sotu_Tr |> 
  paste(collapse = " ") |>
  extract_graph(sw = my_sw) 

g_Tr  <- plot_graph2(
  sotu_g_Tr ,
  dplyr::count(sotu_g_Tr, n1, n2, sort = T),
  edge_color = "red",
  scale_graph = "log2"
) +
  # ggplot2::labs(title= paste("Trump about", term))
  ggplot2::labs(title= "Trump")

# joining the graphs
library(patchwork)
(g_Ob + g_Tr) +
   plot_annotation(
  title = 
    paste('Coocurrence of terms related to: "', 
      term_, '"') )

