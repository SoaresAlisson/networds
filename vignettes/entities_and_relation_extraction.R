## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(txtnet)

## ----NER----------------------------------------------------------------------
"John Does lives in New York in United States of America." |> extract_entity()

## -----------------------------------------------------------------------------
connectors("eng")
connectors("pt")
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

## ----echo=FALSE, eval=T-------------------------------------------------------
# pagina <- "https://en.wikipedia.org/wiki/GNU_General_Public_License" |> rvest::read_html()
# writeLines( as.character(pagina), "../inst/wiki_GNU.html" )
# page <- readLines("../inst/wiki_GNU.html") 
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

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# # page <- "https://en.wikipedia.org/wiki/Hurricane_Milton" |> rvest::read_html()

## ----echo=FALSE, eval=T-------------------------------------------------------
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

