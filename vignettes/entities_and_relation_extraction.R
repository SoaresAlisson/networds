## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  # cache = FALSE,
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

## ----entities pt--------------------------------------------------------------
"João Ninguém mora em São José do Rio Preto. Ele esteve antes em Sergipe" |>
  extract_entity(connect = connectors("pt"))

vonNeumann_txt <- "John von Neumann (/vɒn ˈnɔɪmən/ von NOY-mən; Hungarian: Neumann János Lajos [ˈnɒjmɒn ˈjaːnoʃ ˈlɒjoʃ]; December 28, 1903 – February 8, 1957) was a Hungarian and American mathematician, physicist, computer scientist and engineer"
vonNeumann_txt |> extract_entity()

## ----vonNeumann graph---------------------------------------------------------
vonNeumann_txt |> extract_graph()

## ----vonNeumann graph stopwords-----------------------------------------------
my_sw <- c(stopwords::stopwords(
  language = "en",
  source = "snowball", simplify = TRUE
), "lol")

vonNeumann_txt |> extract_graph(sw = my_sw)

