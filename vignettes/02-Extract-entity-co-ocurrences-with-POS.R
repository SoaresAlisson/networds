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

