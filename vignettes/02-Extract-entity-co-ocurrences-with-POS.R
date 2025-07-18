## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup library------------------------------------------------------------
library(networds)
library(spacyr)

## ----browser, echo=FALSE------------------------------------------------------
options(browser="firefox")

## ----install spacyR, eval = FALSE---------------------------------------------
# install.packages("spacyr")
# 
# # if you prefer, or maybe if the CRAN version is buggy, install the GitHub one:
# pak::pkg_install("quanteda/spacyr")
# 
# # to Install spaCy and requirements (python env). With empty parameters, it will
# # install the default “en_core_web_sm” model.
# spacyr::spacy_install()
# 
# spacyr::spacy_initialize()

