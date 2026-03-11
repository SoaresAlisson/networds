#
# ....... --- DEV --- .......
#
sw <- ""
sw <- sto::s2v("of by the a an in and was")
sw <- stopwords::stopwords("en")
my_sw <- stopwords::stopwords("en")
sw_pt <- stopwordsgen::gen_stopwords("pt", categories = "all")

Graph <- txt_wiki
text <- txt_wiki
DF <- txt_wiki |> cooccur(sw = sw)
head_n <- 30
color <- "lightblue"
edge_norm <- TRUE
token_by <- "sentence"
head_n = 30
lower = TRUE
edge_color = "lightblue"
edge_alpha = 0.5
edge_cut = 2
text_color = "black"
edge_norm = TRUE

net_wordcloud(DF, txt_wiki)
net_wordcloud(DF, txt_wiki, text_contour = "white" )

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

# .......................................................... different number of elements in freq graph and freq words
Vert <- vert |> sort()
FreqPPN <- freqPPN$x |> sort()
FreqPPN[FreqPPN %in% Vert]
FreqPPN[!FreqPPN %in% Vert]
not_listed <- Vert[!Vert %in% FreqPPN]
 |>
not_listed |> lapply(\(x), stringr::str_extract_all(txt_wiki, pattern = x))
stringr::str_extract_all(txt_wiki, pattern = not_listed[1]) |>  unlist()
grepl(x=txt_wiki, pattern = Vert[1]) |>  unlist()
grepl(x=txt_wiki, pattern = not_listed[1]) |>  unlist()
# "3d"   "december" "mangione" "york"
tokens 
# ..........................................................

# replace . to \\.
gsub(x=FreqPPN, "\\.", "\\\\.") 

# Ex
txt_wiki |> # text available in the package                          
  cooccur(sw = my_sw) |>                                             
  net_wordcloud(DF = _, text = txt_wiki, head_n = 90) # plotting                      

txt_wiki |> # text available in the package                          
  cooccur(sw = my_sw) |>                                             
  net_wordcloud(DF = _, text = txt_wiki, head_n = 90, edge_alpha = 0.1) # plotting                      

# using 
wiki_graph <- txt_wiki |> # text available in the package                          
  # because it is a vector, let's collapse it into a single element: 
  paste(collapse = " ") |>                                           
  extract_graph(sw = my_sw) |>                                       
  networds::count_graphs()  # counting the graphs
# As we extracted patterns using regex, the same pattern need to appear in the text feeded to the function
net_wordcloud(txt_wiki, DF = wiki_graph) # plotting
DF <- wiki_graph
text <- txt_wiki

# Test 
df_pt <- readRDS("~/Documentos/Programação/R/shared/UFBA/artigo_conspiracoes_Br/data/twitter_pt2023_haarp.Rds")
user_descr <- df_pt["user_description"] |>
  unlist() |>
  stringi::stri_remove_empty() |>
  paste(collapse = ". ")
cooc_df <- user_descr |> networds:::cooccur(sw = sw_pt)
net_wordcloud(user_descr, cooc_df, head_n = 130)

DF <- cooc_df
text <- user_descr

# ----------------------- 

#' extract the frequency of nodes in a text
freq_nodes <- function(vert, text){
  # frequency of nodes/terms
  freqPPN <- lapply(vert, \(v) {
    text |>
      # gsub(x = _, "\\.", "\\\\.") |>
      # stringr::str_extract_all(pattern = paste0("(?i)", v))
      stringr::str_extract_all(pattern = paste0("\\b(?i)", v, "\\b"))
    # stringr::str_extract_all(pattern = paste0("(?i)\\b", v, "\\b"))
  }) |>
    unlist() |>
    count_vec()

  # If used word like "New_York", it will clean, search in text, and put it back
  # if (length(vert) != nrow(freqPPN)) {
  freqPPN_nodes <- gsub(x = freqPPN$x, "\\.", "\\\\.")

  inVert_not_inFreqPPN <- vert[!vert %in% freqPPN_nodes]

  if (length(inVert_not_inFreqPPN) > 0) {
    missing_nodes <- gsub(x = inVert_not_inFreqPPN, "_", " ")

    missing_nodes_df <- lapply(missing_nodes, \(v) {
      text |>
        stringr::str_extract_all(pattern = paste0("\\b(?i)", v, "\\b"))
    }) |>
      unlist() |>
      count_vec() |>
      dplyr::mutate(x = gsub(x = x, " ", "_"))

    freqPPN <- rbind(freqPPN, missing_nodes_df) |> suppressWarnings()
  }

  # if there is vertices not found in word frequency
  vertices_not_in_freq <- vert[!vert %in% freqPPN$x]

  vertices_not_in_freq_length <- length(vertices_not_in_freq)
  
  if (vertices_not_in_freq_length  > 0) { 
    stop(paste0("There is ", vertices_not_in_freq_length, 
      " nodes not found in the text provided. Make sure that text parameter in the present function is the same text used in graph extraction. Nodes not found: \n", paste(vertices_not_in_freq, collapse = ", ") ))}

  return(freqPPN)

}

