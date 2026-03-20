#' extract the frequency of nodes in a text
#'
#' @param vert a vector of nodes/terms
#' @param text an input text
#' @param lower Convert words to lowercase.
#'
#' @return a dataframe of nodes and its frequency
freq_nodes <- function(vert, text, lower = TRUE) {
  # frequency of nodes/terms
  if (lower) {
    text2 <- tolower(text)
    vert2 <- tolower(vert) |> escape_regex(word_delim = TRUE)
  } else {
    text2 <- text
    vert2 <- vert |> escape_regex(word_delim = TRUE)
   }
 

    freqPPN <- lapply(vert2, \(V) {
        text2 |> stringr::str_extract_all(V) }) |> 
      unlist() |>
      count_vec()

  # check if there is missing input vectors
      freqPPN_nodes <- freqPPN$x

      # vertices_not_in_freq <- vert2[!grepl(vert2, freqPPN_nodes)]
      vertices_not_in_freq <- lapply(vert2, \(V) 
        { V[!any(grepl(V, freqPPN_nodes))] }) |> 
        unlist()

      vertices_not_in_freq_length <- length(vertices_not_in_freq)

      if (vertices_not_in_freq_length > 0) {
        stop(paste0(
          "There is ",
          vertices_not_in_freq_length,
          " nodes not found in the text provided. Make sure that text parameter in the present function is the same text used in graph extraction. Nodes not found: \n",
          paste(vertices_not_in_freq, collapse = ", ")
        ))
      }
  
  return(freqPPN)
}

# freq_ns_old <- function(vert, text, lower = TRUE) {
#   # fry of nodes/terms
#
#   if(lower) {
#   vert2 <- tolower(vert) |> escape_regex(word_delim = TRUE)
#     txt <- tolower(text)
#
#     PPN <- lapply(vert2, \(v) {
#     txt |> stringr::str_extract_all(v)
#     # pattern = stringr::fixed(v)
#     # pattern = stringr::fixed(paste0("\\b", v, "\\b"))
#     })
#   } else {
#     vert2 <- vert |> escape_regex(word_delim = TRUE)
#
#     freqPPN <- lapply(vert2, \(v) {
#       text |> stringr::str_extract_all(v)
#     })
#   }
#
#   freqPPN <- freqPPN |>
#     unlist() |>
#     count_vec()
#
#   # If used word like "New_York", it will clean, search in text, and put it back
#   # if (length(vert) != nrow(freqPPN)) {
#   # freqPPN_nodes <- gsub(x = freqPPN$x, "\\.", "\\\\.")
#   freqPPN_nodes <- freqPPN$x
#
#   inVert_not_inFreqPPN <- vert[!vert %in% freqPPN_nodes]
#
#   if (length(inVert_not_inFreqPPN) > 0) {
#     missing_nodes <- gsub(x = inVert_not_inFreqPPN, "_", " ")
#
#     missing_nodes_df <- lapply(missing_nodes, \(v) {
#       text |>
#         stringr::str_extract_all(pattern = paste0("\\b(?i)", v, "\\b"))
#     }) |>
#       unlist() %>%
#       {
#         if (lower) {
#           tolower(.)
#         } else {
#           .
#         }
#       } |>
#       count_vec() |>
#       dplyr::mutate(x = gsub(x = x, " ", "_"))
#
#     # freqPPN <- rbind(freqPPN, missing_nodes_df) |> suppressWarnings()
#     freqPPN <- rbind(freqPPN, missing_nodes_df) |>
#       suppressWarnings() |>
#       dplyr::group_by(x) |>
#       dplyr::summarise(freq = sum(freq))
#   }
#
#   # if still there is vertices not found in word frequency: stop
#   if (lower) {
#     vert <- tolower(vert)
#     vertices_not_in_freq <- vert[!vert %in% freqPPN$x]
#   } else {
#     vertices_not_in_freq <- vert[!vert %in% freqPPN$x]
#   }
#
#   vertices_not_in_freq_length <- length(vertices_not_in_freq)
#
#   if (vertices_not_in_freq_length > 0) {
#     stop(paste0(
#       "There is ",
#       vertices_not_in_freq_length,
#       " nodes not found in the text provided. Make sure that text parameter in the present function is the same text used in graph extraction. Nodes not found: \n",
#       paste(vertices_not_in_freq, collapse = ", ")
#     ))
#   }
#
#   return(freqPPN)
# }
#
