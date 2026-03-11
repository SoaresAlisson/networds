rgx_abbrev <- "([:upper:]\\.){2,}"
# rgx_word <- "(\\b[A-ZÀ-Ÿ][[A-ZÀ-Ÿ][a-zà-ÿ]\\.\\-]+\\b)"
# rgx_word <- "(\\b[A-ZÀ-Ÿ][A-ZÀ-Ÿa-zà-ÿ0-9\\.\\-]+\\b)"
# unicode in order https://symbl.cc/en/unicode-table/#spacing-modifier-letters
rgx_word <- "(\\b[A-ZÀ-ß][A-ZÀ-ßa-zà-ÿ0-9\\.\\-]+\\b)"


#' A lowercase connectors between two proper names
#'
#' In some languages there is a lowercase connector between two or more proper names.
#' This function returns a regex pattern by language with lowercase allowed connectors.
#' @param lang language, It can be en, es, pt and misc (with many languages)
#' @export
#' @examples
#'
#' connectors("es")
#' connectors("pt")
#' connectors("port")
#' connectors("en")
#' connectors("misc")
connectors <- function(lang = "pt") {
  if (
    lang %in% c("pt", "por", "port", "portugues", "português", "portuguese")
  ) {
    conn <- c("da", "das", "de", "do", "dos")
    # } else if (lang %in% s2v("es spa spanish espanol español")) {
  } else if (lang %in% c("es", "spa", "spanish", "espanol", "español")) {
    conn <- "del"
    # } else if (lang %in% s2v("en eng english inglês")) {
  } else if (lang %in% c("en", "eng", "english", "inglês")) {
    conn <- c("of", "of the")
  } else if (lang == "misc") {
    # conn <- "of the of_the von van del"
    conn <- c("of", "the", "of the", "von", "van", "del")
  } else {
    paste("Lang not found:", lang) |> stop()
  }

  # conn |> s2v()
  conn
}

#' A rule based entity extractor
#' extracts the entity from a text using regex. This regex captures all uppercase words, words that begin with upper case. If there is sequence of this patterns together, this function also captures.
#' In the case of proper names with common lower case connectors like "Wwwww of Wwwww" this function also captures the connector and the subsequent uppercase words.
#' @param text an input text
#' @param connect a vector of lowercase connectors. Use use your own, or use the function "connector" to obtain some patterns.
#' @param sw a vector of stopwords
#' @param underscore keep underscore to make compounded words a unique word?
#' @export
#' @examples
#' "John Does lives in New York in United States of America." |> extract_entity()
#' "João Ninguém mora em São José do Rio Preto. Ele esteve antes em Sergipe" |> extract_entity(connect = connectors("pt"))
# text |> extract_entity()
extract_entity <- function(
    text,
    connect = connectors("misc"),
    sw = "the",
    underscore = TRUE) {
  # connectors <- connectors |> s2v()
  connector <- paste(connect, collapse = "|") |> gsub("(.*)", "(\\1)", x = _)

  # rgx_ppn <- paste0("(", rgx_word, "+ ?)+", "", connector, "? (", rgx_word, " ?)*")
  rgx_ppn <- paste0(
    "(",
    rgx_word,
    "+ ?)+",
    "(",
    connector,
    "? (",
    rgx_word,
    " ?)+)*"
  )

  text_vec <- text |>
    stringr::str_extract_all(rgx_ppn) |>
    unlist() |>
    stringr::str_trim()
  # trimws()

  # deleting stopword elements
  text_vec <- text_vec[!text_vec %in% stringr::str_to_title(sw)]

  if (underscore) {
    gsub(x = text_vec, " ", "_")
  } else {
    text_vec
  }
}

#TODO gen_stopwords neste exemplo 

#' Substitute proper names/entities spaces with underscore in the text.
#'
#' given a text and a vector of entities, it substitutes the spaces with underscores, so the entities are identified.
#'
#' @param text an input text
#' @param entities an input vector, as exported by `extract_entity()`
#' @export
#' @examples
#' texto_teste <- "José da Silva e Fulano de Tal foram, bla Maria Silva. E depois disso, bla Joaquim José da Silva Xavier no STF"
#' ppn <- texto_teste |> extract_entity(connectors("pt"), sw = gen_stopwords("pt"))
#' texto_teste |> subs_ppn(ppn)
#' texto_teste |> subs_ppn(ppn, method = "loop")
#' text <- texto_teste |> subs_ppn(ppn)
#' text
# text |>
#   strsplit(" ") |>
#   unlist() |>
#   count_vec()
subs_ppn <- function(text, entities, method = "normal") {
  # entities <- texto_teste |> extract_entity(connectors("pt"), sw = gen_stopwords("pt"))
  entities <- entities |> unlist()

  if (method == "loop") {
    ent_df <- data.frame(entities = unique(entities)) |>
      dplyr::mutate(
        entities2 = gsub(" ", "_", entities),
        entities = gsub(" ", "[ _]", entities)
      )

    for (i in 1:nrow(ent_df)) {
      message("processing ", i, " of ", nrow(ent_df))
      text <- text |>
        stringr::str_replace_all(ent_df[i, "entities"], ent_df[i, "entities2"])
    }
  } else if (method == "normal") {
    entities2 <- grep(" ", entities)
    # named_vec <- stringr::str_replace_all(entities, " ", "_")
    named_vec <- gsub(" ", "_", entities2)
    # names(named_vec) <- entities2
    names(named_vec) <- gsub(" ", "[ _]", entities2)

    # text <- purrr::map2_chr(ent_df$entities, ent_df$entities2, ~ stringr::str_replace_all(text, .x, .y))
    text <- stringr::str_replace_all(text, named_vec)
  }

  return(text)
}
# "asdc,casd_asd. Asc" |> stringr::str_extract_all("\\W+")
# c("as as", "Joaquim cas", "as_cas", "asdcasdc") |> sto::grep2("\\W")

#' tokenize and selects only sentences/paragraphs with more than one entity per sentence or paragraph
#' @param text an input text
#' @param using sentence or paragraph to tokenize
#' @param connect lowercase connectors, like the "von" in "John von Neumann". To use pre built connectors use `connectors()``
#' @param sw stopwords vector. To use pre built stopwords use `gen_stopwords()`
#'
#' @export
#' @examples
#' "John Does lives in New York in United States of America." |> extract_relation()
#' "João Ninguém mora em São José do Rio Preto. Ele foi para o Rio de Janeiro." |> extract_relation(connector = connectors("pt"))
extract_relation <- function(
    text,
    using = "sentences",
    connect = connectors("misc"),
    sw = gen_stopwords("en")) {
  if (using == "sentences" || using == "sent") {
    message("Tokenizing by sentences")
    list_w <- text |>
      tokenizers::tokenize_sentences()
  } else if (using == "paragraph" || using == "par") {
    message("Tokenizing by paragraph")
    list_w <- text |>
      tokenizers::tokenize_paragraphs()
  } else {
    stop(paste("Parameter invalid: ", using))
  }

  list_w <- lapply(
    X = list_w,
    \(txt) {
      extract_entity(txt, connect = connect, sw = sw)
    }
  )

  list_length <- list_w |>
    lapply(length) |>
    unlist()

  # selecting only sentences with more than one entity
  list_w[list_length > 1] #|> lapply(combn, 2, simplify = TRUE)
}

#' Extract a non directional graph based on co-occurrence in the token.
#' It extracts only if two entities are mentioned in the same token (sentence or paragraph)
#' @param text an input text
#' @param using sentence or paragraph to tokenize
#' @param connect lowercase connectors, like the "von" in "John von Neumann".
#' @param sw stopwords vector.
#' @param loop if TRUE, it will not remove loops, a node pointing to itself.
#' @export
#' @examples
#' text <- "John Does lives in New York in United States of America. He  is a passionate jazz musician, often playing in local clubs."
#' extract_graph(text)
extract_graph <- function(
    text,
    using = "sentences",
    connect = connectors("misc"),
    sw = c("of", "the"),
    loop = FALSE) {
  list_ent <- text |> extract_relation(using, connect, sw)
  graph <- tibble::tibble(n1 = as.character(""), n2 = as.character(""))
  # list_length <- list_ent |> length()

  graph <- lapply(list_ent, \(e) {
    items <- e |> combn(2, simplify = FALSE)
    items_length <- length(items)
    lapply(1:items_length, \(x) {
      line <- unlist(c(items[x][1], items[x][2]))
      graph <- rbind(graph, line)
      graph
    }) |>
      dplyr::bind_rows() |>
      dplyr::filter(n1 != "")
  }) |>
    dplyr::bind_rows()

  if (!loop) {
    graph <- graph |>
      dplyr::mutate(loop = (n1 == n2)) |>
      dplyr::filter(loop == FALSE) |>
      dplyr::select(-loop)
  }
  return(graph)
}


#' Extract a non directional graph based on co-occurrence in the token and returns a tibble
#' It extracts only if two entities are mentioned in the same token (sentence or paragraph)
#'
#' @param df a data frame with two columns: text and id
#' @param column_id name of the column with the id
#' @param column_text name the column with the text to extract the graph
#' @param using sentence or paragraph to tokenize
#' @param connect lowercase connectors, like the "von" in "John von Neumann".
#' @param sw stopwords vector.
#' @param loop if TRUE, it will not remove loops, a node pointing to itself.
#'
#' @export
#'
#' @examples
#' # creating a dataframe with text and id
#' DF <- data.frame(text = c("John Does lives in New York in United States of America. He  is a passionate jazz musician, often playing in local clubs.", r"(John Michael "Ozzy" Osbourne (3 December 1948 – 22 July 2025) was an English singer, songwriter, and media personality. He co-founded the pioneering heavy metal band Black Sabbath in 1968, and rose to prominence in the 1970s as their lead vocalist. During this time, he adopted the title "Prince of Darkness".[3][4] He performed on the band's first eight albums, most notably including Black Sabbath, Paranoid (both 1970) and Master of Reality (1971), before he was fired in 1979 due to his problems with alcohol and other drugs.)")) |> dplyr::mutate(id = paste0("id_", dplyr::row_number()))
#' extract_graph_df(DF, "id", "text")
extract_graph_df <- function(
    df,
    column_id,
    column_text,
    using = "sentences",
    connect = connectors("misc"),
    sw = c("of", "the"),
    loop = FALSE) {
  df_out <- list()

  for (i in 1:nrow(DF)) {
    df_out[[i]] <- DF[[i, column_text]] |>
      extract_graph() |>
      dplyr::mutate(txt_id = DF[[i, column_id]])
  }

  df_out |> dplyr::bind_rows()
}


#' extract a graph from text, using custom regex pattern as nodes.
#'
#' @return a graph
#' keywords internal
extract_graph_rgx <- function(
    text,
    pattern,
    sw = gen_stopwords("en"),
    count_graphs = FALSE) {
  text
  # TODO
}
