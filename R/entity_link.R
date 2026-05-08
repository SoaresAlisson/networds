rgx_abbrev <- "([:upper:]\\.){2,}"
# rgx_word <- "(\\b[A-ZÀ-Ÿ][[A-ZÀ-Ÿ][a-zà-ÿ]\\.\\-]+\\b)"
# rgx_word <- "(\\b[A-ZÀ-Ÿ][A-ZÀ-Ÿa-zà-ÿ0-9\\.\\-]+\\b)"
# unicode in order https://symbl.cc/en/unicode-table/#spacing-modifier-letters
rgx_word <- "(\\b[A-ZÀ-ß][A-ZÀ-ßa-zà-ÿ0-9\\.\\-]+\\b)"


#' list of proper names connectors
#'
#' @description
#' list of connectors between proper names in different languages
connectors_list <- list(
  pt = c("da", "das", "de", "do", "dos"),
  es = "del",
  en = c("of", "of the"),
  misc = c("of", "the", "of the", "von", "van", "del", "de"),
  it = "di",
  ho = "van"
)

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
  # conn_pt  <- c("da", "das", "de", "do", "dos")
  # conn_es  <- "del"
  # conn_en <-  c("of", "of the")
  # conn_misc <-  c("of", "the", "of the", "von", "van", "del")

  if (
    lang %in% c("pt", "por", "port", "portugues", "português", "portuguese")
  ) {
    conn <- connectors_list["pt"]
    # } else if (lang %in% s2v("es spa spanish espanol español")) {
  } else if (
    lang %in% c("es", "spa", "span", "spanish", "espanol", "español")
  ) {
    conn <- connectors_list["es"]
    # } else if (lang %in% s2v("en eng english inglês")) {
  } else if (lang %in% c("en", "eng", "english", "inglês")) {
    conn <- connectors_list["en"]
  } else if (lang %in% c("it", "ita", "italian", "italiano")) {
    conn <- connectors_list["it"]
  } else if (lang %in% c("ho", "hol", "niederland")) {
    conn <- connectors_list["ho"]
  } else if (lang == "misc") {
    # conn <- "of the of_the von van del"
    conn <- connectors_list["misc"]
  } else if (lang == "all") {
    conn <- unlist(connectors_list) |> sort() |> unique()
  } else {
    paste("Lang not found:", lang) |> stop()
  }

  # conn |> s2v()
  return(conn)
}

# TODO gen_stopwords neste exemplo

#' Substitute proper names/entities spaces with underscore in the text.
#'
#' given a text and a vector of entities, it substitutes the spaces with underscores, so the entities are identified.
#'
#' @param text an input text
#' @param entities an input vector, as exported by `extract_entity_rb()`
#' @export
#' @examples
#' texto_teste <- "José da Silva e Fulano de Tal foram, bla Maria Silva. E depois disso, bla Joaquim José da Silva Xavier no STF"
#' ppn <- texto_teste |> extract_entity_rb(connectors("pt"), sw = gen_stopwords("pt"))
#' texto_teste |> subs_ppn(ppn)
#' texto_teste |> subs_ppn(ppn, method = "loop")
#' text <- texto_teste |> subs_ppn(ppn)
#' text
# text |>
#   strsplit(" ") |>
#   unlist() |>
#   count_vec()
subs_ppn <- function(text, entities, method = "normal") {
  # entities <- texto_teste |> extract_entity_rb(connectors("pt"), sw = gen_stopwords("pt"))
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
  sw = gen_stopwords("en")
) {
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
      extract_entity_rb(txt, connect = connect, sw = sw)
    }
  )

  list_length <- list_w |>
    lapply(length) |>
    unlist()

  # selecting only sentences with more than one entity
  list_w[list_length > 1] #|> lapply(combn, 2, simplify = TRUE)
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
  loop = FALSE
) {
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
  count_graphs = FALSE
) {
  text
  # TODO
}
