#' graph substitution
#'
#' @description
#' Substitute node columns (columns 1 and 2) of a graph data frame with a
#' dictionary of substitutions.
#'
#' @param DF a graph data frame
#' @param df_subs a data frame with the substitutions to be made, where the
#' first column is what to substitute and column 2 is what to substitute with.
#' It uses regular expressions. It can be left empty to use only the rm_symbols
#' parameter
#' @param rm_symbols remove non-word characters (symbols), preserving
#' letter, numbers and the underscore (to make it clear the words boundaries).
#' Default regex pattern: "[^\\w\\s\\_\\&]". Is possible to use other custom
#' regex patterns. If no symbol substitution is desired, leave it empty..
#'
#' @export
#'
#' @examples
#' # a test tibble
#' test_graph <- tibble::tibble(
#'   n1 = c("A", "B", "A", "C", "B", "Ab", "A", "D"),
#'   n2 = c("B", "Ab", "B", "D", "C", "A", "C", "D")
#' )
#'
#' # dataframe with substitutions
#' DF_substitution <- tibble::tribble(
#'   ~col1, ~col2,
#'   "B", "blah",
#'   "C", "Capybara"
#' )
#'
#' # Doing the substitutions
#' graph_subs(test_graph, DF_substitution)
graph_subs <- function(DF, df_subs = NA, rm_symbols = "[^\\w\\s\\_\\&]") {
  col_names <- colnames(DF)

  if (!(missing(df_subs) || is.null(df_subs))) {
    # if (exists("df_subs")) {
    # if (!is.na(df_subs)) {
    # mesaage("Exists df subs: ", exists("df_subs"))
    subs <- tibble::deframe(df_subs)
    # Apply string replacements to both columns
    # message("inicio df_subs. Class", class(subs))
    for (col in col_names[1:2]) {
      DF[[col]] <- stringr::str_replace_all(DF[[col]], subs) |>
        stringr::str_squish()
    }
    # message("fim df_subs")
  }

  # if rm_symbol not empty
  if (rm_symbols != "") {
    # message("rm_symbol")
    for (col in col_names[1:2]) {
      DF[[col]] <- DF[[col]] |>
        stringr::str_remove_all(rm_symbols) |>
        stringr::str_replace_all("_+", "_") |>
        stringr::str_squish()
    }
  }

  return(DF)

  # DF |>
  #   dplyr::mutate(
  #     !!col_names[1] := stringr::str_replace_all(
  #       !!dplyr::sym(col_names[1]), subs
  #     ) |> stringr::str_squish(),
  #     !!col_names[2] := stringr::str_replace_all(
  #       !!dplyr::sym(col_names[2]), subs
  #     ) |> stringr::str_squish()
  #   )
}
# library(testthat)
