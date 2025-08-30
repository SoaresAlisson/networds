#' graph substitution
#'
#' Substitute node columns (columns 1 and 2) of a graph data frame with a
#' dictionary of substitutions.
#'
#' @description
#'
#' @param DF a graph data frame
#' @param df_subs a data frame with the substitutions to be made, where the
#' first column is what to substitute and column 2 is what to substitute with.
#' It uses regular expressions.
#'
#' @export
#'
graph_subs <- function(DF, df_subs) {
  col_names <- colnames(DF)
  subs <- tibble::deframe(df_subs)

  # Apply string replacements to both columns
  for (col in col_names[1:2]) {
    DF[[col]] <- stringr::str_replace_all(DF[[col]], subs) |>
      stringr::str_squish()
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
