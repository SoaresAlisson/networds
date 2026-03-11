# # VER great..../GR_analise
#' from semgram output, join passive and active voices
#' @param semgram a object from spacyr::spacy_parse() |> semgram::extract_motifs()
#'
#' @export
extract_triplets <- function(semgram) {

  At <- semgram$agent_treatments |>
    rename(from = Agent, label = treatment, to = Entity)
  aP <- semgram$action_patients |>
    rename(from = Entity, label = action, to = Patient)

  dplyr::bind_rows(At, aP) |>
    # semgramGR2$action_patients
    # triplCount <- triplets |>
    dplyr::count(from, label, to, sort = T) |>
    dplyr::rename(value = n) |>
    tibble::as_tibble() |>
    dplyr::arrange(-value)
}
