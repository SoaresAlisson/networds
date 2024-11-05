# nao gerar rd
s2v <- function(char, sep = " |\\n|\\t|\\r", wss = "_", print = FALSE) {
  vec <- char |>
    strsplit(sep) |>
    unlist() |>
    gsub2(" +", " ") |> # strip extra white spaces
    gsub2("[,;]", " ") |> # strip comma
    stringi::stri_remove_empty() |>
    gsub2("^ | $", "") |> # strip extra white spaces
    gsub2(wss, " ")

  if (!print) {
    return(vec)
  } else {
    vec |>
      paste(collapse = "', '") |>
      gsub2("^", "c('") |>
      gsub2("$", "')")
  }
}


