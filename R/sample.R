#' A sample graph dataframe (tbl-graph)
#'
#' @description
#' A sample dataset used to illustrate the functionality of the package.
#'
#' @format A tibble graph, from {tidygraphs} package
#' \describe{
#'   \item{nodes}{the nodes names}
#'   \item{edges}{the relationships between the nodes}
#' }
#' @export
#' @examples
#' data(g)
#' head(g)
g <- tidygraph::tbl_graph(
  nodes = data.frame(name = c(
    "Alice", "Bob", "Charlie",
    "David", "John", "Mary"
  )),
  edges = data.frame(
    from = c(1, 1, 2, 3, 2, 6),
    to = c(2, 3, 4, 4, 5, 5)
  )
)



#' A Text Sample
#' @description
#' A sample text from text from https://www.bbc.com/news/articles/c7ve36zg0e5o
#' @export
#' @examples
#' data(package = "txtnet")
#' head(text_sample)
text_sample <- r"(A man is being questioned about the fatal shooting of a healthcare insurance boss in New York last week, officials have told the BBC's US partner CBS News.
UnitedHealthcare boss Brian Thompson, 50, was fatally shot in the back on Wednesday morning last week outside the Hilton hotel in Midtown Manhattan.
Police say Thompson was targeted in a pre-planned killing, for which they do not yet have a motive. Nor have officers revealed a name of the suspect.
It is not clear if the person being questioned is the suspect who authorities have been searching for.
Investigators have been using surveillance photos, bullet casings with cryptic messages written on them, and the suspect's movements to track him down. They are also working with the FBI and authorities in other states...
The manhunt for a suspect who gunned down a healthcare chief executive in New York is now in its third day, with police chasing several different leads.
UnitedHealthcare boss Brian Thompson, 50, was fatally shot in the back on Wednesday morning outside the Hilton hotel in Midtown Manhattan.
Police say Thompson was targeted in a pre-planned killing, for which they do not yet have a motive.
Investigators are using surveillance photos, bullet casings with cryptic messages written on them, and the suspect's movements to track him down. They are also working with the FBI and authorities in other states as the search expands beyond New York.)"
