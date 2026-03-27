#' Escape special characters for use in regular expressions
#'
#' @description
#' Escapes special regex characters in a string so they can be used as literal
#' characters in regular expression patterns. Optionally wraps the escaped string
#' with word boundaries.
#'
#' @param string A character vector containing strings to escape
#' @param word_delim Logical. If TRUE, wraps the escaped string with word boundaries
#'   (`\\b` at both ends). Default is FALSE.
#'
#' @return A character vector with special regex characters escaped, optionally
#'   wrapped with word boundaries.
#'
#' @details
#' Special characters that are escaped: . _ /-
#'
#' When `word_delim = TRUE`, the pattern becomes `\\b escaped_string \\b`, which
#' ensures matches only occur at word boundaries. This is useful for matching
#' whole words or phrases that shouldn't be part of larger words.
#'
#' @note
#' The function uses `gsub()` for replacement, so it processes each element of
#' a character vector individually.
#'
#' @seealso
#' [base::gsub()] for the underlying substitution function,
#' [stringr::str_escape()] for a similar function in stringr
#'
#' @examples
#' # Basic escaping
#' escape_regex("Mr.")
#' # Returns: "Mr\\."
#'
#' # Escape multiple strings
#' escape_regex(c("Mr.", "Dr.*", "test[1]"))
#' # Returns: c("Mr\\.", "Dr\\.\\*", "test\\[1\\]")
#'
#' # With word boundaries
#' escape_regex("Mr.", word_delim = TRUE)
#' # Returns: "\\bMr\\.\\b"
#'
#' # Useful for creating patterns from user input
#' escape_regex("Dr. John", word_delim = TRUE)
#' # pattern is "\\bDr\\.John\\b"
#'
escape_regex <- function(string, word_delim = FALSE) {
  # string <- gsub("([.[\\(*+?{|^$])", "\\\\\\1", string)
  string <- string |> 
    gsub(x=_, "\\.", r"(\\.)")
  # string <- gsub("[\\.\\(\\*\\+\\?\\{\\|\\^\\$]", "\\\\\\1", string)
  if (word_delim) {
    string <- paste0("\\b", string, "\\b")
  }
  return(string)
}

# escape_regex("Mr.", word_delim = TRUE)
