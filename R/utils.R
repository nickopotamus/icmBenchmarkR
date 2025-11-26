#' Detect the free-text comments column
#'
#' @param df A cleaned data frame.
#'
#' @return The name of the comments column.
#'
#' @export
detect_comments_column <- function(df) {

  # Keep only character columns
  char_cols <- df |> dplyr::select(where(is.character))

  if (ncol(char_cols) == 0) {
    return(NULL)
  }

  # For each character column, compute maximum string length
  max_lengths <- sapply(char_cols, function(col) {
    suppressWarnings(max(nchar(col), na.rm = TRUE))
  })

  # Return the column with the LONGEST text
  comments_col <- names(which.max(max_lengths))

  comments_col
}
