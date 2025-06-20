#' @name var_missingness
#'
#' @title Variables Missingness
#'
#' @description This function summarizes the numbers of valid (non-missing) and
#'   missing values and the percent of missing values for each variable in a
#'   data frame or tibble.
#'
#' @param x A dataframe for which you want a summary of variable-level missingness.
#'
#' @details This function summary of how many valid and missing values there are
#'   for each variable in a data frame.
#'
#' @return A data frame with one row per variable in the the input data frame
#'   x and 7 columns (position and name of the variable, total number of values,
#'   plus number and percent of valid values, number and percentage of missing
#'   values.
#'
#' @importFrom tibble rowid_to_column tibble
#' @importFrom purrr list_rbind map
#'
#' @examples
#' var_missingness(cars)
#'
#' @export
var_missingness <- function(x) {
  list(x) %>%
    map( \(.x) tibble(
      Name = names(.x),
      N_Total = nrow(.x),
      N_Valid = colSums(!is.na(.x)),
      Pct_Valid = 100 * colSums(!is.na(.x)) / nrow(.x),
      N_Missing = colSums(is.na(.x)),
      Pct_Missing = 100 * (colSums(is.na(.x)) / nrow(.x))
    )) %>%
    list_rbind() %>%
    rowid_to_column(var = "Position")
}
