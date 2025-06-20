#' @name cvv_missingness
#'
#' @title Cases, Variables, and Values Missingness
#'
#' @description This function summarizes the numbers and percentages of complete
#'   and incomplete cases, variables, and values in a data frame or tibble.
#'
#' @param x A dataframe for which you want a summary of missingness.
#'
#' @details This function provides high-level summary of how much missing data
#'   there is in a data frame.
#'
#' @return A data frame with 3 rows (one per subset of data, complete,
#'   incomplete, and all) and 7 columns (subset of the data, number and
#'   percentage of cases, number and percentage of variables, and number and
#'   percentage of values.
#'
#' @importFrom tibble tibble
#' @importFrom stats complete.cases
#'
#' @examples
#' cvv_missingness(cars)
#'
#' @export
cvv_missingness <- function(x) {
  N_cases  <- nrow(x)                      # All cases
  N_ccases <- sum(complete.cases(x))       # Complete cases
  N_icases <- sum(!complete.cases(x))      # Incomplete cases
  N_vars   <- ncol(x)                      # All variables
  N_cvars  <- sum(colSums(is.na(x)) == 0)  # Complete variables
  N_ivars  <- sum(colSums(is.na(x)) > 0)   # Incomplete variables
  N_vals   <- N_cases * N_vars             # All values
  N_cvals  <- sum(!is.na(x))               # Complete values (non-missing)
  N_ivals  <- sum(is.na(x))                # Incomplete values (missing)

  tibble(
    Subset = c("Complete", "Incomplete", "All"),
    Cases = c(N_ccases, N_icases, N_cases),
    Cases_P = 100 * c(N_ccases, N_icases, N_cases) / N_cases,
    Variables = c(N_cvars, N_ivars, N_vars),
    Variables_P = 100 * c(N_cvars, N_ivars, N_vars) / N_vars,
    Values = c(N_cvals, N_ivals, N_vals),
    Values_P = 100 * c(N_cvals, N_ivals, N_vals) / N_vals
  )
}
