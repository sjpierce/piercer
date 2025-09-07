#' @name dc_summary
#'
#' @title Dummy Code Summary
#'
#' @description This function summarizes a dummy-coded binary variable.
#'
#' @param vars A character vector of names for the dummy variables to be
#'   summarized.
#'
#' @param data A data frame containing the binary variable to be summarized.
#'
#' @param na.rm A logical value for whether to exclude missing values from the
#'  the total count used as the rate denominator. Defaults to TRUE.
#'
#' @param conf.level The confidence level to be passed to PropCIs::scoreci().
#'
#' @details This function computes a summary of the selected dummy coded
#'   variable that contains the number of values in each category (0 vs 1),
#'   the total number of observations, the proportion of 1s, and a Wilson score
#'   confidence interval for the proportion.
#'
#' @return A data frame with one row per variable named in vars, and 7 columns
#'   (Variable, number of No values, number of Yes values, total number of
#'   values, rate of Yes values, and lower and upper limits of the confidence
#'   interval for the rate.
#'
#' @importFrom PropCIs scoreci
#' @importFrom dplyr .data group_by if_else mutate n rowwise select summarise ungroup %>%
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#'
#' @examples
#' tmp <- data.frame(x1 = c(1,1,1,0,0,0,0,0,0,0),
#'                   x2 = c(1,1,1,1,1,1,NA,0,0,0))
#' dc_summary(data = tmp, var = "x1")
#' dc_summary(data = tmp, var = c("x1", "x2"), na.rm = FALSE)
#' dc_summary(data = tmp, var = c("x1", "x2"), na.rm = TRUE)
#'
#' @export

dc_summary <- function(data, vars, na.rm = TRUE, conf.level = 0.95) {

  # Local bindings to avoid "no visible binding for global variable"
  Missing <- NULL
  Total   <- NULL
  Yes     <- NULL

  # Verify all values are in the set (0, 1, NA)
  Unique_Values    <- data %>%
    select(all_of(vars)) %>%
    as.matrix() %>%
    as.vector() %>%
    unique()

  assert_that(all(Unique_Values %in% 0:1 | is.na(Unique_Values)),
              msg = "All values in the set of vars must be in the set (0, 1, NA)")

  Return_Vars <- if(na.rm)
    c("Variable", "No", "Yes", "Total", "Rate", "LL", "UL") else
    c("Variable", "Missing", "No", "Yes", "Total", "Rate", "LL", "UL")

  res <- data %>%
    pivot_longer(cols = all_of(vars),
                 names_to = "Variable",
                 values_to = "Value") %>%
    group_by(.data$Variable) %>%
    summarise(Missing = sum(is.na(.data$Value)),
              Total = if_else(na.rm == TRUE,
                              true = n() - Missing,
                              false = n()),
              Yes = sum(.data$Value, na.rm = TRUE)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(No = Total - Yes,
           Rate = Yes/Total,
           LL = scoreci(x = Yes, n = Total,
                        conf.level = conf.level)$conf.int[1],
           UL = scoreci(x = Yes, n = Total,
                        conf.level = conf.level)$conf.int[2]) %>%
    ungroup() %>%
    select(all_of(Return_Vars))

  return(res)
}


