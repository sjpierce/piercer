#' @name display_num
#'
#' @title Display Numbers
#'
#' @description This function converts very large and very small numeric values
#'   into a convenient format by using scientific notation
#'
#' @param x A numeric value (or vector).
#'
#' @param upper A numeric value at or above which the number should be displayed
#'   in scientific notation.
#'
#' @param lower A numeric value below which the number should be displayed
#'   in scientific notation.
#'
#' @param sci.format A scientific notation format value suitable for use by
#'   sprintf().
#'
#' @param default A numeric format value suitable for use by sprintf().
#'
#' @details This function formats the number via sprintf(), using the
#'   sci.format when the number is larger than the upper parameter or smaller
#'   than the lower parameter. Otherwise it uses the default format. My main use
#'   for it is to display small p-values in a compact format without resorting
#'   to  imprecise categorizations like "< 0.001".
#'
#' @return A character string for the formatted number.
#'
#' @examples
#' display_num(c(2150000, 155, 0.025, 0.00006))
#'
#' @importFrom dplyr case_when
#'
#' @export
display_num <- function(x, upper = 1000000, lower = 0.001,
                        sci.format = "%.2e", default = "%.3f") {
  case_when(x >= upper ~ sprintf(sci.format, x),
            x < lower ~ sprintf(sci.format, x),
            .default = sprintf(default, x))
}
