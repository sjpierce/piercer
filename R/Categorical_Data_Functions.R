#'=============================================================================
#' @name DI
#'
#' @title Diversity Index
#'
#' @description Computes a diversity index for a categorical variable, in either
#'   unstandardized or standarized form.
#'
#' @param x A vector with counts for each of k categories, e.g. output from
#'   running table() on a vector.
#'
#' @param std A logical value controlling whether the function returns the
#'   standarized diversity index (I). It defaults to FALSE, which returns the
#'   unstandardized diversity index (D).
#'
#' @param verbose A logical value controling whether or not to print output to
#'   the screen (D, I, N, k, & upper limit of D)
#'
#' @param digits An integer specifying the number of decimal places used when
#'   rounding the result. Defaults to NULL, which does not round the result.
#'
#' @details This function implements the diversity index discussed by Agresti
#'   and Agresti (1978) for a categorical variable with k categories and sample
#'   size of N. The unstandardized form is called D, while the standardized form
#'   is called I. In the verbose output, Dlim is the upper limit of possible
#'   D values.
#'
#' @return A numeric value for the diversity index.
#'
#' @references Agresti, A., & Agresti, B. F. (1978). Statistical analysis of
#'    qualitative variation. Sociological Methodology, 9, 204-237.
#'    doi:10.2307/270810
#'
#' @examples
#' test1 <- c(A = 12, B = 0, C = 0, D = 0)
#' test2 <- c(A = 3, B = 3, C = 3, D = 3)
#' test3 <- c(A = 2, B = 4, C = 5, D = 1)
#' DI(test1, verbose = TRUE)
#' DI(test2, verbose = TRUE, digits = 3)
#' DI(test3, std = TRUE, verbose = TRUE)
#'
#' @export
DI <- function(x, std = FALSE, verbose = FALSE, digits = NULL) {
  k    <- length(x)         # No. of categories
  p    <- prop.table(x)     # Convert to proportions
  N    <- sum(x)            # Sample size
  Dhat <- 1 - sum(p^2)
  Dlim <- (k - 1)/k         # Upper limit of possible D-hat values
  Ihat <- (k/(k - 1))*Dhat  # Standardized value (I-hat).
  # Round results (if requested)
  if(!is.null(digits)) {
    Dhat <- round(Dhat, digits = digits)
    Ihat <- round(Ihat, digits = digits)
    Dlim <- round(Dlim, digits = digits)
    }
  # Select which result to return.
  res  <- ifelse(std, Ihat, Dhat)
  # Print verbose output (if requested)
  if(verbose) {
    print(data.frame(Dhat, Ihat, N, k, Dlim))
    }
  # Return a final result
  return(res)
}

#'=============================================================================
