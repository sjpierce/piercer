#' @name Fisher_r2z
#'
#' @title Fisher's r to z transformation.
#'
#' @description This function uses Fisher's r to z transformation to convert a
#'   correlation into a z-score.
#'
#' @param r A numeric value (or vector) for the correlation coefficient(s).
#'
#' @details This applies Fisher's r to z transformation to convert a correlation
#'   into a z-score. Numerical values of r close to -1 or 1 result Inf or -Inf,
#'   respectively. Missing values of r result in NA values. This transformation
#'   is also called the inverse hyperbolic tangent transformation (often written
#'   as arctanh), so Fisher_r2z() is a wrapper for the base::atanh() function.
#'
#' @return A numeric value (or vector) for the z-scores.
#'
#' @references To be added later.
#'
#' @seealso \code{\link[base]{atanh}} for the base function used in this
#'   function. \code{\link{Fisher_z2r}} for the inverse transformation that
#'   converts z back to r.
#'
#' @examples
#' Fisher_r2z(.3)
#' Fisher_r2z(c(-1, -.99, NA, 0, .5, .99, 1))
#'
#' @export
Fisher_r2z <- function(r) {
  assert_that(all(is.na(r) | is.numeric(r)),
              msg = "r must be NA or numeric")
  assert_that(all(is.na(r) | (r >= -1 & r <= 1)),
              msg = "Numeric r must be in the range -1 to 1")
  z <- atanh(r)
  return(z)
}

#'=============================================================================
#' @name Fisher_z2r
#'
#' @title Fisher's z to r transformation.
#'
#' @description This function uses Fisher's z to r transformation to convert a
#'   z-score into a correlation.
#'
#' @param z A numeric value (or vector) for the z-score(s).
#'
#' @details This function uses Fisher's z to r transformation to convert a
#'   correlation into a z-score. Missing values of z result in NA values.
#'   This is also called the hyperbolic tangent transformation (tanh), so this
#'   is a wrapper for the base::tanh() function.
#'
#' @return A numeric value (or vector) for the correlation coefficient(s).
#'
#' @references To be added later.
#'
#' @seealso \code{\link[base]{tanh}} for the base function used in this
#'   function. \code{\link{Fisher_r2z}} for the inverse transformation that
#'   converts r back to z.
#'
#' @examples
#' Fisher_z2r(.3)
#' Fisher_z2r(c(-Inf, -2.64, NA, 0, 0.55, 2.64, Inf))
#'
#' @export
Fisher_z2r <- function(z) {
  assert_that(all(is.na(z) | is.numeric(z)),
              msg = "z must be NA or numeric")
  r <- tanh(z)
  return(r)
}
