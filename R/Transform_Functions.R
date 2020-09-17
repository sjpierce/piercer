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

#'=============================================================================
#' @name p2odds
#'
#' @title Convert from probability to odds.
#'
#' @description This function converts probability values into odds values.
#'
#' @param p A numeric value (or vector) for the probabilities to be converted.
#'   NA values are acceptable, but will return NA. All numeric values must fall
#'   in the closed unit interval, [0, 1].
#'
#' @details This function uses the formula odds = p/(1 - p).
#'
#' @return A numeric value (or vector) for the odds values.
#'
#' @references To be added later.
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' p2odds(.3)
#' p2odds(c(0, .1, .5, .75, .99, 1))
#'
#' @export
p2odds <- function(p) {
  assert_that(all(is.na(p) | is.numeric(p)),
              msg = "All values of p must be either NA or numeric")
  assert_that(all(is.na(p) | (p >= 0 & p <= 1)),
              msg = "All non-missing values of p must be in the range [0, 1]")
    odds <- p/(1 - p)
  return(odds)
}

#'=============================================================================
#' @name p2or
#'
#' @title Convert from a pair of probabilities to an odds ratio.
#'
#' @description This function converts a pair of probability values into an odds
#'   ratio.
#'
#' @param p0 A numeric value (or vector) for the probabilities of success in
#'   the reference group or category (i.e., the denominator for the odds ratio).
#'   NA values are acceptable, but will return NA. All numeric values must fall
#'   in the closed unit interval, [0, 1].
#'
#' @param p1 A numeric value (or vector) for the probabilities of success in
#'   the focal group or category (i.e., the numerator for the odds ratio).
#'   NA values are acceptable, but will return NA. All numeric values must fall
#'   in the closed unit interval, [0, 1].
#'
#' @details This function uses the formula OR = (p1/(1 - p1))/(p0/(1 - p0)).
#'
#' @return A numeric value (or vector) for the odds ratio values.
#'
#' @references To be added later.
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' p2or(p0 = .3, p1 = .6)
#' p2or(p0 = c(.3, .4), p1 = c(.6, .5))
#'
#' @export
p2or <- function(p0, p1) {
  assert_that(all(is.na(p0) | is.numeric(p0)),
              msg = "All values of p0 must be either NA or numeric")
  assert_that(all(is.na(p0) | (p0 >= 0 & p0 <= 1)),
              msg = "All non-missing values of p0 must be in the range [0, 1]")
  assert_that(all(is.na(p1) | is.numeric(p1)),
              msg = "All values of p1 must be either NA or numeric")
  assert_that(all(is.na(p1) | (p1 >= 0 & p1 <= 1)),
              msg = "All non-missing values of p1 must be in the range [0, 1]")
  oratio <- p2odds(p1)/p2odds(p0)
  return(oratio)
}
