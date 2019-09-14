#' @name p2s
#'
#' @title Convert p-values into s-values
#'
#' @description This function converts p-values into Shannon information values
#'   (s-values).
#'
#' @param p A numeric p-value (or numeric vector of p-values), obtained from
#'   running a statistical model or test. All values should be proportions
#'   within the closed unit interval (0 <= p <= 1).
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the s-value. Defaults to NULL, which does not round the result.
#'
#' @details S-values are a rescaling of p-values that measure the information
#'   against the null hypothesis of a statistical test, as measured in bits.
#'   They are discussed in Greenland (2019) and Wasserstein, Schirm, and Lazar
#'   (2019). S-values fall in the range [0, Inf]. When p = 0, s = Inf and when
#'   p = 1, s = 0.
#'
#' @return A numeric vector of s-values of the same length as p.
#'
#' @references Greenland, S. (2019). Valid p-values behave exactly as they
#'   should: Some misleading criticisms of p-values and their resolution with
#'   s-values. The American Statistician, 73(Supplement 1), 106-114.
#'   doi:10.1080/00031305.2018.1529625
#'
#'   Wasserstein, R. L., Schirm, A. L., & Lazar, N. A. (2019). Moving to a world
#'   beyond "p < .05". The American Statistician, 73(Supplement 1), 1-19.
#'   doi:10.1080/00031305.2019.1583913
#'
#' @examples
#' p2s(p = .25)
#' p2s(p = c(.0001, .46))
#' p2s(p = .0001, digits = 1)
#' p2s(p = NA) # Missing p returns a missing value.
#'
#' @export
p2s <- function(p, digits = NULL) {
  assertthat::assert_that(all(is.na(p) | is.numeric(p)),
                          msg = "p must be NA or numeric")
  assertthat::assert_that(all(is.na(p) | (p >= 0 & p <= 1)),
                          msg = "Numeric p must be a proportion between 0 and 1")
  if(!is.null(digits)) {
    assertthat::assert_that(assertthat::is.number(digits),
                            msg = "If present, digits must be a scalar numeric/integer value")
    assertthat::assert_that(digits%%1 == 0,
                            msg = "If present, digits must be a whole number")
  }
    s <- -log2(p)
  if(!is.null(digits))  s <- round(s, digits = digits)
  return(s)
}

#'=============================================================================
#' @name p2bfb
#'
#' @title Convert p-values into Bayes Factor Bounds (BFBs)
#'
#' @description This function converts p-values into Bayes Factor Bounds (BFBs),
#'   which are the data-based odds of the alternative hypotheis H1 being true to
#'   the null hypothesis H0 being true and the upper bound on the Bayes Factor.
#'
#' @param p A numeric p-value (or numeric vector of p-values), obtained from
#'   running a statistical model or test. All values should be proportions
#'   within the closed unit interval (0 <= p <= 1).
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the BFB. Defaults to NULL, which does not round the result.
#'
#' @details Bayes Factor Bounds (BFBs)  are the data-based odds of the
#'   alternative hypotheis H1 being true to the null hypothesis H0 being true
#'   (Benjamin & Berger, 2019). They are the upper bound on the Bayes Factor
#'   for a given test. The BFBs are most useful when the p-value lies in the
#'   open unit interval (0 < p < 1). Plugging in p = 0 or p = 1 will return
#'   NaN or -Inf, respectively.
#'
#' @return A numeric vector of BFB values of the same length as p.
#'
#' @references Benjamin, D. J., & Berger, J. O. (2019). Three recommendations
#'   for improving the use of p-values. The American Statistician, 73
#'   (Supplement 1), 186-191. doi:10.1080/00031305.2018.1543135
#'
#'   Wasserstein, R. L., Schirm, A. L., & Lazar, N. A. (2019). Moving to a world
#'   beyond "p < .05". The American Statistician, 73(Supplement 1), 1-19.
#'   doi:10.1080/00031305.2019.1583913
#'
#' @examples
#' p2bfb(.05)
#' p2bfb(c(.1, .05, .01, .005, .001, .0001, .00001), digits = 2)
#' p2bfb(0)
#' p2bfb(p = NA) # Missing p returns a missing value.
#'
#' @export
p2bfb <- function(p, digits = NULL){
  assertthat::assert_that(all(is.na(p) | is.numeric(p)),
                          msg = "p must be NA or numeric")
  assertthat::assert_that(all(is.na(p) | (p >= 0 & p <= 1)),
                          msg = "Numeric p must be a proportion between 0 and 1")
  if(!is.null(digits)) {
    assertthat::assert_that(assertthat::is.number(digits),
                            msg = "If present, digits must be a scalar numeric/integer value")
    assertthat::assert_that(digits%%1 == 0,
                            msg = "If present, digits must be a whole number")
  }
  e   <- exp(1) # Base for natural logarithms
  bfb <- 1/(-e*p*log(p))
  if(!is.null(digits))  bfb <- round(bfb, digits = digits)
  return(bfb)
}

#'=============================================================================
#' @name p2pp
#'
#' @title Convert p-values into posterior probabilities that H1 is true
#'
#' @description This function converts p-values into the upper bounds for
#'   posterior probability that the alternative hypotheis H1 is true, under the
#'   assumption of equal prior probabilities for H0 and H1.
#'
#' @param p A numeric p-value (or numeric vector of p-values), obtained from
#'   running a statistical model or test. All values should be proportions
#'   within the closed unit interval (0 <= p <= 1).
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the posterior probabilities. Defaults to NULL, which does not
#'   round the result.
#'
#' @details The upper bound for the posterior probability that the alternative
#'   hypothesis H1 is true can be obtained from a p-value with a simple
#'   formula when the prior probabilities of the null and alternative
#'   hypotheses (H0 and H1, respectively) are equal:
#'   PrU (H1 | p) =  BFB/(1 + BFB). The posterior probability is a measure of the
#'   evidence against the null hypothesis. When the p-value is zero or one, the
#'   result is returned as NaN (not a number).
#'
#' @return A numeric vector of posterior probability values of the same length
#'   as p.
#'
#' @references Benjamin, D. J., & Berger, J. O. (2019). Three recommendations
#'   for improving the use of p-values. The American Statistician, 73
#'   (Supplement 1), 186-191. doi:10.1080/00031305.2018.1543135
#'
#'   Wasserstein, R. L., Schirm, A. L., & Lazar, N. A. (2019). Moving to a world
#'   beyond "p < .05". The American Statistician, 73(Supplement 1), 1-19.
#'   doi:10.1080/00031305.2019.1583913
#'
#' @examples
#' p2pp(.05)
#' p2pp(c(.1, .05, .01, .005, .001, .0001, .00001), digits = 2)
#' p2pp(0)
#' p2pp(p = NA) # Missing p returns a missing value.
#'
#' @export
p2pp <- function(p, digits = NULL){
  assertthat::assert_that(all(is.na(p) | is.numeric(p)),
                          msg = "p must be NA or numeric")
  assertthat::assert_that(all(is.na(p) | (p >= 0 & p <= 1)),
                          msg = "Numeric p must be a proportion between 0 and 1")
  if(!is.null(digits)) {
    assertthat::assert_that(assertthat::is.number(digits),
                            msg = "If present, digits must be a scalar numeric/integer value")
    assertthat::assert_that(digits%%1 == 0,
                            msg = "If present, digits must be a whole number")
  }
  bfb <- p2bfb(p)
  pp  <- bfb/(1 + bfb)
  if(!is.null(digits))  pp <- round(pp, digits = digits)
  return(pp)
}

#'=============================================================================
#' @name convertp
#'
#' @title Convert p-values into s-values, Bayes Factor Bounds, and posterior
#'   probabilities that H1 is true
#'
#' @description This function converts p-values into three different measures:
#'   s-values, Bayes Factor Bounds (BFBs), and posterior probabilities that the
#'   alternative hypotheis H1 is true.
#'
#' @param p A numeric p-value (or numeric vector of p-values), obtained from
#'   running a statistical model or test. All values should be proportions
#'   within the closed unit interval (0 <= p <= 1).
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the results. Defaults to NULL, which does not round the results.
#'
#' @details This function applies \code{\link{p2s}}, \code{\link{p2bfb}}, and
#'   \code{\link{p2pp}} to the p-values supplied. It is a convenient way to get
#'   all three results with a single function call.
#'
#' @return A data frame containing three numeric columns values, each the same
#'   length as p: s-value, BFB, and posterior probability.
#'
#' @references Benjamin, D. J., & Berger, J. O. (2019). Three recommendations
#'   for improving the use of p-values. The American Statistician, 73
#'   (Supplement 1), 186-191. doi:10.1080/00031305.2018.1543135
#'
#'   Greenland, S. (2019). Valid p-values behave exactly as they
#'   should: Some misleading criticisms of p-values and their resolution with
#'   s-values. The American Statistician, 73(Supplement 1), 106-114.
#'   doi:10.1080/00031305.2018.1529625
#'
#'   Wasserstein, R. L., Schirm, A. L., & Lazar, N. A. (2019). Moving to a world
#'   beyond "p < .05". The American Statistician, 73(Supplement 1), 1-19.
#'   doi:10.1080/00031305.2019.1583913
#'
#' @seealso \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @examples
#' convertp(.05)
#' convertp(c(.1, .05, .01, .005, .001, .0001, .00001), digits = 2)
#' convertp(p = NA) # Missing p returns a missing value.
#'
#' @export
convertp <- function(p, digits = NULL){
  assertthat::assert_that(all(is.na(p) | is.numeric(p)),
                          msg = "p must be NA or numeric")
  assertthat::assert_that(all(is.na(p) | (p >= 0 & p <= 1)),
                          msg = "Numeric p must be a proportion between 0 and 1")
  if(!is.null(digits)) {
    assertthat::assert_that(assertthat::is.number(digits),
                            msg = "If present, digits must be a scalar numeric/integer value")
    assertthat::assert_that(digits%%1 == 0,
                            msg = "If present, digits must be a whole number")
  }
  s   <- p2s(p, digits = digits)
  bfb <- p2bfb(p, digits = digits)
  pp  <- p2pp(p, digits = digits)
  res <- data.frame(S = s, BFB = bfb, PPH1 = pp)
  return(res)
}
