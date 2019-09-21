#' @name ci.rpc
#'
#' @title Compute confidence intervals for polychoric and polyserial
#'   correlations.
#'
#' @description This function compute confidence intervals for polychoric and
#'   polyserial correlations using the Fisher's z-transformation.
#'
#' @param r A numeric value for the correlation coefficient.
#'
#' @param se A numeric value for the standard error of the correlation
#'   coefficient.
#'
#' @param rn An optional character value to be used as a row name in the data
#'   frame returned by this function.
#'
#' @details This function implements the computations suggested by Raykov and
#'   Marcoulides (2011, p. 112) to obtain the upper and lower bounds of the
#'   confidence interval. Additional statistics are also computed, namely the
#'   s-value, BFB, and posterior probability of H1.
#'
#' @return A data frame containing the correlation coefficient, standard error,
#'   confidence interval limits, z-statistic, p-value, s-value, BFB, and
#'   posterior probability of H1.
#'
#' @references Raykov, T., & Marcoulides, G. A. (2011). Introduction to
#'   psychometric theory. New York, NY: Routledge.
#'
#' @importFrom assertthat assert_that
#' @importFrom stats pnorm
#'
#' @seealso \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @export
ci.rpc <- function(r, se, rn = NULL) {
  if(!is.null(rn)) {
    assert_that(class(rn) == "character",
                msg = "If present, rn must be a character value")
    assert_that(length(rn) == 1,
                msg = "If present, rn must have only 1 value")
  }
  z       <- .5*log((1 + r)/(1 - r))  # correlation after Fisher's z-transform
  sez     <- se/((1 - r^2))           # se after Fisher's z-transform
  ci_z_lo <- z - 1.96*sez             # CIs after Fisher's z transform
  ci_z_up <- z + 1.96*sez
  ci_lo   <- (exp(2*ci_z_lo) - 1)/(exp(2*ci_z_lo) + 1)
  ci_up   <- (exp(2*ci_z_up) - 1)/(exp(2*ci_z_up) + 1)
  # Below here modified by Steven J. Pierce
  # First calculate a Z statistic from which to get a p-value for H0: r = 0.
  Z <- r/se
  # Next convert that Z statistic to a lower-tail p-value
  p <- pnorm(q = Z)
  # Calculate a two-sided p-value, plus an s-value
  Pval <- 2*min(p, 1 - p)
  Sval <- p2s(Pval)
  BFB  <- p2bfb(Pval)
  PPH1 <- p2pp(Pval)
  # Combine results into a data frame for nice printing.
  ci = data.frame(Cor = r, SE = se, CI.LL = ci_lo, CI.UL = ci_up,
                  Z, Pval, Sval, BFB, PPH1)
  row.names(ci) <- rn
  return(ci)
}

#'=============================================================================
#' @name r.ps
#'
#' @title Compute confidence intervals for a set of polyserial correlations.
#'
#' @description This function computes confidence intervals for polyserial
#'   correlations obtained from a hetcor object.
#'
#' @param x A hetcor object produced by hetcor().
#'
#' @param cont A character vector of names for continuous variables.
#'
#' @param ord A character vector of names for ordinal variables.
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the correlation, SE, CI bounds, z-statistic, s-value, BFB, and
#'   posterior probability. Defaults to NULL, which does not round the result.
#'
#' @param pdigits An integer specifying the number of decimal places to used when
#'   rounding the p-value. Defaults to NULL, which does not round the result.
#'
#' @details This function applies ci.pc() to all the polyserial correlations
#'   in the hetcor object supplied by the user.
#'
#' @return A data frame containing the results.
#'
#' @references Raykov, T., & Marcoulides, G. A. (2011). Introduction to
#'   psychometric theory. New York, NY: Routledge.
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#'
#' @seealso \code{\link{ci.rpc}} for the function used to get the CIs,
#'   \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @export
r.ps <- function(x, cont, ord, digits = NULL, pdigits = NULL) {
  assert_that(class(x) == "hetcor",
              msg = "x must be hetcor object (see polycor package)")
  assert_that(class(cont) == "character",
              msg = "cont must be character vector")
  assert_that(class(ord) == "character",
              msg = "ord vector must have at least two values")
  if(!is.null(digits)) {
    assert_that(is.number(digits),
                msg = "If present, digits must be a scalar numeric/integer value")
    assert_that(digits%%1 == 0,
                msg = "If present, digits must be a whole number")
  }
  if(!is.null(pdigits)) {
    assert_that(is.number(pdigits),
                msg = "If present, pdigits must be a scalar numeric/integer value")
    assert_that(pdigits%%1 == 0,
                msg = "If present, pdigits must be a whole number")
  }
  res <- data.frame()
  for(i in cont) {
    for (j in ord){
      res <- rbind(res,
                   ci.rpc(r = x$correlations[i, j],
                          se = x$std.errors[i, j],
                          rn = paste(i, "and", j, sep = " ")))
    }
  }
  vars <- c("Cor", "SE", "CI.LL", "CI.UL", "Z", "Sval", "BFB", "PPH1")
  if(!is.null(digits)) res[, vars] <- round(res[, vars], digits = digits)
  if(!is.null(pdigits)) res$Pval   <- round(res$Pval, digits = pdigits)
  return(res)
}

#'=============================================================================
#' @name r.pc
#'
#' @title Compute confidence intervals for a set of polychoric correlations.
#'
#' @description This function computes confidence intervals for polychoricvvvvvv
#'   correlations obtained from a hetcor object.
#'
#' @param x A hetcor object produced by hetcor().
#'
#' @param ord A character vector of names for ordinal variables.
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the correlation, SE, CI bounds, z-statistic, s-value, BFB, and
#'   posterior probability. Defaults to NULL, which does not round the result.
#'
#' @param pdigits An integer specifying the number of decimal places to used when
#'   rounding the p-value. Defaults to NULL, which does not round the result.
#'
#' @details This function applies ci.pc() to all the polyserial correlations
#'   in the hetcor object supplied by the user.
#'
#' @return A data frame containing the results.
#'
#' @references Raykov, T., & Marcoulides, G. A. (2011). Introduction to
#'   psychometric theory. New York, NY: Routledge.
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#'
#' @seealso \code{\link{ci.rpc}} for the function used to get the CIs,
#'   \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @export
r.pc <- function(x, ord, digits = NULL, pdigits = NULL) {
  assert_that(class(x) == "hetcor",
              msg = "x must be hetcor object (see polycor pacakage)")
  assert_that(class(ord) == "character",
              msg = "ord must be character vector")
  assert_that(length(ord) >= 2,
              msg = "ord vector must have at least two values")
  if(!is.null(digits)) {
    assert_that(is.number(digits),
                msg = "If present, digits must be a scalar numeric/integer value")
    assert_that(digits%%1 == 0,
                msg = "If present, digits must be a whole number")
  }
  if(!is.null(pdigits)) {
    assert_that(is.number(pdigits),
                msg = "If present, pdigits must be a scalar numeric/integer value")
    assert_that(pdigits%%1 == 0,
                msg = "If present, pdigits must be a whole number")
  }
  k   <- length(ord)
  # Create a k*k matrix of all 0s
  Mat <- matrix(0, nrow = k, ncol = k, dimnames = list(ord, ord))
  # Set Mat's lower triangle to all 1s
  Mat[lower.tri(Mat)] <- 1
  res <- data.frame()
  for(i in ord) {
    for (j in ord){
      if(Mat[j, i] == 1) {
        res <- rbind(res,
                     ci.rpc(r = x$correlations[i, j],
                            se = x$std.errors[i, j],
                            rn = paste(i, "and", j, sep = " ")))
      }
    }
  }
  vars <- c("Cor", "SE", "CI.LL", "CI.UL", "Z", "Sval", "BFB", "PPH1")
  if(!is.null(digits)) res[, vars] <- round(res[, vars], digits = digits)
  if(!is.null(pdigits)) res$Pval   <- round(res$Pval, digits = pdigits)
  return(res)
}

