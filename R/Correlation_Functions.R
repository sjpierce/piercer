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
#' @param conf.level A numeric value for the confidence level of the returned
#'   confidence interval, restricted to values between 0 and 1. Defaults to
#'   0.95.
#'
#' @param rn An optional character value to be used as a row name in the data
#'   frame returned by this function.
#'
#' @details This function implements the computations suggested by Raykov and
#'   Marcoulides (2011, p. 112) to obtain the upper and lower bounds of the
#'   approximate confidence interval. These are two-sided confidence intervals
#'   based on the z-distribution, with the p-value also based on the z-test
#'   statistic. Additional statistics are also computed, namely the p-value,
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
#' @importFrom mvtnorm rmvnorm
#'
#' @seealso \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @examples
#' library(mvtnorm)
#' library(polycor)
#' set.seed(45284)
#' # Create a population correlation matrix.
#' R <- matrix(0, 4, 4)
#' R[upper.tri(R)] <- c(.2, .3, .4, .5, .6, .7)
#' diag(R) <- 1
#' R <- cov2cor(t(R) %*% R)
#' # Show population correlations.
#' round(R, 4)
#' # Simulate data with normal distributions and correlation structure R.
#' mydf <- rmvnorm(1000, mean = rep(0, 4), sigma = R)
#' mydf <- data.frame(mydf)
#' names(mydf) <- c("x1", "x2", "y1", "y2")
#' # Show sample correlations.
#' Rhat <- round(cor(mydf), 4)
#' Rhat
#' # Convert y1 & y2  into ordinal categorical variables.
#' mydf$y1 <- cut(mydf$y1, c(-Inf, .75, Inf))
#' mydf$y2 <- cut(mydf$y2, c(-Inf, -1, .5, 1.5, Inf))
#' # Pearson, polychoric, and polyserial correlations, ML estimates.
#' HC <- hetcor(mydf, ML = TRUE)
#' HC
#'
#' # Polyserial correlation, x1 & y1
#' ci.rpc(r = HC$correlations[3,1], se = HC$std.errors[3,1], rn = "x1 & y1")
#'
#' # Polychoric correlation, y1 & y2
#' ci.rpc(r = HC$correlations[4,3], se = HC$std.errors[4,3], rn = "y1 & y2")
#'
#' @export
ci.rpc <- function(r, se, conf.level = 0.95, rn = NULL) {
  if(!is.null(rn)) {
    assert_that(is.finite(conf.level),
                msg = "conf.level must be a finite number between 0 and 1")
    assert_that(length(conf.level) == 1,
                msg = "conf.level must have only 1 value")
    assert_that(conf.level > 0 & conf.level < 1,
                msg = "conf.level must be between 0 and 1")
    assert_that(class(rn) == "character",
                msg = "If present, rn must be a character value")
    assert_that(length(rn) == 1,
                msg = "If present, rn must have only 1 value")
  }
  # Apply Fisher's r to z transform
  z       <- Fisher_r2z(r) # correlation after Fisher's z-transform
  sez     <- se/(1 - r^2)  # se after Fisher's z-transform
  # CI limits after back-transformation from z-scores back to r.
  ci_lo   <- Fisher_z2r(z - qnorm((1 + conf.level)/2)*sez)
  ci_up   <- Fisher_z2r(z + qnorm((1 + conf.level)/2)*sez)
  # Calculate a z statistic from which to get a p-value for H0: r = 0.
  ztest <- r/se
  # Next convert that z test statistic to a lower-tail p-value
  p <- pnorm(q = ztest)
  # Calculate a two-sided p-value, plus additional measures.
  Pval <- 2*min(p, 1 - p)
  Sval <- p2s(Pval)
  BFB  <- p2bfb(Pval)
  PPH1 <- p2pp(Pval)
  # Combine results into a data frame for nice printing.
  ci = data.frame(Cor = r, SE = se, CI.LL = ci_lo, CI.UL = ci_up,
                  Z = ztest, Pval, Sval, BFB, PPH1)
  row.names(ci) <- rn
  return(ci)
}

#'=============================================================================
#' @name ci.rp
#'
#' @title Compute confidence intervals for Pearson correlations.
#'
#' @description This function computes confidence intervals for Pearson
#'   correlations using the Fisher's z-transformation.
#'
#' @param r A numeric value for the correlation coefficient.
#'
#' @param se A numeric value for the standard error of the correlation
#'   coefficient.
#'
#' @param conf.level A numeric value for the confidence level of the returned
#'   confidence interval, restricted to values between 0 and 1. Defaults to
#'   0.95.
#'
#' @param n A numeric value for the sample size.
#'
#' @param rn An optional character value to be used as a row name in the data
#'   frame returned by this function.
#'
#' @details This function computes the upper and lower bounds of an approximate,
#'   two-sided confidence interval based on the z-distribution, with the p-value
#'   also based on the z-test statistic. Additional statistics are also
#'   computed, namely the p-value, s-value, BFB, and posterior probability of
#'   H1.
#'
#' @return A data frame containing the correlation coefficient, standard error,
#'   confidence interval limits, z-statistic, p-value, s-value, BFB, and
#'   posterior probability of H1.
#'
#' @importFrom assertthat assert_that
#' @importFrom stats pnorm
#' @importFrom mvtnorm rmvnorm
#'
#' @seealso \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @examples
#' library(mvtnorm)
#' library(polycor)
#' set.seed(45284)
#' # Create a population correlation matrix.
#' R <- matrix(0, 4, 4)
#' R[upper.tri(R)] <- c(.2, .3, .4, .5, .6, .7)
#' diag(R) <- 1
#' R <- cov2cor(t(R) %*% R)
#' # Show population correlations.
#' round(R, 4)
#' # Simulate data with normal distributions and correlation structure R.
#' mydf <- rmvnorm(1000, mean = rep(0, 4), sigma = R)
#' mydf <- data.frame(mydf)
#' names(mydf) <- c("x1", "x2", "y1", "y2")
#' # Show sample correlations.
#' Rhat <- round(cor(mydf), 4)
#' Rhat
#' # Convert y1 & y2  into ordinal categorical variables.
#' mydf$y1 <- cut(mydf$y1, c(-Inf, .75, Inf))
#' mydf$y2 <- cut(mydf$y2, c(-Inf, -1, .5, 1.5, Inf))
#' # Pearson, polychoric, and polyserial correlations, ML estimates.
#' HC <- hetcor(mydf, ML = TRUE)
#' HC
#'
#' # Pearson correlation, x1 & y1
#' ci.rp(r = HC$correlations[2,1], se = HC$std.errors[2,1],
#'       n = HC$n, rn = "x1 & x2")
#'
#' @export
ci.rp <- function(r, se, n, conf.level = 0.95, rn = NULL) {
  if(!is.null(rn)) {
    assert_that(n >= 3,
                msg = "Not enough observations (n >= 3 required)")
    assert_that(is.finite(conf.level),
                msg = "conf.level must be a finite number between 0 and 1")
    assert_that(length(conf.level) == 1,
                msg = "conf.level must have only 1 value")
    assert_that(conf.level > 0 & conf.level < 1,
                msg = "conf.level must be between 0 and 1")
    assert_that(class(rn) == "character",
                msg = "If present, rn must be a character value")
    assert_that(length(rn) == 1,
                msg = "If present, rn must have only 1 value")
  }
  # Apply Fisher's r to z transform
  z       <- Fisher_r2z(r)  # correlation after Fisher's z-transform
  sez     <- 1/sqrt(n - 3)  # se after Fisher's z-transform
  # CI limits after back-transformation from z-scores back to r.
  ci_lo   <- Fisher_z2r(z - qnorm((1 + conf.level)/2)*sez)
  ci_up   <- Fisher_z2r(z + qnorm((1 + conf.level)/2)*sez)
  # Calculate a z statistic from which to get a p-value for H0: r = 0.
  ztest <- r/se
  # Next convert that z test statistic to a lower-tail p-value
  p <- pnorm(q = ztest)
  # Calculate a two-sided p-value, plus additional measures.
  Pval <- 2*min(p, 1 - p)
  Sval <- p2s(Pval)
  BFB  <- p2bfb(Pval)
  PPH1 <- p2pp(Pval)
  # Combine results into a data frame for nice printing.
  ci = data.frame(Cor = r, SE = se, CI.LL = ci_lo, CI.UL = ci_up,
                  Z = ztest, Pval, Sval, BFB, PPH1)
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
#' @importFrom mvtnorm rmvnorm
#'
#' @seealso \code{\link{ci.rpc}} for the function used to get the CIs,
#'   \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @examples
#' library(mvtnorm)
#' library(polycor)
#' set.seed(63247)
#' # Create a population correlation matrix.
#' R <- matrix(0, 4, 4)
#' R[upper.tri(R)] <- c(.2, .3, .4, .5, .6, .7)
#' diag(R) <- 1
#' R <- cov2cor(t(R) %*% R)
#' # Show population correlations.
#' round(R, 4)
#' # Simulate data with normal distributions and correlation structure R.
#' mydf <- rmvnorm(1000, mean = rep(0, 4), sigma = R)
#' mydf <- data.frame(mydf)
#' names(mydf) <- c("x1", "x2", "y1", "y2")
#' # Show sample correlations.
#' Rhat <- round(cor(mydf), 4)
#' Rhat
#' # Convert y1 & y2  into ordinal categorical variables.
#' mydf$y1 <- cut(mydf$y1, c(-Inf, .75, Inf))
#' mydf$y2 <- cut(mydf$y2, c(-Inf, -1, .5, 1.5, Inf))
#' # Pearson, polychoric, and polyserial correlations, ML estimates.
#' HC <- hetcor(mydf, ML = TRUE)
#' HC
#'
#' # Polyserial correlation, x2 & y2
#' r.ps(x = HC, cont = "x2", ord = "y2")
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
#' @description This function computes confidence intervals for polychoric
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
#' @param pdigits An integer specifying the number of decimal places to used
#'   when rounding the p-value. Defaults to NULL, which does not round the
#'   result.
#'
#' @details This function applies ci.rpc() to all the polyserial correlations
#'   in the hetcor object supplied by the user.
#'
#' @return A data frame containing the results.
#'
#' @references Raykov, T., & Marcoulides, G. A. (2011). Introduction to
#'   psychometric theory. New York, NY: Routledge.
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats qnorm
#'
#' @seealso \code{\link{ci.rpc}} for the function used to get the CIs,
#'   \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @examples
#' library(mvtnorm)
#' library(polycor)
#' set.seed(12475)
#' # Create a population correlation matrix.
#' R <- matrix(0, 4, 4)
#' R[upper.tri(R)] <- c(.2, .3, .4, .5, .6, .7)
#' diag(R) <- 1
#' R <- cov2cor(t(R) %*% R)
#' # Show population correlations.
#' round(R, 4)
#' # Simulate data with normal distributions and correlation structure R.
#' mydf <- rmvnorm(1000, mean = rep(0, 4), sigma = R)
#' mydf <- data.frame(mydf)
#' names(mydf) <- c("x1", "x2", "y1", "y2")
#' # Show sample correlations.
#' Rhat <- round(cor(mydf), 4)
#' Rhat
#' # Convert y1 & y2  into ordinal categorical variables.
#' mydf$y1 <- cut(mydf$y1, c(-Inf, .75, Inf))
#' mydf$y2 <- cut(mydf$y2, c(-Inf, -1, .5, 1.5, Inf))
#' # Pearson, polychoric, and polyserial correlations, ML estimates.
#' HC <- hetcor(mydf, ML = TRUE)
#' HC
#'
#' # Polychoric correlation, y1 & y2
#' r.pc(x = HC, ord = c("y1", "y2"))
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

#'=============================================================================
#' @name r.p
#'
#' @title Compute confidence intervals for a set of Pearson correlations.
#'
#' @description This function computes confidence intervals for Pearson
#'   correlations obtained from a hetcor object.
#'
#' @param x A hetcor object produced by hetcor().
#'
#' @param cont A character vector of names for ordinal variables.
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the correlation, SE, CI bounds, z-statistic, s-value, BFB, and
#'   posterior probability. Defaults to NULL, which does not round the result.
#'
#' @param pdigits An integer specifying the number of decimal places to used
#'   when rounding the p-value. Defaults to NULL, which does not round the
#'   result.
#'
#' @details This function applies ci.rp() to all the Pearson correlations
#'   in the hetcor object supplied by the user.
#'
#' @return A data frame containing the results.
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats qnorm
#'
#' @seealso \code{\link{ci.rp}} for the function used to get the CIs,
#'   \code{\link{p2s}} for s-values, \code{\link{p2bfb}} for BFBs, and
#'   \code{\link{p2pp}} for posterior probabilities.
#'
#' @examples
#' library(mvtnorm)
#' library(polycor)
#' set.seed(12475)
#' # Create a population correlation matrix.
#' R <- matrix(0, 4, 4)
#' R[upper.tri(R)] <- c(.2, .3, .4, .5, .6, .7)
#' diag(R) <- 1
#' R <- cov2cor(t(R) %*% R)
#' # Show population correlations.
#' round(R, 4)
#' # Simulate data with normal distributions and correlation structure R.
#' mydf <- rmvnorm(1000, mean = rep(0, 4), sigma = R)
#' mydf <- data.frame(mydf)
#' names(mydf) <- c("x1", "x2", "y1", "y2")
#' # Show sample correlations.
#' Rhat <- round(cor(mydf), 4)
#' Rhat
#' # Convert y1 & y2  into ordinal categorical variables.
#' mydf$y1 <- cut(mydf$y1, c(-Inf, .75, Inf))
#' mydf$y2 <- cut(mydf$y2, c(-Inf, -1, .5, 1.5, Inf))
#' # Pearson, polychoric, and polyserial correlations, ML estimates.
#' HC <- hetcor(mydf, ML = TRUE)
#' HC
#'
#' # Pearson correlation, x1 & x2
#' r.p(x = HC, cont = c("x1", "x2"))
#'
#' @export
r.p <- function(x, cont, digits = NULL, pdigits = NULL) {
  assert_that(class(x) == "hetcor",
              msg = "x must be hetcor object (see polycor pacakage)")
  assert_that(class(ord) == "character",
              msg = "cont must be character vector")
  assert_that(length(ord) >= 2,
              msg = "cont vector must have at least two values")
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
  k   <- length(cont)
  # Create a k*k matrix of all 0s
  Mat <- matrix(0, nrow = k, ncol = k, dimnames = list(cont, cont))
  # Set Mat's lower triangle to all 1s
  Mat[lower.tri(Mat)] <- 1
  res <- data.frame()
  for(i in cont) {
    for (j in cont){
      if(Mat[j, i] == 1) {
        res <- rbind(res,
                     ci.rp(r = x$correlations[i, j],
                           se = x$std.errors[i, j],
                           n = x$n,
                           rn = paste(i, "and", j, sep = " ")))
      }
    }
  }
  vars <- c("Cor", "SE", "CI.LL", "CI.UL", "Z", "Sval", "BFB", "PPH1")
  if(!is.null(digits)) res[, vars] <- round(res[, vars], digits = digits)
  if(!is.null(pdigits)) res$Pval   <- round(res$Pval, digits = pdigits)
  return(res)
}
