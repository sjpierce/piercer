#'=============================================================================
#' @name pseudoR2
#'
#' @title Compute pseudo-\eqn{R^2} for a logistic regression model
#'
#' @description Computes a simple pseudo-\eqn{R^2} measure for a logistic
#'   regression model.
#'
#' @param x A logistic regression model fit via glm(family = binomial).
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the result. Defaults to NULL, which does not round the result.
#'
#' @details This pseudo-R2 measure is just the Pearson correlation between the
#'   observed and fitted values from the logistic regression model, as discussed
#'   by Hosmer, Lemeshow, & Sturdivant (2013, p. 182).
#'
#' @return A numeric value for the pseudo-\eqn{R^2}.
#'
#' @references Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). Applied
#'   logistic regression (3rd ed.). Hoboken, NJ: John Wiley & Sons, Inc.
#'
#' @examples
#' m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#' pseudoR2(m1)
#' pseudoR2(m1, digits = 2)
#'
#' @export
pseudoR2 <- function(x, digits = NULL) {
  R2 <- stats::cor(x$y, y = stats::fitted(x), method = "pearson")^2
  if(!is.null(digits))  R2 <- round(R2, digits = digits)
  return(R2)
}

#'=============================================================================
#' @name R2Dev
#'
#' @title Compute R^2 for a GLM based on deviance residuals
#'
#' @description Computes an R^2 measure for a generalized linear model (GLM)
#'   that is based on deviance residuals, as discussed in Fox (1997, p. 481) and
#'   Cameron and Windmeijer (1997).
#'
#' @param x A model fit via glm().
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the result. Defaults to NULL, which does not round the result.
#'
#' @details This R^2 measure has a number of desirable properties
#'   and is also called R^2_KL (Cameron & Windmeijer, 1997). It is
#'   suitable for use with logistic regression and Poisson regression models.
#'
#' @return A numeric value for the \eqn{R^2}.
#'
#' @references Cameron, A. C., & Windmeijer, F. A. G. (1997). An R-squared
#'   measure of goodness of fit for some common nonlinear regression models.
#'   Journal of Econometrics, 77(2), 329-342. doi:10.1016/S0304-4076(96)01818-0
#'
#'   Fox, J. (1997). Applied regression analysis, linear models, and related
#'   methods. Thousand Oaks, CA: Sage Publications.
#'
#' @examples
#' m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#' R2Dev(m1)
#' R2Dev(m1, digits = 2)
#'
#' @export
R2Dev <- function(x, digits = NULL) {
  R2 <- 1 - (x$deviance/x$null.deviance)
  if(!is.null(digits))  R2 <- round(R2, digits = digits)
  return(R2)
}

#'=============================================================================
#' @name invlogit
#'
#' @title Inverse logit transformation
#'
#' @description This function performs the inverse logit transformation, which
#'   converts continuous values to the range (0, 1).
#'
#' @param x A vector of numeric values.
#'
#' @details The inverse logit transform is useful when you want to convert
#'   an estimate from the log odds (logit) scale back into a probability. That
#'   may happen when working with logistic regression models.
#'
#' @return A vector of estimated probabilities.
#'
#' @examples
#' invlogit(0)
#' round(invlogit(-7:7), 3)
#'
#' @export
invlogit <- function(x) {
  assertthat::assert_that(all(is.na(x) | is.numeric(x)),
                          msg = "x must be NA or numeric")
  return(1/(1 + exp(-x)))
}

#'=============================================================================
#' @name CookDco
#'
#' @title Compute a cutoff value for Cook's D values
#'
#' @description This function computes a cutoff for Cook's D values from
#'   a glm() model fit.
#'
#' @param x A glm() model fit object.
#'
#' @details Fox (1997, p. 281) suggested a cutoff value for identifying
#'   observations with high Cook's D values in a GLM model. This function
#'   computes that cutoff. Examining observations with Cook's D values larger
#'   than the cutoff may be warranted.
#'
#' @return A numeric value for the cutoff.
#'
#' @references Fox, J. (1997). Applied regression analysis, linear models, and
#'   related methods. Thousand Oaks, CA: Sage Publications.
#'
#' @examples
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  CookDco(m1)
#'
#' @export
CookDco <- function(x) {
  n  <- nrow(x$model)
  k  <- length(stats::coef(x))
  CO <- 4/(n - k - 1)
  return(CO)
}

#'=============================================================================
#' @name hatco
#'
#' @title Compute a cutoff value for leverage hat values
#'
#' @description This function computes a cutoff for leverage hat values from
#'   a glm() model fit.
#'
#' @param x A glm() model fit object.
#'
#' @details Fox (1997, p. 280) suggested a cutoff value for identifying
#'   observations with high leverage hat values in a GLM model. This function
#'   computes that cutoff. Examining observations with hat values larger than
#'   the cutoff may be warranted.
#'
#' @return A numeric value for the cutoff.
#'
#' @references Fox, J. (1997). Applied regression analysis, linear models, and
#'   related methods. Thousand Oaks, CA: Sage Publications.
#'
#' @examples
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  hatco(m1)
#'
#' @export
hatco <- function(x) {
  n  <- nrow(x$model)
  k  <- length(stats::coef(x))
  CO <- 2*((k + 1)/n)
  return(CO)
}
