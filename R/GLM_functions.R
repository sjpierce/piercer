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
  R2 <- cor(x$y, y = fitted(x), method = "pearson")^2
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
