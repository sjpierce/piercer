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

#'=============================================================================
#' @name PlotCookD
#'
#' @title Plot Cook's D values
#'
#' @description This function plots Cook's D values from a glm() model fit
#'   against the observation index, and highlights observations with values
#'   exceeding a recommended cutoff.
#'
#' @param x A glm() model fit object.
#'
#' @param id.n An integer specifying how many observations to label.
#'
#' @details Fox (1997, p. 280) suggested a cutoff value for identifying
#'   observations with high Cook's D values in a GLM model. Plotting the Cook's
#'   D values against the observation number (row index) and highlighting the
#'   values that exceed the cutoff is a quick way to inspect a model fit.
#'
#' @return None. This produces a plot via ggplot().
#'
#' @references Fox, J. (1997). Applied regression analysis, linear models, and
#'   related methods. Thousand Oaks, CA: Sage Publications.
#'
#' @examples
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  PlotCookD(m1)
#'
#' @export
PlotCookD <- function(x, id.n = 10) {
  DF       <- data.frame(CookD = stats::cooks.distance(x))
  DF$RN    <- as.numeric(rownames(DF))
  DF$group <- DF$CookD > CookDco(x)
  ggplot2::ggplot(DF,
                  ggplot2::aes(DF$RN, DF$CookD, color=DF$group, group=DF$group,
                               ggplot2::aes(DF$RN, DF$CookD))) +
    ggplot2::annotate(geom = "text", x = 0, y = max(DF$CookD), color = "black",
                      hjust = 0, vjust = 0, size = 4,
                      label = paste("Cutoff >", round(CookDco(x), digits = 3))) +
    ggplot2::geom_segment(ggplot2::aes(DF$RN, xend=DF$RN, 0, yend=DF$CookD,
                                       color=DF$group),
                          data=DF) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values=c("black", "red")) +
    ggplot2::geom_hline(yintercept = CookDco(x), color = "black", linetype = 2) +
    ggplot2::ylab("Cook's Distance") +
    ggplot2::xlab("Row Name") +
    ggplot2::theme(legend.position = "none")
}

#'=============================================================================
#' @name PlotHat
#'
#' @title Plot leverage hat values
#'
#' @description This function plots leverage hat values from a glm() model fit
#'   against the observation index, and highlights observations with values
#'   exceeding a recommended cutoff.
#'
#' @param x A glm() model fit object.
#'
#' @param id.n An integer specifying how many observations to label.
#'
#' @details Fox (1997, p. 281) suggested a cutoff value for identifying
#'   observations with high leverage hat values in a GLM model. Plotting the hat
#'   values against the observation number (row index) and highlighting the
#'   values that exceed the cutoff is a quick way to inspect a model fit.
#'
#' @return None. This produces a plot via ggplot().
#'
#' @references Fox, J. (1997). Applied regression analysis, linear models, and
#'   related methods. Thousand Oaks, CA: Sage Publications.
#'
#' @examples
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  PlotHat(m1)
#'
#' @export
PlotHat <- function(x, id.n = 10) {
  DF       <- data.frame(Hat = stats::hatvalues(x))
  DF$RN    <- as.numeric(rownames(DF))
  DF$group <- DF$Hat > hatco(x)
  ggplot2::ggplot(DF, ggplot2::aes(DF$RN, DF$Hat, color=DF$group, group=DF$group,
                                   ggplot2::aes(DF$RN, DF$Hat))) +
    ggplot2::annotate(geom = "text", x = 0, y = max(DF$Hat), color = "black",
                      hjust = 0, vjust = 0, size = 4,
                      label = paste("Cutoff >", round(hatco(x), digits = 3))) +
    ggplot2::geom_segment(ggplot2::aes(DF$RN, xend=DF$RN, 0, yend=DF$Hat,
                                       color=DF$group),
                          data=DF)  +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values=c("black", "red")) +
    ggplot2::geom_hline(yintercept = hatco(x), color = "black", linetype = 2) +
    ggplot2::ylab("Leverage (hat)") +
    ggplot2::xlab("Row Name") +
    ggplot2::theme(legend.position = "none")
}

#'=============================================================================
#' @name InfCases
#'
#' @title Identify influential cases (high leverage and high Cook's D)
#'
#' @description This function identifies influential cases from a glm() model
#'   by finding those that exceed cutoffs for high leverage and high Cook's D.
#'
#' @param x A glm() model fit object.
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the result. Defaults to 3.
#'
#' @details Fox (1997, p. 280-281) suggested cutoff values for identifying
#'   observations with high leverage hat values and high Cook's D values in a
#'   GLM model. Listing these influential observations is a quick way to inspect
#'   a model fit.
#'
#' @return A data frame showing observations that have both high leverage and
#'   high Cook's D.
#'
#' @references Fox, J. (1997). Applied regression analysis, linear models, and
#'   related methods. Thousand Oaks, CA: Sage Publications.
#'
#' @examples
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  InfCases(m1)
#'
#' @export
InfCases <- function(x, digits = 3){
  if(!is.null(digits)) {
    assertthat::assert_that(assertthat::is.number(digits),
                            msg = "digits must be a scalar numeric/integer value")
    assertthat::assert_that(digits%%1 == 0,
                            msg = "digits must be a whole number")
  }
  DF            <- x$model
  DF$hat        <- stats::hatvalues(x)
  DF$CookD      <- stats::cooks.distance(x)
  DF$StdPearson <- round(stats::rstandard(x, type = "pearson"), digits = digits)
  High          <- DF[with(DF, hat > hatco(x) & CookD > CookDco(x)),]
  High$hat      <- round(High$hat, digits = digits)
  High$CookD    <- round(High$CookD, digits = digits)
  return(High)}
