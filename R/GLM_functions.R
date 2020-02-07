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
#' @importFrom stats cor
#' @importFrom stats fitted
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
#' @importFrom assertthat assert_that
#'
#' @examples
#' invlogit(0)
#' round(invlogit(-7:7), 3)
#'
#' @export
invlogit <- function(x) {
  assert_that(all(is.na(x) | is.numeric(x)),
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
#' @seealso \code{\link{CookDco}} for Cook's D cutoff.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom stats cooks.distance
#'
#' @examples
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  PlotCookD(m1)
#'
#' @export
PlotCookD <- function(x, id.n = 10) {
  DF       <- data.frame(CookD = cooks.distance(x))
  DF$RN    <- 1:nrow(DF)
  DF$group <- DF$CookD > CookDco(x)
  ggplot(DF, aes(DF$RN, DF$CookD, color=DF$group, group=DF$group,
                 aes(DF$RN, DF$CookD))) +
    annotate(geom = "text", x = 0, y = max(DF$CookD), color = "black",
             hjust = 0, vjust = 0, size = 4,
             label = paste("Cutoff >", round(CookDco(x), digits = 3))) +
    geom_segment(aes(DF$RN, xend=DF$RN, 0, yend=DF$CookD,
                     color=DF$group), data=DF) +
    theme_bw() +
    scale_color_manual(values=c("black", "red")) +
    geom_hline(yintercept = CookDco(x), color = "black", linetype = 2) +
    ylab("Cook's Distance") +
    xlab("Row Name") +
    theme(legend.position = "none")
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
#' @seealso \code{\link{hatco}} for leverage cutoff.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom stats hatvalues
#'
#' @examples
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  PlotHat(m1)
#'
#' @export
PlotHat <- function(x, id.n = 10) {
  DF       <- data.frame(Hat = hatvalues(x))
  DF$RN    <- 1:nrow(DF)
  DF$group <- DF$Hat > hatco(x)
  ggplot(DF, aes(DF$RN, DF$Hat, color=DF$group, group=DF$group,
                 aes(DF$RN, DF$Hat))) +
    annotate(geom = "text", x = 0, y = max(DF$Hat), color = "black",
             hjust = 0, vjust = 0, size = 4,
             label = paste("Cutoff >", round(hatco(x), digits = 3))) +
    geom_segment(aes(DF$RN, xend=DF$RN, 0, yend=DF$Hat,
                     color=DF$group),
                 data=DF)  +
    theme_bw() +
    scale_color_manual(values=c("black", "red")) +
    geom_hline(yintercept = hatco(x), color = "black", linetype = 2) +
    ylab("Leverage (hat)") +
    xlab("Row Name") +
    theme(legend.position = "none")
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
#' @seealso \code{\link{hatco}} for leverage cutoff, \code{\link{CookDco}} for
#'   Cook's D cutoff.
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom stats cooks.distance
#' @importFrom stats hatvalues
#' @importFrom stats rstandard
#'
#' @examples
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  InfCases(m1)
#'
#' @export
InfCases <- function(x, digits = 3){
  if(!is.null(digits)) {
    assert_that(is.number(digits),
                msg = "digits must be a scalar numeric/integer value")
    assert_that(digits%%1 == 0,
                msg = "digits must be a whole number")
  }
  DF            <- x$model
  DF$hat        <- hatvalues(x)
  DF$CookD      <- cooks.distance(x)
  DF$StdPearson <- round(rstandard(x, type = "pearson"), digits = digits)
  High          <- DF[with(DF, hat > hatco(x) & CookD > CookDco(x)),]
  High$hat      <- round(High$hat, digits = digits)
  High$CookD    <- round(High$CookD, digits = digits)
  return(High)
}

#'=============================================================================
#' @name lrcm
#'
#' @title Logistic regression classification measures
#'
#' @description This function computes a variety of classification measures
#'   relevant to logistic regression models.
#'
#' @inheritParams pROC::coords
#'
#' @inheritParams pROC::ci.coords
#'
#' @param seed A single number to be passed to set.seed(), which is used to
#'   ensure reproducibility of the bootstrapped confidence intervals.
#'
#' @details This function is a convnient wrapper around pROC::coords() and
#'   pROC::ci.coords(). Most of the arguments are passed along to those
#'   functions. What lrcm() does is just gather the results of both estimates
#'   and bootstrapped confidence intervals into a data frame. See the details
#'   sections of documentetion for pROC::coords() and pROC::ci.coords() for more
#'   information. Some information mentioned in the descriptions of individual
#'   parameters listed above is absent here because these parameters are
#'   inherited from pROC. It is more efficient to refer you to the pROC
#'   documentation than to retype it.
#'
#' @return A data frame showing estimates and bootstrapped confidence intervals
#'   for various classification measures.
#'
#' @references Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F.,
#'   Sanchez, J.-C., & Müller, M. (2011). pROC: an open-source package for R and
#'   S+ to analyze and compare ROC curves. BMC Bioinformatics, 12, 77.
#'   doi:10.1186/1471-2105-12-77
#'
#'   Youden, W. J. (1950). Index for rating diagnostic tests. Cancer, 3(1),
#'   32-35.
#'   doi:10.1002/1097-0142(1950)3%3A1%3C32%3A%3AAID-CNCR2820030106%3E3.0.CO%3B2-3
#'
#' @seealso \code{\link[pROC]{roc}}, \code{\link[pROC]{coords}},
#'   \code{\link[pROC]{ci.coords}}, \code{\link[base]{set.seed}}.
#'
#' @importFrom pROC coords
#' @importFrom pROC ci.coords
#'
#' @examples
#'  library(pROC)
#'  m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#'  set.seed(4921) # For reproducibility of bootstrap estimates.
#'  rocm1 <- roc(m1$y ~ predict(m1, type = "response"), ci = TRUE,
#'               direction = "<", ci.method = "bootstrap")
#'  print(rocm1)
#'  lrcm(rocm1, seed = 563)
#'
#' @export
lrcm <- function(roc, x = "best", best.method = "youden", transpose = FALSE,
                 ret = "all", seed, ...){
  Est   <- t(coords(roc, x = x, best.method = best.method,
                    transpose = transpose, ret = ret))
  set.seed(seed) # Ensure reproducible bootstrap estimates.
  Estci <- ci.coords(roc, x = x, best.method = best.method,
                     transpose = transpose, ret = ret)
  res <- as.data.frame(cbind(Est, do.call(rbind.data.frame, Estci)),
                       row.names = rownames(Estci))
  return(res)
}

#'=============================================================================
#' @name brier
#'
#' @title Compute the Brier score for a logistic regression model
#'
#' @description Computes the Brier score (average prediction error) for a
#'   logistic regression model, which is a measure of overall accuracy.
#'
#' @param x A logistic regression model fit via glm(family = binomial).
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the result. Defaults to NULL, which does not round the result.
#'
#' @param scaled A logical value that controls whether the to return the scaled
#'   Brier score (if TRUE, the default) or the unscaled score (if FALSE).
#'
#' @details The Brier score is a measure of overall accuracy for a logistic
#'   regression model; it is the average prediction error. It can be computed in
#'   unscaled or scaled form. The scaled Brier score ranges from [0, 1]. A
#'   perfect model will have a value of 0, while a noninformative model will
#'   have a value of 1. The unscaled score can range from [0, 0.25] if the
#'   incidence of the outcome is 50%.
#'
#' @return Numeric values for the Brier score and the scaled Brier score.
#'
#' @references Steyerberg, E. W., Harrell Jr., F. E., Borsboom, G. J. J. M.,
#'   Eijkemans, M. J. C., Vergouwe, Y., & Habbema, J. D. F. (2001). Internal
#'   validation of predictive models: Efficiency of some procedures for logistic
#'   regression analysis. Journal of Clinical Epidemiology, 54(8), 774-781.
#'   doi:10.1016/S0895-4356(01)00341-9
#'
#'   Steyerberg, E. W., Vickers, A. J., Cook, N. R., Gerds, T., Gonen, M.,
#'   Obuchowski, N. A., . . . Kattan, M. W. (2010). Assessing the performance of
#'   prediction models : A framework for traditional and novel measures.
#'   Epidemiology, 21(1), 128-138. doi:10.1097/EDE.0b013e3181c30fb2
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @importFrom stats nobs
#' @importFrom stats predict
#' @importFrom stats resid
#'
#' @examples
#' m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
#' brier(m1)
#' brier(m1, digits = 2)
#' brier(m1, scaled = FALSE)
#'
#' @export
brier <- function(x, scaled = TRUE, digits = NULL) {
  assert_that(is.logical(scaled),
              msg = "scaled must be a logical value (TRUE or FALSE)")
  if(!is.null(digits)) {
    assert_that(is.number(digits),
                msg = "digits must be a scalar numeric/integer value")
    assert_that(digits%%1 == 0,
                msg = "digits must be a whole number")
  }
  r     <- resid(x, type = "response")
  p     <- predict(x, type = "response")
  y     <- p + r
  n     <- nobs(x)
  bs    <- sum((y - p)^2)/n
  bsmax <- mean(p)*(1 - mean(p))
  sbs   <- 1 - bs/bsmax
  res <- ifelse(scaled == TRUE, yes = sbs, no = bs)
  if(!is.null(digits))  res <- round(res, digits = digits)
  return(res)
}

