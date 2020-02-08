#'=============================================================================
#' @name rxx.NL
#'
#' @title Non-linear SEM composite reliability coefficient
#'
#' @description Computes a reliability estimate for a latent factor measured by
#'   ordinal indicator items from confirmatory factor analysis (CFA) parameter
#'   estimates.
#'
#' @param lambda A factor loading matrix (items x factors). See details below.
#'
#' @param tau A threshold matrix (items x thresholds, same set for all items).
#'
#' @param fcor A latent factor correlation matrix (factors x factors), which
#'   defaults to a 1 x 1 matrix: [cor = 1]
#'
#' @param digits An integer specifying the number of decimal places to used when
#'   rounding the result. Defaults to NULL, which does not round the result.
#'
#' @details This function computes Green & Yang's (2009) nonlinear SEM
#'   reliability coefficient from CFA with ordinal indicators (Equation 21).
#'   The code was adapted from Yang & Green's (2015) SAS macro, found online at
#'   http://myweb.fsu.edu/yyang3/files/examplecodes.txt. Note that Green &
#'   Yang's (2009) Equation 21 matches Yang & Green's (2015) Equation 9.
#'
#'   This code was written assuming one would use Mplus to estimate the CFA, so
#'   some terminology here reflects that. If you are using WLSMV estimation with
#'   latent variance fixed to 1 and all indicator loadings freely estimated,
#'   then unstandardized & standardized estimates are identical (you can use
#'   either one as the lambda input to this function). However, if you are using
#'   Bayesian estimation with PX parameterization, use the STDYX estimates.
#'
#' @return A numeric value for the reliability.
#'
#' @references Green, S. B., & Yang, Y. (2009). Reliability of summed item
#'   scores using structural equation modeling: An alternative to coefficient
#'   alpha. Psychometrika, 74(1), 155-167. doi:10.1007/s11336-008-9099-3
#'
#'   Yang, Y., & Green, S. B. (2015). Evaluation of structural equation modeling
#'   estimates of reliability for scales with ordered categorical items.
#'   Methodology: European Journal of Research Methods for the Behavioral and
#'   Social Sciences, 11(1), 23-34. doi:10.1027/1614-2241/a000087
#'
#' @importFrom pbivnorm pbivnorm
#'
#' @export
rxx.NL <- function(lambda, tau, fcor = matrix(1, ncol = 1), digits = NULL) {
  if(!is.null(digits)) {
    assert_that(is.number(digits),
                msg = "If present, digits must be a scalar numeric/integer value")
    assert_that(digits%%1 == 0,
                msg = "If present, digits must be a whole number")
  }
  NTHRESH <- ncol(tau)
  NCAT    <- NTHRESH + 1
  NITEM   <- nrow(lambda)
  NFACT   <- ncol(lambda)
  # Compute polychoric correlation matrix & set diagonals to 1.
  POLYR   <- lambda %*% fcor %*% t(lambda)
  diag(POLYR) <- 1
  # Compute numerator & denominator (strictly parallels the SAS code).
  sumnum <- 0
  addden <- 0
  for(j in 1:NITEM) {
      for(jp in 1:NITEM) {
          sumprobn2 <- 0
          addprobn2 <- 0
          for(c in 1:NTHRESH) {
              for(cp in 1:NTHRESH) {
                  sumrvstar <- 0
                  for(k in 1:NFACT) {
                      for(kp in 1:NFACT){
                          sumrvstar <- sumrvstar + lambda[j,k]*lambda[jp,kp]*fcor[k,kp]
                      }
                  }
                  sumprobn2 <- sumprobn2 + pbivnorm(tau[j,c], tau[jp,cp], sumrvstar)
                  addprobn2 <- addprobn2 + pbivnorm(tau[j,c], tau[jp,cp], POLYR[j,jp])
              }
          }
          sumprobn1  <- 0
          sumprobn1p <- 0
          for (cc in 1:NTHRESH){
               sumprobn1  <- sumprobn1 + pnorm(tau[j,cc])
               sumprobn1p <- sumprobn1p + pnorm(tau[jp,cc])
          }
          sumnum <- sumnum + (sumprobn2 - sumprobn1*sumprobn1p)
          addden <- addden + (addprobn2 - sumprobn1*sumprobn1p)
      }
  }
  # Compute NL-SEM reliability.
  rxx <- data.frame(Numerator = sumnum, Denominator = addden,
                    r.NL = sumnum/addden)
  # Round result if digits is specified via a number
  if(!is.null(digits))  rxx <- round(rxx, digits = digits)
  return(rxx)
}
