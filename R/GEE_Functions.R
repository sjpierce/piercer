#'=============================================================================
#' @name find_tor_probs
#'
#' @title Find the pair of probabilities that average to a target probability
#'   and yield a target odds-ratio.
#'
#' @description Given an input probability (mp) and a target odds-ratio (tor),
#'   iteratively find a pair of probabilities (p0 and p1) whose mean is equal
#'   to the input mean probability (mp) and also yield the target odds-ratio.
#'
#' @param mp A numeric value for the mean probability. This value must be a
#'   positive number in the open unit interval (0,1). It is assumed that this
#'   is the unweighted mean probability after pooling data from two groups of
#'   equal size.
#'
#' @param by The amount to add or subtract from the mean probability at each
#'   iteration to generate each member of the pair of probabilities used to
#'   compute the odds-ratio tested against the target odds-ratio. This value
#'   must be a positive number in the open unit interval (0,1).
#'
#' @param tor A numeric value for the targets odds-ratio. This value must be
#'   greater than zero (tor > 0) and must be finite (tor = Inf yields an error).
#'
#' @param direction A character value of either "gt" or "lt", which respectively
#'   indicate whether to solve for a resulting odds-ratio greater than tor
#'   (between 1 and Inf) or less than tor (between 0 and 1). The default is
#'   "gt".
#'
#' @details This function is intended to facilitate sample size planning
#'   calculations. The idea is that one may have some a priori estimate of an
#'   average probability of some event occurring, but want to use a binary
#'   predictor in a generalized linear model (GLM) or generalized estimating
#'   equation (GEE) that has a particular target odds-ratio. Some sample size
#'   formulas require you to input the pair of probabilities instead of the
#'   odds-ratio. This function allows you to convert the odds-ratio and average
#'   probability (ignoring the predictor) into the required pair of
#'   probabilities.
#'
#'   The function will keep p0 and p1 inside the open unit interval
#'   (0,1). It checks and validates the arguments provided to ensure sensible
#'   values have been selected. It assumes that the two proportions are
#'   estimated from groups of equal size, so the values of p0 and p1 are
#'   incrementally moved away from the mp value in opposite directions until
#'   the resulting odds-ratio meets the target value. If either p0 or p1
#'   approaches too close to either 0 or 1, then it will stop changing and then
#'   only the other probability will change thereafter to reach the target
#'   odds-ratio.
#'
#' @return A data frame containing the mean probability (mp), the target
#'   odds-ratio (TargetOR), the value of by argument, the pair of target
#'   probabilities (p0 and p1), the odds-ratio resulting from them (ResultOR),
#'   and the inverse of the resulting odds-ratio (InverseOR).
#'
#' @importFrom assertthat assert_that is.number is.string
#'
#' @examples
#' find_tor_probs(mp = .75, by = .001, tor = 2)
#' find_tor_probs(mp = .75, by = .001, tor = 0.5, direction = "lt")
#'
#' @export
find_tor_probs <- function (mp = .50, by = .001, tor, direction = "gt") {
  # Validate inputs.
  assert_that(is.number(mp),
              msg = "mp must be a single numeric value")
  assert_that(mp > 0 & mp < 1,
              msg = "mp must be a number between 0 and 1")
  assert_that(is.number(by),
              msg = "by must be a single numeric value")
  assert_that(by > 0 & by < 1,
              msg = "by must be a number between 0 and 1")
  assert_that(is.number(tor),
              msg = "tor must be a single numeric value")
  assert_that(tor > 0,
              msg = "tor must be a number greater than 0")
  assert_that(is.finite(tor),
              msg = "tor must be a finite number")
  assert_that(is.string(direction),
              msg = "direction must be a single character value")
  assert_that(direction %in% c("gt", "lt"),
              msg = "direction must be either 'gt' or 'lt'")

  # Initialize values before staring loops
  ror <- 1.0
  p0 <- mp
  p1 <- mp

  # If user wants ror greater than tor
  if(direction == "gt") {
    assert_that(tor >= 1,
                msg = "If direction = 'gt', tor must be >= 1")
    while (ror < tor) {
      # Adjust p0 toward 0, but keep it positive.
      p0  <- ifelse(test = p0 - by > 0, yes = p0 - by, no = p0)
      # Adjust p1 toward 1, but keep it less than 1.
      p1  <- ifelse(test = p1 + by < 1, yes = p1 + by, no = p1)
      # Compute the resulting odds-ratio and inverse odds ratio.
      ror <- p2or(p0 = p0, p1 = p1)
      ior <- 1/ror
    }
  }

  # If user wants ror less than tor
  if(direction == "lt") {
    assert_that(tor <= 1,
                msg = "If direction = 'lt', tor must be <= 1")
    while (ror > tor) {
      # Adjust p0 toward 1, but keep it less than 1.
      p0  <- ifelse(test = p0 + by < 1, yes = p0 + by, no = p0)
      # Adjust p1 toward 0, but keep it positive.
      p1  <- ifelse(test = p1 - by > 0, yes = p1 - by, no = p1)
      # Compute the resulting odds-ratio and inverse odds ratio.
      ror <- p2or(p0 = p0, p1 = p1)
      ior <- 1/ror
    }
  }

  # Create and return the results
  res <- data.frame(mp, TargetOR = tor, by, p0, p1, ResultOR = ror,
                    InverseOR = ior)
  return(res)
}

#'=============================================================================
#' @name geen
#'
#' @title Generalized estimating equation number of clusters (N) required for
#'   testing a main effect of treatment
#'
#' @description Compute the sample size (N, number of clusters) required to test
#'   a main effect of a treatment in a 2-arm parallel groups design via a
#'   3-level generalized estimating equation with binary outcome data.
#'
#' @param p0 A numeric value for \eqn{p_0}{p_0}: the probability of the outcome
#'   in the control arm.
#'
#' @param p1 A numeric value for \eqn{p_1}{p_1}: the probability of the outcome
#'   in the treatment arm.
#'
#' @param r A numeric value for \eqn{r}{r}: the correlation between evaluations
#'   from the same subject.
#'
#' @param rho A numeric value for \eqn{\rho}{rho}: the correlation between
#'   outcome evaluations from different subjects in the same cluster.
#'
#' @param n_e A numeric value for \eqn{n_e}{n_e}: the number of outcome
#'   evaluations per subject (level 1 observations of the binary outcome
#'   variable), which is assumed to be constant across subjects.
#'
#' @param n_s A numeric value for \eqn{n_s}{n_s}: the number of subjects (level
#'   2 units) per cluster (i.e., the cluster size), which is assumed to be
#'   constant across clusters.
#'
#' @param pi_c A numeric value for \eqn{\pi}{pi}: the proportion of clusters
#'   in the control arm.
#'
#' @param alpha A numeric value for \eqn{\alpha}{alpha}: the Type I error rate.
#'
#' @param gamma A numeric value for \eqn{\gamma}{gamma}: the Type II error rate.
#'
#' @details This function uses a z-score approximation instead of a t-statistic
#'   to avoid iteratively solving the sample size formula. The resulting N is
#'   then then translated into power using the t-statistic and degrees of
#'   freedom implied by the resulting sample size.
#'
#' @return A data frame.
#'
#' @references Teerenstra, S., Lu, B., Preisser, J. S., van Achterberg, T., &
#'   Borm, G. F. (2010). Sample size considerations for GEE analyses of
#'   three-level cluster randomized trials. Biometrics, 66(4), 1230-1237.
#'   doi:10.1111/j.1541-0420.2009.01374.x
#'
#' @importFrom stats pt qt
#'
#' @examples
#'
#' @export
geen <- function(p0, p1, r, rho, n_e = 1, n_s = 1, pi_c = 0.50, alpha = .05,
                 gamma = .20) {
  res         <- data.frame(p0, p1, rho, r, n_s, n_e, pi_c, alpha, gamma)
  res$phi.e   <- with(res, 1 + (n_e - 1)*r)               # Eq. 3, p. 1232
  res$rho.sne <- with(res, n_e*rho/(1 + (n_e - 1)*r))     # Eq. 3, p. 1232
  res$phi.s   <- with(res, 1 + (n_s - 1)*rho.sne)         # Eq. 3, p. 1232
  res$phi     <- with(res, phi.s*phi.e)                   # Eq. 3, p. 1232
  res$b       <- with(res, log(p0/(1 - p0)) - log(p1/(1 - p1)))  # p. 1232
  # Find numerator for Eq. 5 on 1232.
  res$Eq5.a   <- with(res, 1/(pi_c*p0*(1 - p0)) + 1/((1 - pi_c)*p1*(1 - p1)))
  # Find denominator for Eq. 5 using substitution above Eq. 3 on p. 1232.
  res$Eq5.b   <- with(res, (n_s*n_e)/phi)
  # Find s2B using Eq. 5 (after substitution of denominator)
  res$s2B     <- with(res, Eq5.a/Eq5.b)
  # Find Nraw via equation right after Eq. 2 on p. 1232.
  res$z1      <- with(res, qnorm(p = alpha/2))
  res$z2      <- with(res, qnorm(p = gamma))
  res$Nraw    <- with(res, ((z1 + z2)^2)*(s2B/(b^2)))
  # Adjust raw N calculation per top left paragraph on p. 1233.
  res$Nraw    <- with(res, Nraw*(Nraw + 1)/(Nraw - 1))
  # Find N by truncating Nraw (add 1 if that does not yield an even integer).
  res$N       <- with(res, ifelse(trunc(Nraw) %% 2 == 0,
                                  trunc(Nraw),
                                  trunc(Nraw + 1)))
  # Power via Eq. 2, p. 1232
  res$t3      <- with(res, qt(p = alpha/2, df = N - 2))
  res$Power   <- with(res, pt(q = (t3 + (sqrt(N)*sqrt(b^2))/(sqrt(s2B))),
                              df = N - 2))
  return(res)
}

#'=============================================================================
#' @name geep
#'
#' @title Generalized estimating equation power for testing a main effect of
#'   treatment given a specific sample size (N, number of clusters)
#'
#' @description Compute the power for testing a main effect of a treatment in a
#'   2-arm parallel groups design via a 3-level generalized estimating equation
#'   with binary outcome data, given sample size (N, number of clusters).
#'
#' @param p0 A numeric value for \eqn{p_0}{p_0}: the probability of the outcome
#'   in the control arm.
#'
#' @param p1 A numeric value for \eqn{p_1}{p_1}: the probability of the outcome
#'   in the treatment arm.
#'
#' @param r A numeric value for \eqn{r}{r}: the correlation between evaluations
#'   from the same subject.
#'
#' @param rho A numeric value for \eqn{\rho}{rho}: the correlation between
#'   outcome evaluations from different subjects in the same cluster.
#'
#' @param n_e A numeric vector for \eqn{n_e}{n_e}: the number of outcome
#'   evaluations per subject (level 1 observations of the binary outcome
#'   variable), which is assumed to be constant across subjects.
#'
#' @param n_s A numeric vector for \eqn{n_s}{n_s}: the number of subjects (level
#'   2 units) per cluster (i.e., the cluster size), which is assumed to be
#'   constant across clusters.
#'
#' @param pi_c A numeric value for \eqn{\pi}{pi}: the proportion of clusters
#'   in the control arm.
#'
#' @param alpha A numeric value for \eqn{\alpha}{alpha}: the Type I error rate.
#'
#' @param N A numeric vector for the total number of clusters in the sample.
#'
#' @details This function is useful for examining a range of scenarios. Using
#'   vectors with legnth > 1 for parameters such as n_s, n_e, and N will yield
#'   power estimates for multiple scenarios in the data frame returned.
#'
#' @return A data frame.
#'
#' @references Teerenstra, S., Lu, B., Preisser, J. S., van Achterberg, T., &
#'   Borm, G. F. (2010). Sample size considerations for GEE analyses of
#'   three-level cluster randomized trials. Biometrics, 66(4), 1230-1237.
#'   doi:10.1111/j.1541-0420.2009.01374.x
#'
#' @importFrom stats pt qt
#'
#' @examples
#'
#' @export
geep <- function(p0, p1, r, rho, n_e = 1, n_s = 1, pi_c = 0.50, alpha = .05,
                 N) {
  res         <- data.frame(p0, p1, rho, r, n_s, n_e, N, pi_c, alpha)
  res$phi.e   <- with(res, 1 + (n_e - 1)*r)               # Eq. 3, p. 1232
  res$rho.sne <- with(res, n_e*rho/(1 + (n_e - 1)*r))     # Eq. 3, p. 1232
  res$phi.s   <- with(res, 1 + (n_s - 1)*rho.sne)         # Eq. 3, p. 1232
  res$phi     <- with(res, phi.s*phi.e)                   # Eq. 3, p. 1232
  res$b       <- with(res, log(p0/(1 - p0)) - log(p1/(1 - p1)))  # p. 1232
  # Find numerator for Eq. 5 on 1232.
  res$Eq5.a   <- with(res, 1/(pi_c*p0*(1 - p0)) + 1/((1 - pi_c)*P1*(1 - P1)))
  # Find denominator for Eq. 5 using substitution above Eq. 3 on p. 1232.
  res$Eq5.b   <- with(res, (n_s*n_e)/phi)
  # Find s2B using Eq. 5 (after substition of denominator)
  res$s2B     <- with(res, Eq5.a/Eq5.b)
  # Power via Eq. 2, p. 1232
  res$t3      <- with(res, qt(p = alpha/2, df = N - 2))
  res$Power   <- with(res, pt(q = (t3 + (sqrt(N)*sqrt(b^2))/(sqrt(s2B))),
                              df = N - 2))
  return(res)
}
