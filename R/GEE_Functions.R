#'=============================================================================
#' @name find_tor_probs()
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
#'   (between 1 and Inf) or less than tor (between 0 and 1). The default is "gt".
#'
#' @details The function will keep p0 and p1 inside the open unit interval
#'   (0,1). It checks and validates the arguments provided to ensure sensible
#'   values have been selected. It assumes that the two proportions are
#'   estimated from groups of equal size, so the values of p0 and p1 are
#'   incrementally moved away from the mp value in opposite directions until
#'   the resulting odds-ratio meets the target value. If either p0 or p1 reaches
#'   either 0 or 1, then it will stop changing and then only the other
#'   probability will change thereafter to reach the target odds-ratio.
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
#' find_tor_probs(mp = .75, by = .001, tor = 1/2, direction = "lt")
#'
#' @export
find_tor_probs <- function (mp = .50, by = .001, tor, direction = "gt") {
  # Validate inputs.
  assert_that(is.number(mp),
              msg = "mp must be a single numeric value")
  assert_that(mp >= 0 & mp <= 1,
              msg = "mp must be a number between 0 and 1")
  assert_that(is.number(by),
              msg = "by must be a single numeric value")
  assert_that(by >= 0 & by <= 1,
              msg = "by must be a number between 0 and 1")
  assert_that(is.number(tor),
              msg = "tor must be a single numeric value")
  assert_that(tor >= 0,
              msg = "tor must be a number greater than 0")
  assert_that(is.finite(tor),
              msg = "tor must be a finite number")
  assert_that(is.string(direction),
              msg = "tor must be a character value")
  assert_that(direction %in% c("gt", "lt"),
              msg = "tor must be either 'gt' or 'lt'")

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

