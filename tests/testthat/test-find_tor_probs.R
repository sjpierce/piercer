context("find_to_probs inputs generate appropriate results")

test_that("Non-numeric mp returns appropriate error", {
  expect_error(find_tor_probs(mp = "a", by = .001, tor = 2),
               "mp must be a single numeric value")
})

test_that("Non-scalar mp returns appropriate error", {
  expect_error(find_tor_probs(mp = c(.7, .5), by = .001, tor = 2),
               "mp must be a single numeric value")
})

test_that("Scalar, numeric mp out of range returns appropriate error", {
  expect_error(find_tor_probs(mp = -.1, by = .001, tor = 2),
               "mp must be a number between 0 and 1")
  expect_error(find_tor_probs(mp = 0, by = .001, tor = 2),
               "mp must be a number between 0 and 1")
  expect_error(find_tor_probs(mp = 1, by = .001, tor = 2),
               "mp must be a number between 0 and 1")
  expect_error(find_tor_probs(mp = 1.1, by = .001, tor = 2),
               "mp must be a number between 0 and 1")
})

test_that("Non-numeric by returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = "a", tor = 2),
               "by must be a single numeric value")
})

test_that("Non-scalar by returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = c(.001, .0005), tor = 2),
               "by must be a single numeric value")
})

test_that("Scalar, numeric by out of range returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = -.001, tor = 2),
               "by must be a number between 0 and 1")
  expect_error(find_tor_probs(mp = .7, by = 0, tor = 2),
               "by must be a number between 0 and 1")
  expect_error(find_tor_probs(mp = .7, by = 1, tor = 2),
               "by must be a number between 0 and 1")
  expect_error(find_tor_probs(mp = .7, by = 1.001, tor = 2),
               "by must be a number between 0 and 1")
})

test_that("Non-numeric tor returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = .001, tor = "a"),
               "tor must be a single numeric value")
})

test_that("Non-scalar tor returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = .001, tor = c(2, 3)),
               "tor must be a single numeric value")
})

test_that("Scalar, numeric tor out of range returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = .001, tor = -.2),
               "tor must be a number greater than 0")
  expect_error(find_tor_probs(mp = .7, by = .001, tor = 0),
               "tor must be a number greater than 0")
  expect_error(find_tor_probs(mp = .7, by = .001, tor = Inf),
               "tor must be a finite number")
})

test_that("Non-character direction returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = .001, tor = 2, direction = 1),
               "direction must be a single character value")
})

test_that("Non-scalar direction returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = .001, tor = 2, direction = c("gt", "lt")),
               "direction must be a single character value")
})

test_that("Invalid scalar character direction returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = .001, tor = 2, direction = "at"),
               "direction must be either 'gt' or 'lt'")
})

test_that("Inconsistent tor and direction returns appropriate error", {
  expect_error(find_tor_probs(mp = .7, by = .001, tor = 2, direction = "lt"),
               "If direction = 'lt', tor must be <= 1")
  expect_error(find_tor_probs(mp = .7, by = .001, tor = .5, direction = "gt"),
               "If direction = 'gt', tor must be >= 1")
})
