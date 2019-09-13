context("p2s inputs generate appropriate results")

test_that("p = 0 returns Inf", {
  expect_equal(p2s(p = 0), Inf)
})

test_that("p = NA returns appropriate error", {
  expect_error(p2s(p = NA), "p must not contain NA values")
})

test_that("Non-numeric p returns appropriate error", {
  expect_error(p2s(p = "a"), "p must be a numeric vector")
  expect_error(p2s(p = TRUE), "p must be a numeric vector")
  expect_error(p2s(p = "a"), "p must be a numeric vector")
  expect_error(p2s(p = factor("a")), "p must be a numeric vector")
})

test_that("Numeric p outside 0 to 1 returns appropriate error", {
  expect_error(p2s(p = -.1), "p must be a proportion between 0 and 1")
  expect_error(p2s(p = 1.1), "p must be a proportion between 0 and 1")
})

test_that("If present, digits is a single whole number", {
  expect_error(p2s(.05, digits = c(1, 2)),
               "If present, digits must be a scalar numeric/integer value")
  expect_error(p2s(.05, digits = "a"),
               "If present, digits must be a scalar numeric/integer value")
  expect_error(p2s(.05, digits = 0.5),
               "If present, digits must be a whole number")
})
