context("p2pp inputs generate appropriate results")

test_that("p = NA returns appropriate error", {
  expect_error(p2pp(p = NA), "p must not contain NA values")
})

test_that("Non-numeric p returns appropriate error", {
  expect_error(p2pp(p = "a"), "p must be a numeric vector")
  expect_error(p2pp(p = TRUE), "p must be a numeric vector")
  expect_error(p2pp(p = "a"), "p must be a numeric vector")
  expect_error(p2pp(p = factor("a")), "p must be a numeric vector")
})

test_that("Numeric p outside 0 to 1 returns appropriate error", {
  expect_error(p2pp(p = -.1), "p must be a proportion between 0 and 1")
  expect_error(p2pp(p = 1.1), "p must be a proportion between 0 and 1")
})
