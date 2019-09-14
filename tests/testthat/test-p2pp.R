context("p2pp inputs generate appropriate results")

test_that("p = NA returns NA", {
  expect_output(p2pp(p = NA), NA)
})

test_that("Non-numeric p returns appropriate error", {
  expect_error(p2pp(p = "a"), "p must be NA or numeric")
  expect_error(p2pp(p = TRUE), "p must be NA or numeric")
  expect_error(p2pp(p = factor("a")), "p must be NA or numeric")
})

test_that("Numeric p outside 0 to 1 returns appropriate error", {
  expect_error(p2pp(p = -.1), "Numeric p must be a proportion between 0 and 1")
  expect_error(p2pp(p = 1.1), "Numeric p must be a proportion between 0 and 1")
})

test_that("If present, digits is a single whole number", {
  expect_error(p2pp(.05, digits = c(1, 2)),
               "If present, digits must be a scalar numeric/integer value")
  expect_error(p2pp(.05, digits = "a"),
               "If present, digits must be a scalar numeric/integer value")
  expect_error(p2pp(.05, digits = 0.5),
               "If present, digits must be a whole number")
})
