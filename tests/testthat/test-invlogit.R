# Test whether invlogit inputs generate appropriate results.

test_that("x = NA returns NA", {
  expect_output(invlogit(x = NA), NA)
})

test_that("Non-numeric x returns appropriate error", {
  expect_error(invlogit(x = "a"), "x must be NA or numeric")
  expect_error(invlogit(x = TRUE), "x must be NA or numeric")
  expect_error(invlogit(x = factor("a")), "x must be NA or numeric")
})
