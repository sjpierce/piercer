context("P-value inputs generate appropriate results")

test_that("p = 0 returns Inf", {
  expect_equal(p2s(p = 0), Inf)
})
