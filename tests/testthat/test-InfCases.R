context("InfCases inputs generate appropriate results")

m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)

test_that("Digits is a single whole number", {
  expect_error(InfCases(m1, digits = c(1, 2)),
               "digits must be a scalar numeric/integer value")
  expect_error(InfCases(m1, digits = "a"),
               "digits must be a scalar numeric/integer value")
  expect_error(InfCases(m1, digits = 0.5),
               "digits must be a whole number")
})
