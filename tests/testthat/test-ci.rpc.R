context("ci.rpc inputs generate appropriate results")

# Passed
test_that("conf.level = Inf returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = Inf),
               "conf.level must be a finite number between 0 and 1")
})

# Passed
test_that("conf.level = NA returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = NA),
               "conf.level must be a finite number between 0 and 1")
})

# Passed
test_that("conf.level = NaN returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = NaN),
               "conf.level must be a finite number between 0 and 1")
})

# Passed
test_that("conf.level = '0.95' returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = "0.95"),
               "conf.level must be a finite number between 0 and 1")
})

# Passed
test_that("conf.level = numeric() returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = numeric()),
               "conf.level must have a value")
})

# Passed
test_that("conf.level = c(0.90, 0.95) returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = c(0.90, 0.95)),
               "conf.level must have only 1 value")
})

# Passed
test_that("conf.level = 95 returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = 95),
               "conf.level must be between 0 and 1")
})

# Passed
test_that("conf.level = 0 returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = 0),
               "conf.level must be between 0 and 1")
})

# Passed
test_that("conf.level = 1 returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, conf.level = 1),
               "conf.level must be between 0 and 1")
})

# Passed
test_that("rn = 1 returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, rn = 1),
               "If present, rn must be a character value")
})

# Passed
test_that("rn = TRUE returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, rn = TRUE),
               "If present, rn must be a character value")
})

# Passed
test_that("rn = c('xy', 'xz') returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, rn = c("xy", "xz")),
               "If present, rn must have only 1 value")
})

# Passed
test_that("rn = character() returns error", {
  expect_error(ci.rpc(r = 0.5, se = .03, rn = character()),
               "If present, rn must have only 1 value")
})
