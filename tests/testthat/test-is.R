library(testthat)
library(miriamPrime)

context("Testing Prime function")


test_that("Correctly identify primes", {
  expect_equal(is.prime(5:10), as.logical(c(1, 0, 1, 0, 0, 0)))
})

test_that("Case for 2", {
  expect_equal(is.prime(2), TRUE)
})

test_that("Non-integer case", {
  expect_error(is.prime(3.5))
})

test_that("<2 case", {
  expect_error(is.prime(1))
})
