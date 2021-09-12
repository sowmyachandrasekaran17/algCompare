context("alg_compare")

A <-c(0.1,0.05,0.05,0.08)
B <- c(0.01,0.02,0.01,0.05)

test_that("check that alg_compare function works without problems", {
  skip_on_cran()
  res <-alg_compare(A,B)
  expect_equal(typeof(res$decision),"character")
  expect_equal(is.numeric(res$p),TRUE)
  expect_equal(is.numeric(res$severity_rejectH0),TRUE)
  expect_equal(is.numeric(res$discrepancy),TRUE)
  expect_equal(is.numeric(res$power),TRUE)
})
test_that("Error when discrepancy_range is not valid",{
  expect_that(alg_compare(A,B,discrepancy_range = "ff"),throws_error())
})
test_that("Check that the function returns values",
          {
            expect_true( is.list(alg_compare(A,B)))
          })

C <-c(0.1,0.15,0.15,0.08)
D <- c(0.01,0.02)

test_that("Error when Alg_bm and Alg_new lengths differ",
          {
            expect_that(alg_compare(C,D), throws_error())
          })
C <-c(0.1,0.15,0.15,0.08)
D <- c("a","b","c","d")

test_that("Error when input is not numeric vector",
          {
            expect_that(alg_compare(C,D), throws_error())
          })
