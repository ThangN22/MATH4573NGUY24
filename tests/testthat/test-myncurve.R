test_that("Function runs for extreme values", {
  expect_no_error(myncurve(a = 1000, mu = 10000, sigma = 100))
})

test_that("Area calculation is correct", {
  result <- myncurve(a = 6, mu = 10, sigma = 5)
  expect_equal(result$area, round(pnorm(6, mean = 10, sd = 5) - pnorm(-1000, mean = 10, sd = 5), 4))
})

test_that("dnorm is correct", {
  x <- seq(-1000, 6, length.out = 1000)
  result <- myncurve(a = 6, mu = 10, sigma = 5)
  expect_equal(dnorm(x,mean=result$mu,sd=result$sigma), dnorm(x,mean=10,sd=5))
})

test_that("xcurve is correct", {
  result <- myncurve(a = 6, mu = 10, sigma = 5)
  expect_equal(seq(-1000,result$a,length=1000), seq(-1000,6,length=1000))
})



