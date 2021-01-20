context('CUSUM filter')

test_that("resuts are correct", {
  test <- seq(100, 110, 0.5)
  expect_equal(cusum_filter(test, 0.01), c(4, 7, 10, 13, 16, 19))
}
)
