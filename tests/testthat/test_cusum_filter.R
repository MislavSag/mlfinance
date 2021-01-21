context('CUSUM filter')

test_that("resuts are correct", {
  data(spy)
  close <- subset(spy, select = c("index", "close"))[1:100, ]
  expect_equal(cusum_filter(close, 0.002, return_datetime = FALSE), c(7, 75, 80, 90))
}
)
