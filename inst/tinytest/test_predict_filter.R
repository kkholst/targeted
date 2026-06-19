test_predict_filter_bound <- function() {
  expect_error(
    predict_filter_bound(lower = 10, upper = 5),
    pattern = "lower bound should not be greater than upper bound."
  )

  d0 <- data.frame(y = c(1, 1))
  filter <- predict_filter_bound(lower = 0)(d0)
  pr <- c(-1, 0, 1)
  expect_equal(filter(pr), c(0, 0, 1))


  filter <- predict_filter_bound(upper = 0)(d0)
  expect_equal(filter(pr), c(-1, 0, 0))

  filter <- predict_filter_bound(upper = 0, lower = 0)(d0)
  expect_equal(filter(pr), c(0, 0, 0))

  filter <- predict_filter_bound(lower = NULL)(d0)
  expect_equal(filter(pr), pr)
}
test_predict_filter_bound()

test_predict_filter_bound_dynamic <- function() {
  expect_error(
    predict_filter_bound_dynamic(lower = 1),
    pattern = "lower and upper argument need to be logical."
  )
  expect_error(
    predict_filter_bound_dynamic(upper = 1),
    pattern = "lower and upper argument need to be logical."
  )
  expect_error(
    predict_filter_bound_dynamic(response = 1),
    pattern = "response argument needs to be a character."
  )

  d0 <- data.frame(y=c(-1, 0, 1), y0 = c(-10, 0, 10))

  pr <- c(-2, 0, 2)
  filter <- predict_filter_bound_dynamic()(d0)
  expect_equal(filter(pr), pr)

  filter <- predict_filter_bound_dynamic(lower = TRUE)(d0)
  expect_equal(filter(pr), c(-1, 0, 2))

  filter <- predict_filter_bound_dynamic(upper = TRUE)(d0)
  expect_equal(filter(pr), c(-2, 0, 1))

  filter <- predict_filter_bound_dynamic(lower = TRUE, upper = TRUE)(d0)
  expect_equal(filter(pr), c(-1, 0, 1))

  filter <- predict_filter_bound_dynamic(
    lower = TRUE, upper = TRUE, response = "y0"
  )(d0)
  expect_equal(filter(d0$y0 * 2), d0$y0)
}
test_predict_filter_bound_dynamic()
