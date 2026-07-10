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

  expect_error(
    predict_filter_bound_dynamic(lower = TRUE, response = "nope")(d0),
    pattern = "response variable not found in data."
  )

}
test_predict_filter_bound_dynamic()

test_predict_filter_boundary <- function() {
  d0 <- data.frame(y = c(0, 1))
  filter <- predict_filter_bound(lower = 0, upper = 1)(d0)
  expect_equal(filter(c(0, 1)), c(0, 1))

  filter <- predict_filter_bound_dynamic(lower = TRUE, upper = TRUE)(d0)
  expect_equal(filter(c(0, 1)), c(0, 1))
}
test_predict_filter_boundary()

test_predict_filter_degenerate <- function() {
  d0 <- data.frame(y = c(5, 5))
  filter <- predict_filter_bound(lower = 5, upper = 5)(d0)
  expect_equal(filter(c(-1, 5, 10)), c(5, 5, 5))

  filter <- predict_filter_bound_dynamic(lower = TRUE, upper = TRUE)(d0)
  expect_equal(filter(c(-1, 5, 10)), c(5, 5, 5))
}
test_predict_filter_degenerate()


test_predict_filter_na <- function() {
  d0 <- data.frame(y = c(0, 1))
  # NA predictions remain NA (comparison with bound is NA)
  filter <- predict_filter_bound(lower = 0, upper = 1)(d0)
  expect_equal(filter(c(NA, -1, 2)), c(NA, 0, 1))

  dna <- data.frame(y = c(0, NA, 1))
  filter <- predict_filter_bound_dynamic(lower = TRUE, upper = TRUE)(dna)
  expect_equal(filter(c(-1, 0.5, 2)), c(0, 0.5, 1))
  expect_equal(filter(c(NA, 0.5, 2)), c(NA, 0.5, 1))
}
test_predict_filter_na()

# Integration with learner via the predict.filter argument
test_predict_filter_learner <- function() {
  n <- 200
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- with(d, x1 * 2 - x2 + rnorm(n))

  # static bounds
  lr <- learner_glm(y ~ x1 + x2,
    learner.args = list(
      predict.filter = predict_filter_bound(lower = 0, upper = 1)
    )
  )
  lr$estimate(d)
  pr <- lr$predict(d)
  expect_true(min(pr) >= 0 && max(pr) <= 1)

  # dynamic bounds derived from the training response
  lr <- learner_glm(y ~ x1 + x2,
    learner.args = list(predict.filter = predict_filter_bound_dynamic(
      lower = TRUE, upper = TRUE
    ))
  )
  lr$estimate(d)
  pr <- lr$predict(data.frame(x1 = c(-1e6, 1e6), x2 = c(1e6, -1e6)))
  expect_equal(unname(c(min(pr), max(pr))), c(min(d$y), max(d$y)))
}
test_predict_filter_learner()
