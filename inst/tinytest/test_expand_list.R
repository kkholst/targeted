

args <- list(
  formula = list(y ~ offset(x) + a:x, y ~ offset(x) + a),
  family = list(gaussian(), gaussian()),
  eta = 1:3,
  a = NULL
)

args2 <- list(
  formula = y ~ offset(x) + a:x,
  family = gaussian(),
  eta = 1:3,
  a = NULL
)

test_expand_list <- function() {
  res <- expand.list(INPUT = args)
  expect_true(12 == length(res))

  # check that the NULL is correctly handled
  for (i in seq_along(res)) {
    expect_true(is.null(res[[i]]$a))
  }

  # check formula works
  expect_true(inherits(res[[1]]$formula, "formula"))

  # check with non-vectors / non-list elements
  res2 <- expand.list(INPUT = args2)
  expect_true(3 == length(res2))
  expect_true(inherits(res2[[1]]$formula, "formula"))
  expect_true(is.null(res2[[1]]$a))
  expect_true(is.numeric(res2[[1]]$eta))
}
test_expand_list()
