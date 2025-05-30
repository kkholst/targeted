# use info field to generate names of returned list
lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3))
)
expect_equal(
  names(lrs),
  paste0("xgboost reg:squarederror", c("", ".1", ".2"))
)

# use "xgboost" instead of info field as pre-fix for names
lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3)),
  names = "xgboost"
)
expect_equal(
  names(lrs),
  paste0("xgboost", c("", ".1", ".2"))
)

# also add parameters to names
lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ ., eta = c(0.2)),
  names = "xgboost",
  params = TRUE
)
expect_equal(
  names(lrs), "xgboost:Sepal.Length ~ .:0.2"
)

# no names
lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3)),
  names = FALSE
)
expect_null(names(lrs))


# verify that learner.args can be passed on
lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ .,
    learner.args = c(
      list(predict.args = list(a = 1, b = 2)),
      list(predict.args = list(c = 3))
    )
  )
)
expect_equal(lrs[[1]]$summary()$predict.args, list(a = 1, b = 2))
expect_equal(lrs[[2]]$summary()$predict.args, list(c = 3))

# formula is expanded
lrs <- learner_expand_grid(
  learner_glm,
  list(formula = c(y ~ x, y ~ x + w))
)
expect_equal(lrs[[1]]$summary()$formula, y ~ x)
expect_equal(lrs[[2]]$summary()$formula, y ~ x + w)
