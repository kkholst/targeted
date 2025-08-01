library(tinytest)

set.seed(42)
n <- 1e2
ddata <- data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))

# testing the basic functionality
test_design <- function() {
  # test adding intercept
  dd <- design(y ~ x1, ddata, intercept = TRUE)

  dd_expect <- matrix(
    cbind(1, ddata$x1),
    nrow = n,
    dimnames = list(rep(1, n), c("(Intercept)", "x1"))
  )

  expect_equivalent(dd_expect, dd$x)
  # test colnames separately because expect_equivalent doesn't compare them
  expect_equal(colnames(dd_expect), colnames(dd$x))
  # outcome element is populated correctly
  yy <- ddata$y
  names(yy) <- seq(n)
  expect_equal(yy, dd$y)

  # intercept is not added even when specified in formula
  dd <- design(y ~ 1 + x1, ddata)
  expect_equivalent(as.matrix(ddata$x1), dd$x)

  # intercept is not added with intercept argument
  dd <- design(y ~ - 1 + x1, ddata, intercept = TRUE)
  expect_equal(colnames(dd$x), "x1")

  # raise error when specifying a variable inside the formula that doesn't
  # exist in data
  expect_error(
    design(y ~ notfound, ddata),
    pattern = "object 'notfound' not found"
  )

  # test output class and attributes
  expect_true(inherits(dd, "design"))
  # data attribute contains sample data and terms
  expect_equal(dd$data, ddata[0, ])
  expect_true("terms" %in% names(dd))
  # terms includes environment
  expect_true(".Environment" %in% names(attributes(dd$terms)))
  # environment is successfully removed from terms
  dd <- design(y ~ x1, ddata, rm_envir = TRUE)
  expect_false(".Environment" %in% names(attributes(dd$terms)))
}
test_design()

# test that ellipsis are passed on to model.frame
test_design_ellipsis <- function() {
  expect_error(
    design(y ~ x1, data = ddata, subset = seq(10)),
    pattern = "subset is not an allowed specials argument"
  )

  # no error when argument is a vector of length n
  dd <- design(y ~ x1, ddata, nocolumn = rep(1, n))
  expect_equal(rep(1, n), unname(dd$nocolumn))

  # fails because column cannot be added with model.frame because of differing
  # lengths
  expect_error(
    design(y ~ x1, ddata, nocolumn = rep(1, 5)),
    pattern = "variable lengths differ"
  )
}
test_design_ellipsis()

# test specials argument
test_design_specials <- function() {
  # offset is correctly identified as a special variable and not added as a
  # covariate
  dd <- design(y ~ offset(x1), ddata, specials="offset")

  expect_equal(ncol(dd$x), 0)
  offset_expect <- ddata$x1
  names(offset_expect) <- seq(n)
  expect_equal(dd$offset, offset_expect)

  # offset is not identified correctly because it is not defined in specials
  dd <- design(y ~ offset(x1), ddata, specials = c("empty"))
  expect_false("offset" %in% names(dd))

  # an offset variable is not changed
  ddata1 <- ddata
  ddata1$offset <- 1
  dd <- design(y ~ offset + x1, ddata1, specials="offset")
  expect_equivalent(
    as.matrix(ddata1[, c("offset", "x1")]),
    dd$x
  )

  # specifying a variable accidentally doesn't have an effect
  dd <- design(y ~ x1, ddata, specials = "x1")
  expect_equivalent(as.matrix(ddata$x1), dd$x)

  # a user defined specials is handled correctly
  sq <- identity # nolint
  dd <- design(y ~ x1 + sq(x1), ddata, specials = "sq")
  expect_equal(dd$x[, 1], dd$sq)

  # formula with only one variable, which is a special
  dd <- design(y ~ sq(x1), ddata, specials = "sq")
  expect_equal(ddata$x1, unname(dd$sq))

  # with two specials
  dd <- design(y ~ sq(x1) + offset(x1), ddata, specials = c("sq", "offset"))
  expect_equal(ddata$x1, unname(dd$sq))
  expect_equal(ddata$x1, unname(dd$offset))

  # test default weight special
  dd <- design(y ~ weights(x1), ddata, specials="weights")
  expect_equal(unname(dd$weights), ddata$x1)

  # specials and additional predictor
  des <- design(y ~ offset(x1) + x2, specials="offset", data=ddata)
  expect_equivalent(des$x, cbind(ddata$x2))
  expect_equivalent(des$offset, ddata$x1)
  expect_equivalent(des$y, ddata$y)
  expect_equivalent(update(des, head(ddata))$offset, head(ddata)$x1)

  # irespective of order in formula
  des <- design(y ~ x2 + offset(x1), specials="offset", data=ddata)
  expect_equivalent(des$x, cbind(ddata$x2))
  expect_equivalent(des$offset, ddata$x1)
}
test_design_specials()

# specials returning factor
etest_design_specials_factor <- function() {
  strata <- survival::strata
  dat <- transform(ddata, a=rbinom(nrow(ddata), 1, 0.5))
  des <- design(y ~ strata(a) + x1*x2, data=dat, specials="strata")

  expect_equivalent(des$x, cbind(dat$x1))
  expect_equivalent(as.numeric(des$strata)-1, dat$a)

}


# test behavior of design when formula specifies transformations
test_design_transformations <- function() {
    # interactions work as expected
  dd <- design(y ~ x1 * x2 + I(x1 ** 2), ddata)
  dd_expect <- cbind(
    ddata[, c("x1", "x2")], ddata$x1 ** 2, ddata$x1 * ddata$x2
  ) |> as.matrix()
  colnames(dd_expect) <- c("x1", "x2", "I(x1^2)", "x1:x2")
  expect_equivalent(dd_expect, dd$x)
  expect_equal(colnames(dd_expect), colnames(dd$x))

  # transformation also work for target variables
  dd <- design(I(y > 0) ~ x1, ddata)
  expect_equivalent(dd$y > 0, dd$y)

  foo <- cos
  dd <- design(y ~ foo(x1), ddata)
  # works also with arbitrary transformations
  expect_equivalent(as.matrix(foo(ddata$x1)), dd$x)
  expect_equal(colnames(dd$x), "foo(x1)")

  # NAs that result from transformation are removed
  dd <- suppressWarnings(design(y ~ log(x1), ddata))
  dd_expect <- suppressWarnings(log(ddata$x1))
  dd_expect <- dd_expect[!is.na(dd_expect)]
  expect_equivalent(dd$x, as.matrix(dd_expect))
}
test_design_transformations()

# test behavior of design for handling NA values in data
test_design_na_handling <- function() {
  # handle NA values
  ddata_na <- ddata
  ddata_na[1, "x1"] <- NA

  dd <- design(y ~ x1 * x2, ddata_na)
  dd_expect <- cbind(ddata_na[, c("x1", "x2")], ddata_na$x1 * ddata_na$x2) |>
    as.matrix()
  # remove rows with NAs
  expect_equivalent(dd$x, dd_expect[-1, ])
  # same behavior without interactions
  dd <- design(y ~ x1 + x2, ddata_na)
  expect_equivalent(dd$x, as.matrix(ddata[-1, c("x1", "x2")]))
}
test_design_na_handling()

# test behavior of design with factor variables
test_design_factor <- function() {
  ddata_fact <- ddata
  ddata_fact$x3 <- as.factor(rep(c("a", "b"), length.out = n))
  # removing intercept in formula ensures that both levels are added as
  # covariates
  dd <- design(y ~ -1 + x3, ddata_fact, intercept = TRUE)

  # factors levels are collected in xlevels attribute
  expect_equal(dd$xlevels, list(x3 = c("a", "b")))
  # factors are one-hot encoded
  dd_expect <- cbind(
    rep(c(1, 0), length.out = n),
    rep(c(0, 1), length.out = n)
  ) |> as.matrix()
  expect_equivalent(dd_expect, dd$x)
  expect_equal(paste0("x3", c("a", "b")), colnames(dd$x))

  # interactions with numerical values work as expected
  dd <- design(y ~ -1 + x3:x1, ddata_fact)
  dd_expect_inter <- dd_expect * matrix(rep(ddata$x1, 2), nrow = n)
  expect_equivalent(dd_expect_inter, dd$x)
  nn <- paste0("x3", c("a", "b"))
  expect_equal(paste0(nn, ":x1"), colnames(dd$x))

  # characters are automatically converted to factors
  ddata_fact$x4 <- rep(c("a", "b"), length.out = n)
  dd <- design(y ~ -1 + x4, ddata_fact)
  expect_equivalent(dd_expect, dd$x)

  dd <- design(y ~ -1 + x3:x4, ddata_fact)
  dd_expect_inter2 <- cbind(
    dd_expect * matrix(c(1, 0), nrow = n, ncol = 2), # x4a
    dd_expect * matrix(c(0, 1), nrow = n, ncol = 2) # x4b
  )
  expect_equivalent(dd_expect_inter2, dd$x)

  # not removing the intercept in formula removes the first level of the factor
  dd <- design(y ~ x3, ddata_fact)
  expect_equal(unname(dd$x[, 1]), rep(c(0, 1), length.out = nrow(dd$x)))
  expect_equal(colnames(dd$x), "x3b")

  # order of levels can be controlled with xlev argument
  dd <- design(y ~ x3, ddata_fact, xlev = list(x3 = c("b", "a")))
  expect_equal(colnames(dd$x), "x3a")
}
test_design_factor()

# test update.design s3 method
test_update.design <- function() {
  dd <- design(y ~ x1, ddata)
  dd_upd <- update(dd, head(ddata, 10))
  expect_equal(unname(dd_upd$x[, 1]), ddata$x1[1:10])
  expect_equal(names(dd_upd$x[, 1]), as.character(1:10))

  # return design object without data when data = NULL
  dd_upd <- update(dd)
  expect_equal(nrow(dd_upd$x), 0)
  expect_equal(colnames(dd_upd$x), "x1")

  # specials are updated correctly
  sq <- \(x) x ** 2
  dd <- design(y ~ sq(x1), ddata, specials = "sq")
  dd_upd <- update(dd, head(ddata, 10))
  expect_equal(unname(dd_upd$sq), sq(ddata$x1[1:10]))

  # returned design object can be updated again
  dd_upd <- update(dd_upd, head(ddata, 20))
  expect_equal(unname(dd_upd$sq), sq(ddata$x1[1:20]))

  # intercept is kept also when updating the design object
  dd <- design(y ~ x1, ddata, intercept = TRUE)
  dd_upd <- update(dd, head(ddata, 10))
  dd_expect <- cbind(1, head(ddata, 10)$x1) |> as.matrix()
  expect_equivalent(dd_expect, dd_upd$x)
}
test_update.design()

# test update.design with factors
test_update.design.factors <- function() {
  ddata_fact <- ddata
  ddata_fact$x3 <- as.factor(rep(c("a", "b"), length.out = n))
  dd <- design(y ~ x3, ddata_fact)

  # works as expected when new data contains both levels
  dd_upd <- update(dd, head(ddata_fact, 10))
  expect_equal(unname(dd_upd$x[, 1]), rep(c(0, 1), length.out = 10))
  expect_equal(colnames(dd_upd$x), "x3b")

  # new data does not contain some levels that the original data contains.
  # in this case ddata_fact does not contain factor "b"
  dd_upd <- update(dd, head(ddata_fact, 1))
  expect_equal(dd_upd$x[, 1], 0)
  expect_equal(colnames(dd_upd$x), "x3b")

  # also works when characters are internally converted to factors
  dd <- design(y ~ x3, ddata_fact)
  ddata_fact$x3 <- rep(c("a", "b"), length.out = n)
  dd_upd <- update(dd, head(ddata_fact, 1))

  # expect error when new data contains different levels
  newdata <- ddata_fact
  newdata$x3 <- rep(c("a", "c"), length.out = n)
  expect_error(
    update(dd, newdata),
    pattern = "factor x3 has new levels c"
  )

  # intercept is handled correctly for factors
  dd <- design(y ~ -1 + x3, ddata_fact, intercept = TRUE)
  dd_upd <- update(dd, head(ddata_fact))
  expect_equivalent(dd_upd$x, head(dd$x))

  # same as above
  dd <- design(y ~ -1 + x3, ddata_fact)
  dd_upd <- update(dd, head(ddata_fact))
  expect_equivalent(dd_upd$x, head(dd$x))
}
test_update.design.factors()

test_model.matrix.design <- function() {
  dd <- design(y ~ x1, ddata)
  # simply return x attribute
  expect_equal(model.matrix(dd), dd$x)

  # also works when no covariates are specified on RHS
  dd <- design(y ~ -1, ddata)
  expect_equal(model.matrix(dd), dd$x)
}
test_model.matrix.design()
