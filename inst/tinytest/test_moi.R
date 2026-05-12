
test_moi <- function() {

  simdata <- function(n, full = FALSE) {
    w <- rnorm(n) # unmeasured baseline covariate
    x <- rnorm(n) - 0.5 * w # baseline covariate
    a <- rbinom(n, 1, 0.5)    # treatment
    z <- x + a * w^2 + (1-a) * sin(w) + rnorm(n) # post randomization variable
    delta <- rbinom(n = n, size = 1, prob = lava::expit(2 + z)) # non-missingness indicator
    y <- 1 + a + x - a * x + w + a * w + z + rnorm(n)           # outcome
    y <- ifelse(delta == 1, y, NA)
    d <- data.frame(id = n:1, y = y, z = z, a = a, x = x)
    if(full == TRUE) {
      d <- cbind(d, w = w)
    }
    return(d)
  }

  data <- simdata(1e3)
  delta <- !is.na(data$y)

  ghat <- mean(data$a)
  pred_degen <- function(object = NULL, newdata, type = NULL) {
    a <- newdata[ , "a"]
    x <- newdata[ , "x"]
    z <- newdata[ , "z"]

    a * (x + z) + (1 - a) * (x^2 - z)
  }
  uhat <- pred_degen(newdata = data)


  learner_degen <- learner$new(estimate = function(y, x){"degenerate"},
                               formula = ~ .,
                               predict = pred_degen)

  tmp <- learner_degen
  class(tmp) <- c("learner_glm", class(tmp))

  out <- targeted:::moi_missing(data = data,
                                delta = delta,
                                treatment.model = learner_glm(a ~ 1, family = binomial()),
                                imputation.model = tmp,
                                imputation.subset = "!is.na(y)")

}
