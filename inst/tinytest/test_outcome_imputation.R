library("tinytest")

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

set.seed(1)
d <- simdata(n = 1e3)


## true target parameter, E[U(X,A,Z)|A = a, \Delta = 0],
## U(X,A,Z): linear model of x, z among the non-missing in
## the reference treatment arm, a = 0

## approx_target <- function(n = 1e6) {
##   data <- simdata(n = n, full = TRUE)
##   ## approximating the true imputation model u: y ~ x + z in a = 0
##   data$q <- with(data = data, 1 + a + x - a * x + w + a * w + z)
##   imp <- learner_glm(q ~ x + z, family = gaussian())
##   imp$estimate(data = data[!is.na(data$y) & data$a == 0,])
##   data$u <- imp$predict(data, type = "response")

##   target <- sapply(
##     X = c(1,0),
##     FUN = function(a) {
##       mean(data[!is.na(data$y) & data$a == a, ]$u)
##     }
##   )
## }
## set.seed(1)
## target0 <- approx_target(n = 1e7)
target0 <- c(2.768475, 1.449266)
target0[1] - target0[2]

moiate(data = d,
       response.model = y ~ a * x,
       propensity.model = learner_glm(a ~ 1, family = "binomial"),
       missing.model = tmp ~ a * x,
       imputation.model = y ~ x + z,
       imputation.subset = "!is.na(y) & a == 0",
       return.all = FALSE)
