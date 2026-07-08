# Superlearner (stacked/ensemble learner)

This function creates a predictor object (class [learner](learner.md))
from a list of existing [learner](learner.md) objects. When estimating
this model a stacked prediction will be created by weighting together
the predictions of each of the initial learners The weights are learned
using cross-validation.

## Usage

``` r
superlearner(
  learners,
  data,
  nfolds = 10,
  meta.learner = metalearner_nnls,
  model.score = mse,
  mc.cores = NULL,
  future.seed = TRUE,
  silent = TRUE,
  name.prefix = NULL,
  ...
)
```

## Arguments

- learners:

  (list) List of [learner](learner.md) objects (i.e.
  [learner_glm](learner_glm.md))

- data:

  (data.frame) Data containing the response variable and covariates.

- nfolds:

  (integer) Number of folds to use in cross-validation to estimate the
  ensemble weights.

- meta.learner:

  (function) Algorithm to learn the ensemble weights (default
  non-negative least squares). Must be a function of the response (nx1
  vector), `y`, and the predictions (nxp matrix), `pred`, with p being
  the number of learners. Alternatively, this can be set to the
  character value "discrete", in which case the Discrete Super-Learner
  is applied where the model with the lowest risk (model-score) is given
  weight 1 and all other learners weight 0.

- model.score:

  (function) Model scoring method (see [learner](learner.md))

- mc.cores:

  (integer) If not NULL, then
  [parallel::mcmapply](https://rdrr.io/r/parallel/mclapply.html) is used
  with `mc.cores` number of cores for parallelization instead of the
  [future.apply::future_lapply](https://future.apply.futureverse.org/reference/future_lapply.html)
  package. Parallelization is disabled with `mc.cores = 1`.

- future.seed:

  (logical or integer) Argument passed on to
  [future.apply::future_lapply](https://future.apply.futureverse.org/reference/future_lapply.html).
  If TRUE, then [.Random.seed](https://rdrr.io/r/base/Random.html) is
  used if it holds a L'Ecuyer-CMRG RNG seed, otherwise one is created
  randomly.

- silent:

  (logical) Suppress all messages and progressbars

- name.prefix:

  (character) Prefix used to name learner objects in `learners` without
  names. If NULL, then obtain the name from the info field of a learner.

- ...:

  Additional arguments to
  [parallel::mclapply](https://rdrr.io/r/parallel/mclapply.html) or
  [future.apply::future_lapply](https://future.apply.futureverse.org/reference/future_lapply.html).

## References

Luedtke & van der Laan (2016) Super-Learning of an Optimal Dynamic
Treatment Rule, The International Journal of Biostatistics.

## See also

[predict.superlearner](predict.superlearner.md)
[weights.superlearner](weights.superlearner.md)
[score.superlearner](score.superlearner.md)

## Examples

``` r
sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
   data.frame(y, x1, x2)
}
m <- list(
  "mean" = learner_glm(y ~ 1),
  "glm" = learner_glm(y ~ x1 + x2)
)
sl <- superlearner(m, data = sim1(), nfolds = 2)
predict(sl, newdata = sim1(n = 5))
#> [1] -0.6538208  2.3619358  0.0198510 -1.7237326 -0.6763111
predict(sl, newdata = sim1(n = 5), all.learners = TRUE)
#>         mean        glm
#> 1 0.05269828  1.8946877
#> 2 0.05269828 -1.9981212
#> 3 0.05269828 -0.2000841
#> 4 0.05269828 -4.4679193
#> 5 0.05269828 -0.3890295
```
