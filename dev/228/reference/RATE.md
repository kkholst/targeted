# Responder Average Treatment Effect

Estimation of the Average Treatment Effect among Responders

## Usage

``` r
RATE(
  response,
  post.treatment,
  treatment,
  data,
  M = 5,
  pr.treatment,
  treatment.level,
  preprocess = NULL,
  efficient = TRUE,
  ...
)
```

## Arguments

- response:

  (formula or learner) Response model. A formula (e.g., `Y ~ D*A`) is
  wrapped in [learner_glm](learner_glm.md) with a Gaussian family.

- post.treatment:

  (formula or learner) Post treatment marker model. A formula (e.g.,
  `D ~ W`) is wrapped in [learner_glm](learner_glm.md) with a binomial
  family.

- treatment:

  Treatment formula (e.g, A ~ 1)

- data:

  data.frame

- M:

  Number of folds in cross-fitting (M=1 is no cross-fitting)

- pr.treatment:

  (optional) Randomization probability of treatment.

- treatment.level:

  Treatment level in binary treatment (default 1)

- preprocess:

  (optional) Data preprocessing function

- efficient:

  If TRUE, the estimate will be efficient. If FALSE, the estimate will
  be a simple plug-in estimate.

- ...:

  Additional arguments to lower level functions

## Value

estimate object

## Author

Andreas Nordland, Klaus K. Holst
