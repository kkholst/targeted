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
  SL.args.response = list(family = gaussian(), SL.library = c("SL.mean", "SL.glm")),
  SL.args.post.treatment = list(family = binomial(), SL.library = c("SL.mean", "SL.glm")),
  preprocess = NULL,
  efficient = TRUE,
  ...
)
```

## Arguments

- response:

  Response formula (e.g, Y ~ D\*A)

- post.treatment:

  Post treatment marker formula (e.g., D ~ W)

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

- SL.args.response:

  Arguments to SuperLearner for the response model

- SL.args.post.treatment:

  Arguments to SuperLearner for the post treatment indicator

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
