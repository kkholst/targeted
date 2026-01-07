# SuperLearner wrapper for learner

SuperLearner wrapper for learner

## Usage

``` r
SL(
  formula = ~.,
  ...,
  SL.library = c("SL.mean", "SL.glm"),
  binomial = FALSE,
  data = NULL,
  info = "SuperLearner"
)
```

## Arguments

- formula:

  Model design

- ...:

  Additional arguments for SuperLearner::SuperLearner

- SL.library:

  character vector of prediction algorithms

- binomial:

  boolean specifying binomial or gaussian family (default FALSE)

- data:

  Optional data.frame

- info:

  model information (optional)

## Value

learner object

## Author

Klaus Kähler Holst
