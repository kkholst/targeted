# Responder Average Treatment Effect

Estimation of the Average Treatment Effect among Responders for Survival
Outcomes

## Usage

``` r
RATE.surv(
  response,
  post.treatment,
  treatment,
  censoring,
  tau,
  data,
  M = 5,
  pr.treatment,
  call.response,
  args.response = list(),
  SL.args.post.treatment = list(family = binomial(), SL.library = c("SL.mean", "SL.glm")),
  call.censoring,
  args.censoring = list(),
  preprocess = NULL,
  ...
)
```

## Arguments

- response:

  Response formula (e.g., Surv(time, event) ~ D + W).

- post.treatment:

  Post treatment marker formula (e.g., D ~ W)

- treatment:

  Treatment formula (e.g, A ~ 1)

- censoring:

  Censoring formula (e.g., Surv(time, event == 0) ~ D + A + W)).

- tau:

  Time-point of interest, see Details.

- data:

  data.frame

- M:

  Number of folds in cross-fitting (M=1 is no cross-fitting)

- pr.treatment:

  (optional) Randomization probability of treatment.

- call.response:

  Model call for the response model (e.g. "mets::phreg").

- args.response:

  Additional arguments to the response model.

- SL.args.post.treatment:

  Arguments to SuperLearner for the post treatment indicator

- call.censoring:

  Similar to call.response.

- args.censoring:

  Similar to args.response.

- preprocess:

  (optional) Data preprocessing function

- ...:

  Additional arguments to lower level functions

## Value

estimate object

## Details

Estimation of \$\$ \frac{P(T \leq \tau\|A=1) - P(T \leq
\tau\|A=1)}{E\[D\|A=1\]} \$\$ under right censoring based on plug-in
estimates of \\P(T \leq \tau\|A=a)\\ and \\E\[D\|A=1\]\\.

An efficient one-step estimator of \\P(T \leq \tau\|A=a)\\ is
constructed using the efficient influence function \$\$
\frac{I\\A=a\\}{P(A = a)} \Big(\frac{\Delta}{S^c\_{0}(\tilde T\|X)}
I\\\tilde T \leq \tau\\ + \int_0^\tau
\frac{S_0(u\|X)-S_0(\tau\|X)}{S_0(u\|X)S^c_0(u\|X)} d M^c_0(u\|X)\Big)
\$\$ \$\$ + \Big(1 - \frac{I\\A=a\\}{P(A = a)}\Big)F_0(\tau\|A=a, W) -
P(T \leq \tau\|A=a). \$\$ An efficient one-step estimator of
\\E\[D\|A=1\]\\ is constructed using the efficient influence function
\$\$ \frac{A}{P(A = 1)}\left(D-E\[D\|A=1, W\]\right) + E\[D\|A=1, W\]
-E\[D\|A=1\]. \$\$

## Author

Andreas Nordland, Klaus K. Holst
