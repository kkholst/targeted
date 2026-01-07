# Estimation of mean clinical outcome truncated by event process

Let \\Y\\ denote the clinical outcome, \\A\\ the binary treatment
variable, \\X\\ baseline covariates, \\T\\ the failure time, and
\\epsilon=1,2\\ the cause of failure. The following are our two target
parameters \$\$E(Y\|T\>t, A=1)- E(Y\|T\>t, A=0)\$\$
\$\$P(T\<t,\epsilon=1\|A=1)- P(T\<t,\epsilon=1\|A=0)\$\$

## Usage

``` r
estimate_truncatedscore(
  data,
  mod.y,
  mod.r,
  mod.a,
  mod.event,
  time,
  cause = NULL,
  cens.code = 0,
  naive = FALSE,
  control = list(),
  ...
)
```

## Arguments

- data:

  (data.frame)

- mod.y:

  (formula or learner) Model for clinical outcome given T\>time. Using a
  formula specifies a glm with an identity link (see example).

- mod.r:

  (formula or learner) Model for missing data mechanism for clinical
  outcome at T=time. Using a formula specifies a glm with a log link.

- mod.a:

  (formula or learner) Treatment model (in RCT should just be 'a ~ 1').
  Using a formula specifies a glm with a log link.

- mod.event:

  (formula) Model for time-to-event process ('Event(time,status) ~ x').

- time:

  (numeric) Landmark time.

- cause:

  (integer) Primary event (in the 'status' variable of the 'Event'
  statement).

- cens.code:

  (integer) Censoring code.

- naive:

  (logical) If TRUE, the unadjusted estimates ignoring baseline
  covariates is returned as the attribute 'naive'.

- control:

  (list) optimization routine parameters.

- ...:

  Additional arguments passed to
  [mets::binregATE](http://kkholst.github.io/mets/reference/binregATE.md).

## Value

[lava::estimate.default](http://kkholst.github.io/lava/reference/estimate.default.md)
object

## Author

Klaus Kähler Holst

## Examples

``` r
data(truncatedscore)
mod1 <- learner_glm(y ~ a * (x1 + x2))
mod2 <- learner_glm(r ~ a * (x1 + x2), family = binomial)
a <- estimate_truncatedscore(
  data = truncatedscore,
  mod.y = mod1,
  mod.r = mod2,
  mod.a = a ~ 1,
  mod.event = mets::Event(time, status) ~ x1+x2,
  time = 2
)
s <- summary(a, noninf.t = -0.1)
print(s)
#> 
#> 
#> ── Parameter estimates ──
#> 
#>                Estimate  Std.Err      2.5%    97.5%   P-value
#> E(Y|T>2.0,A=0) 41.24803 0.392781 40.478191 42.01787 0.000e+00
#> E(Y|T>2.0,A=1) 43.49447 0.401564 42.707423 44.28153 0.000e+00
#> diff            2.24645 0.462438  1.340083  3.15281 1.187e-06
#> ──────────────                                               
#> P(T>2.0|A=0)    0.87934 0.007674  0.864303  0.89439 0.000e+00
#> P(T>2.0|A=1)    0.90358 0.007130  0.889609  0.91756 0.000e+00
#> riskdiff        0.02424 0.010446  0.003766  0.04471 2.032e-02
#> 
#> ── One-sided tests ──
#> 
#> 
#> b1 = E(Y|T>2.0,A=1) - E(Y|T>2.0,A=0)
#> 
#>  Signed Wald Test
#> 
#> data:  H0: b1 =< 0
#> Q = 23.598, p-value = 5.934e-07
#> alternative hypothesis: HA: b1 > 0
#> sample estimates:
#>       b1 
#> 2.246446 
#> 
#> 
#> b2 = P(T>2.0|A=1) - P(T>2.0|A=0)
#> 
#>  Signed Wald Test
#> 
#> data:  H0: b2 =< -0.1
#> Q = 141.46, p-value < 2.2e-16
#> alternative hypothesis: HA: b2 > -0.1
#> sample estimates:
#>         b2 
#> 0.02423915 
#> 
#> ── Intersection test ──
#> 
#> 
#>  Signed Wald Intersection Test
#> 
#> data:  
#> Intersection null hypothesis: b =< [0, -0.1]
#> w = [0.5, 0.5]
#> Q = 44.067, p-value < 2.2e-16
#> 
parameter(s)
#>                estimate statistic      p.value
#> b1           2.24644561  23.59850 5.934004e-07
#> b2           0.02423915 141.45721 6.390524e-33
#> intersection         NA  44.06709 0.000000e+00

# the above is equivalent to
# a <- estimate_truncatedscore(
#   data = truncatedscore,
#   mod.y = y ~ a * (x1 + x2),
#   mod.r = r ~ a * (x1 + x2),
#   mod.a = a ~ 1,
#   mod.event = mets::Event(time, status) ~ x1+x2,
#   time = 2
# )
```
