# Signed Wald intersection test

Calculating test statistics and p-values for the signed Wald
intersection test given by \$\$SW = \inf\_{\theta \in \cap\_{i=1}^n H_i}
\\(\widehat{\theta}-\theta)^\top W\widehat{\Sigma}W
(\widehat{\theta}-\theta)\\ \$\$ with individual hypotheses for each
coordinate of \\\theta\\ given by \\H_i: \theta_j \< \delta_j\\ for some
non-inferiority margin \\\delta_j\\, \\j=1,\ldots,n\\. \#

## Usage

``` r
test_intersection_sw(
  par,
  vcov,
  noninf = 0,
  weights = 1,
  nsim.null = 10000,
  index = NULL,
  control = list(),
  par.name = "theta"
)
```

## Arguments

- par:

  (numeric) parameter estimates or `estimate` object

- vcov:

  (matrix) asymptotic variance estimate

- noninf:

  (numeric) non-inferiority margins

- weights:

  (numeric) optional weights

- nsim.null:

  (integer) number of sample used in Monte-Carlo simulation

- index:

  (integer) subset of parameters to test

- control:

  (list) arguments to alternating projection algorithm. See details
  section.

- par.name:

  (character) parameter names in output

## Value

`htest` object

## Details

The constrained least squares problem is solved using Dykstra's
algorithm. The following parameters for the optimization can be
controlled via the `control` list argument: `dykstra_niter` sets the
maximum number of iterations (default 500), `dykstra_tol` convergence
tolerance of the alternating projection algorithm (default 1e-7),
`pinv_tol` tolerance for calculating the pseudo-inverse matrix (default
length(par)*.Machine\$double.eps*max(eigenvalue)).

## References

Christian Bressen Pipper, Andreas Nordland & Klaus Kähler Holst (2025) A
general approach to construct powerful tests for intersections of
one-sided null-hypotheses based on influence functions. arXiv:
https://arxiv.org/abs/2511.07096.

## See also

[test_zmax_onesided](test_zmax_onesided.md)
[lava::test_wald](https://kkholst.github.io/lava/reference/compare.html)
[lava::closed_testing](https://kkholst.github.io/lava/reference/closed_testing.html)

## Author

Klaus Kähler Holst, Christian Bressen Pipper

## Examples

``` r
S <- matrix(c(1, 0.5, 0.5, 2), 2, 2)
thetahat <- c(0.5, -0.2)
test_intersection_sw(thetahat, S, nsim.null = 1e5)
#> 
#>  Signed Wald Intersection Test
#> 
#> data:  
#> Intersection null hypothesis: theta =< [0, 0]
#> w = [0.5, 0.5]
#> Q = 0.0625, p-value = 0.4797
#> 
test_intersection_sw(thetahat, S, weights = NULL)
#> 
#>  Signed Wald Intersection Test
#> 
#> data:  
#> Intersection null hypothesis: theta =< [0, 0]
#> w = [0.5, 0.5]
#> Q = 0.0625, p-value = 0.4638
#> 

if (FALSE) { # \dontrun{
# only on 'lava' >= 1.8.2
e <- estimate(coef = thetahat, vcov = S, labels = c("p1", "p2"))
lava::closed_testing(e, test_intersection_sw, noninf = c(-0.1, -0.1)) |>
  summary()
} # }
```
