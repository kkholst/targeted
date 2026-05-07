# One-sided Zmax test

Calculating test statistics and p-values for the onesided Zmax / minP
test.z

Given parameter estimates \\(\widehat{\theta}\_1, \ldots,
\widehat{\theta}\_p)^\top\\ with approximate assymptotic covariance
matrix \\\widehat{S}\\, let \\ Z_i = \frac{\widehat{\theta}\_i -
\delta_i}{\operatorname{SE}(\widehat{\theta}\_i)}\\ , where
\\\operatorname{SE}(\widehat{\theta}\_i) = \widehat{S}\_{ii}\\. The Zmax
test statistic is then \\Z\_{max} = \max \\Z_1,\ldots,Z_p\\\\, and the
null-hypothesis is \\H_0: \theta_i \leq \delta_i, i=1,\ldots,p\\ with
non-inferiority margin \\\delta_i, i=1,\ldots,p\\, for which the p-value
is calculated as \\ 1 - \Phi_R(Z\_{max}) \\ where \\\phi_R\\ is the CDF
of the multivariate normal distribution with mean zero and correlation
matrix \\R = \operatorname{diag}(S\_{11}^{-0.5}, \ldots,
S\_{pp}^{-0.5})S\operatorname{diag}(S\_{11}^{-0.5}, \ldots,
S\_{pp}^{-0.5})\\.

## Usage

``` r
test_zmax_onesided(par, vcov, noninf = 0, index = NULL, par.name = "theta")
```

## Arguments

- par:

  (numeric) parameter estimates or `estimate` object

- vcov:

  (matrix) asymptotic variance estimate

- noninf:

  (numeric) non-inferiority margins

- index:

  (integer) subset of parameters to test

- par.name:

  (character) parameter names in output

## Value

`htest` object

## See also

[`test_intersection_sw()`](test_intersection_sw.md)
[`lava::test_wald()`](https://kkholst.github.io/lava/reference/compare.html)
[`lava::closed_testing()`](https://kkholst.github.io/lava/reference/closed_testing.html)

## Author

Christian Bressen Pipper, Klaus Kähler Holst
