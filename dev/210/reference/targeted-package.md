# targeted: Targeted Inference

Various methods for targeted and semiparametric inference including
augmented inverse probability weighted (AIPW) estimators for missing
data and causal inference (Bang and Robins (2005)
[doi:10.1111/j.1541-0420.2005.00377.x](https://doi.org/10.1111/j.1541-0420.2005.00377.x)
), variable importance and conditional average treatment effects (CATE)
(van der Laan (2006)
[doi:10.2202/1557-4679.1008](https://doi.org/10.2202/1557-4679.1008) ),
estimators for risk differences and relative risks (Richardson et al.
(2017)
[doi:10.1080/01621459.2016.1192546](https://doi.org/10.1080/01621459.2016.1192546)
), assumption lean inference for generalized linear model parameters
(Vansteelandt et al. (2022)
[doi:10.1111/rssb.12504](https://doi.org/10.1111/rssb.12504) ).

## References

Bang & Robins (2005) Doubly Robust Estimation in Missing Data and Causal
Inference Models, Biometrics.

Vansteelandt & Dukes (2022) Assumption-lean inference for generalised
linear model parameters, Journal of the Royal Statistical Society:
Series B (Statistical Methodology).

Thomas S. Richardson, James M. Robins & Linbo Wang (2017) On Modeling
and Estimation for the Relative Risk and Risk Difference, Journal of the
American Statistical Association.

Mark J. van der Laan (2006) Statistical Inference for Variable
Importance, The International Journal of Biostatistics.

## See also

Useful links:

- <https://kkholst.github.io/targeted/>

- <https://github.com/kkholst/targeted>

- Report bugs at <https://github.com/kkholst/targeted/issues>

## Author

**Maintainer**: Klaus K. Holst <klaus@holst.it>

Authors:

- Klaus K. Holst <klaus@holst.it>

- Benedikt Sommer <benediktsommer92@gmail.com>

- Andreas Nordland <andreasnordland@gmail.com>

Other contributors:

- Christian B. Pipper <cbpipper@gmail.com> \[contributor\]

Klaus K. Holst (Maintainer) <klaus@holst.it>

## Examples

``` r
if (FALSE) { # \dontrun{
example(riskreg)
example(cate)
example(ate)
example(calibration)
} # }
```
