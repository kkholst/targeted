# Targeted Learning Library

Python package for targeted inference.

**targeted** provides a number of methods for semi-parametric
estimation.  The library also contains implementations of various
parametric models (including different discrete choice models) and
model diagnostics tools.

The implemention currently includes
- **Risk regression models** with binary exposure
  (Richardson et al., 2017, doi:10.1080/01621459.2016.1192546)
- **Augmented Inverse Probability Weighted** estimators for missing
  data and causal inference (Bang and Robins, 2005,
  doi:10.1111/j.1541-0420.2005.00377.x)
- Model diagnostics based on **cumulative residuals** methods
- Efficient weighted **Pooled Adjacent Violator Algorithms**
- **Nested multinomial logit** models

Documentation and tutorials can be found at https://targetlib.org/python/.
