    :Author: Klaus Holst

.. contents::



Quickstart
----------

.. code:: R

    summary(lm(Sepal.Length ~ Petal.Length, iris))

::


    Call:
    lm(formula = Sepal.Length ~ Petal.Length, data = iris)

    Residuals:
         Min       1Q   Median       3Q      Max
    -1.24675 -0.29657 -0.01515  0.27676  1.00269

    Coefficients:
                 Estimate Std. Error t value Pr(>|t|)
    (Intercept)   4.30660    0.07839   54.94   <2e-16 ***
    Petal.Length  0.40892    0.01889   21.65   <2e-16 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    Residual standard error: 0.4071 on 148 degrees of freedom
    Multiple R-squared:   0.76,	Adjusted R-squared:  0.7583
    F-statistic: 468.6 on 1 and 148 DF,  p-value: < 2.2e-16

.. figure:: testfig.png

    ... `fig:testfig <fig:testfig>`_
