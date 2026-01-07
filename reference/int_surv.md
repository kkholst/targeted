# Integral approximation of a time dependent function. Computes an approximation of \\\int_start^stop S(t) dt\\, where \\S(t)\\ is a survival function, for a selection of start and stop time points.

Integral approximation of a time dependent function. Computes an
approximation of \\\int_start^stop S(t) dt\\, where \\S(t)\\ is a
survival function, for a selection of start and stop time points.

## Usage

``` r
int_surv(times, surv, start = 0, stop = max(times), extend = FALSE)
```

## Arguments

- times:

  Numeric vector, sorted time points.

- surv:

  Numeric vector, values of a survival function evaluated at time points
  given by `times`.

- start:

  Numeric vector, start of the integral.

- stop:

  Numeric vector, end of the integral.

- extend:

  (logical) If TRUE, integral is extended beyond the last observed time
  point

## Value

Numeric vector, value of the integral.

## Author

Andreas Nordland
