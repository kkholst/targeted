# Calculate the right censoring augmentation integral

For a user defined function \\H(u\|X)\\, computes the integral
\\\int_0^\tau \frac{H(u)\|X}{S^c}\\ dM^c(u\|X), where \$S^c\$ is the
censoring time survival function and \$M^c\$ is the censoring is the
right censoring martingale with the Doob-Meyer decomposition \\M^c =
N^c - L^c\\, where \\N^c\\ is the counting process \\N^c(s) = I\\\tilde
T \leq s \Delta = 0\\\\ and \\L^c\\ is the compensator \\L^c(s) =
\int_0^s I \\\tilde T \geq u\\ d\Lambda^c(u\|X)\\.

## Usage

``` r
rcai(
  T_model,
  C_model,
  data,
  time,
  event,
  tau,
  H_constructor,
  sample = 0,
  blocksize = 0,
  return_all = FALSE,
  ...
)
```

## Arguments

- T_model:

  model for event time

- C_model:

  model for censoring

- data:

  data.frame

- time:

  time variable

- event:

  event variable

- tau:

  stopping time

- H_constructor:

  function H(u\|X)

- sample:

  approximate integral by subsampling jump-times

- blocksize:

  evaluate cumhaz in chunks of size blocksize

- return_all:

  if TRUE then bot counting process N and compensator term L are
  returned

- ...:

  additional arguments passed to lower level functions

## Value

vector with integral from 0 to all jump-times

## Author

Andreas Nordland
