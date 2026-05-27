
if (requireNamespace("tinytest", quietly = TRUE)) {
  options(Ncpus = 1)
  data.table::setDTthreads(1)
  tinytest::test_package("targeted")
}
