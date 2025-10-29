
if (requireNamespace("tinytest", quietly = TRUE)) {

  ## future::plan("sequential")
  Sys.setenv("OMP_THREAD_LIMIT" = 2)
  options(Ncpus = 1)
  data.table::setDTthreads(1)

  tinytest::test_package("targeted")
}
