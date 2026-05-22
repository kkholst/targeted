
if (requireNamespace("tinytest", quietly = TRUE)) {
  options(Ncpus = 1)
  data.table::setDTthreads(1)
  LOG_THRESHOLD <- logger::log_threshold()
  tinytest::test_package("targeted")
}
