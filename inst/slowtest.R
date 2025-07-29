library("targeted")
if (Sys.getenv("GITHUB_RUN_ID") == "") {
  future::plan("multicore")
}
res <- tinytest::run_test_dir("inst/slowtest")
if (summary(res)["Total", "fails"] > 0) {
  print(res)
  quit(status = 2)
}
