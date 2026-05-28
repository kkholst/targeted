# Shared test utilities.
# Sourced explicitly by test files that need these helpers; this file is
# intentionally not prefixed with "test_" so run_test_dir() never executes it
# as a standalone test file.

# Captures log lines emitted via the logger package while evaluating expr.
# Returns a character vector of the captured lines.
capture_warn_logs <- function(expr) {
  msgs <- character(0L)
  old_appender <- eval(
    logger::log_appender(),
    envir = getNamespace("logger")
  )
  on.exit(logger::log_appender(old_appender))
  logger::log_appender(function(line) msgs <<- c(msgs, line))
  force(expr)
  msgs
}
