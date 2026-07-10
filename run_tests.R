# run_tests.R
#
# Runs the full unit test suite for the R/ helper modules.
# Usage: Rscript run_tests.R

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

results <- test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)

failed <- sum(vapply(results, function(r) sum(r$failed) > 0, logical(1)))
if (failed > 0) {
  quit(status = 1)
}
