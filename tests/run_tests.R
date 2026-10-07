#!/usr/bin/env Rscript

test_files <- sort(list.files(
  file.path("tests"),
  pattern = "^test_.*\\.R$",
  full.names = TRUE
))

if (length(test_files) == 0L) {
  stop("No test files were found.", call. = FALSE)
}

for (test_file in test_files) {
  message("Running ", test_file)
  sys.source(test_file, envir = new.env(parent = globalenv()))
}

message("All tests passed (", length(test_files), " test files).")
