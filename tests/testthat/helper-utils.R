# helper-utils.R — Test utilities for saslineager test suite

#' Create a temporary SAS file with given content, call `fun(filepath)`,
#' then clean up.
#' @param content Character string of SAS code.
#' @param fun Function taking a single filepath argument.
#' @return Result of `fun(filepath)`.
with_temp_sas <- function(content, fun) {
  f <- tempfile(fileext = ".sas")
  writeLines(content, f, useBytes = TRUE)
  on.exit(unlink(f), add = TRUE)
  fun(f)
}

#' Create a temporary directory containing one or more SAS files, call
#' `fun(dir_path)`, then clean up.
#' @param files Named list: name = filename, value = content string.
#' @param fun Function taking a single directory path argument.
#' @return Result of `fun(dir_path)`.
with_temp_sas_dir <- function(files, fun) {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (nm in names(files)) {
    writeLines(files[[nm]], file.path(dir, nm), useBytes = TRUE)
  }
  fun(dir)
}

#' Read lines from a SAS string (splits on newlines, keeps trailing newline)
sas_lines <- function(text) {
  strsplit(text, "\n", fixed = TRUE)[[1]]
}
