# test-proc_export.R — Tests for proc_export.R

.parse_export <- function(source) {
  f <- tempfile(fileext = ".sas")
  on.exit(unlink(f), add = TRUE)
  writeLines(source, f, useBytes = TRUE)
  lines <- readLines(f, warn = FALSE)
  parse_proc_export(lines, 1L, f)
}

test_that("outfile with macro param chain (syrius pattern)", {
  result <- .parse_export(paste0(
    "proc export data=s4\n",
    'outfile= "&rep./&param_ipe..&param_finessS..&param_ordre..&annee..&param_periode..tab15_1.csv"\n',
    "dbms=dlm replace;\n",
    'delimiter=";";\n',
    "run;\n"
  ))
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "tab15_1")
  expect_equal(result$operation$operation_type, "PROC EXPORT")
  expect_equal(result$operation$input_datasets, "s4")
  expect_equal(result$operation$line_number, 1L)
  expect_equal(result$end_idx, 5L)
})

test_that("outfile simple literal path", {
  result <- .parse_export(paste0(
    "proc export data=mydata\n",
    'outfile="/tmp/output.csv" dbms=csv replace;\n',
    "run;\n"
  ))
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "output")
  expect_equal(result$operation$input_datasets, "mydata")
})

test_that("outfile single line, single quotes", {
  result <- .parse_export(
    "proc export data=src outfile='/tmp/foo.csv' dbms=csv; run;\n"
  )
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "foo")
  expect_equal(result$operation$input_datasets, "src")
})

test_that("outfile library-qualified input", {
  result <- .parse_export(paste0(
    "proc export data=work.moy_rpu\n",
    'outfile="&rep./tab2_1.csv" dbms=dlm replace;\n',
    "run;\n"
  ))
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "tab2_1")
  expect_equal(result$operation$input_datasets, "moy_rpu")
})

test_that("outfile fileref returns NULL", {
  result <- .parse_export(paste0(
    "proc export data=src outfile=myref dbms=csv replace;\n",
    "run;\n"
  ))
  expect_null(result)
})

test_that("no proc export returns NULL", {
  result <- .parse_export("data x; run;\n")
  expect_null(result)
})
