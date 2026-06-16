# test-proc_format.R — Tests for proc_format.R

test_that("parse_proc_format detects value statement", {
  with_temp_sas(
    paste0(
      "proc format;\n",
      "  value agegrp 0-14='0-14' 15-24='15-24';\n",
      "run;\n"
    ),
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_format(lines, 1L, f)
      expect_false(is.null(result))
      ops <- result$operations
      expect_true(length(ops) >= 1L)
      expect_equal(ops[[1]]$operation_type, "PROC FORMAT")
    }
  )
})

test_that("scan_format_refs finds put-function format refs", {
  code <- c(
    "x = put(val, agegrp.);",
    "y = put(val, $sex.);",
    "z = cats(a, b);"
  )
  refs <- scan_format_refs(code)
  expect_true("agegrp" %in% refs)
  expect_true("sex" %in% refs)
})

test_that("parse_proc_format picks up cntlin= on the proc line", {
  lines <- c("proc format cntlin=mycntl;", "run;")
  result <- parse_proc_format(lines, 1L, "f.sas")
  datasets <- vapply(result$operations, `[[`, character(1), "dataset")
  expect_true("fmt:cntlin:mycntl" %in% datasets)
  cntlin_op <- result$operations[[which(datasets == "fmt:cntlin:mycntl")]]
  expect_equal(cntlin_op$input_datasets, "mycntl")
})

test_that("parse_proc_format picks up inline cntlin= and a value statement", {
  lines <- c(
    "proc format;",
    "value foo 1='a';",
    "set cntlin=work.fmtdata;",
    "run;"
  )
  result <- parse_proc_format(lines, 1L, "f.sas")
  datasets <- vapply(result$operations, `[[`, character(1), "dataset")
  expect_true("fmt:foo" %in% datasets)
  expect_true("fmt:cntlin:fmtdata" %in% datasets)
})

test_that("parse_proc_format de-duplicates repeated value names and strips $", {
  lines <- c(
    "proc format;",
    "value foo 1='a';",
    "value foo 2='b';",
    "value $bar 1='x';",
    "run;"
  )
  result <- parse_proc_format(lines, 1L, "f.sas")
  datasets <- vapply(result$operations, `[[`, character(1), "dataset")
  expect_equal(sum(datasets == "fmt:foo"), 1L)
  expect_true("fmt:bar" %in% datasets)
})

test_that("parse_proc_format bails on a following data/proc step before run;", {
  lines <- c(
    "proc format;",
    "value foo 1='a';",
    "data next; set x; run;"
  )
  result <- parse_proc_format(lines, 1L, "f.sas")
  datasets <- vapply(result$operations, `[[`, character(1), "dataset")
  expect_true("fmt:foo" %in% datasets)
  expect_false(any(grepl("next", datasets)))
})

test_that("scan_format_refs de-duplicates repeated format references", {
  refs <- scan_format_refs(c(
    "x = put(a, agegrp.);",
    "y = put(b, agegrp.);"
  ))
  expect_equal(sum(refs == "agegrp"), 1L)
})

test_that("non-proc-format returns empty list", {
  with_temp_sas(
    "data x; set y; run;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_format(lines, 1L, f)
      expect_true(is.null(result) || length(result$operations) == 0L)
    }
  )
})
