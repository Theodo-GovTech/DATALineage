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
