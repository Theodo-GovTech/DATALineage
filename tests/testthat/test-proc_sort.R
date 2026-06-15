# test-proc_sort.R — Tests for proc_sort.R

test_that("simple proc sort", {
  with_temp_sas(
    "proc sort data=mydata; by id; run;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sort(lines, 1L, f)
      expect_false(is.null(result))
      expect_equal(result$dataset, "mydata")
      expect_equal(result$operation_type, "PROC SORT")
      expect_equal(result$input_datasets, "mydata")
    }
  )
})

test_that("proc sort with out= option", {
  with_temp_sas(
    "proc sort data=raw out=sorted; by id; run;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sort(lines, 1L, f)
      expect_false(is.null(result))
      expect_equal(result$dataset, "sorted")
      expect_equal(result$input_datasets, "raw")
    }
  )
})

test_that("non-proc-sort returns NULL", {
  with_temp_sas(
    "data x; set y; run;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sort(lines, 1L, f)
      expect_null(result)
    }
  )
})
