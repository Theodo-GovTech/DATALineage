# test-proc_generic.R — Tests for proc_generic.R (FREQ, MEANS, SUMMARY, UNIVARIATE)

.parse_generic <- function(source, proc_type) {
  f <- tempfile(fileext = ".sas")
  on.exit(unlink(f), add = TRUE)
  writeLines(source, f, useBytes = TRUE)
  lines <- readLines(f, warn = FALSE)
  parse_proc_generic(lines, 1L, f, proc_type)
}

# --- PROC FREQ ---

test_that("table /out= picks up freq output", {
  result <- .parse_generic(
    paste0(
      "proc freq data=tmp noprint;\n",
      "\ttable ghmerr /out=freq_ghm_err ;\n",
      "run;\n"
    ),
    "freq"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "freq_ghm_err")
  expect_equal(result$operation_type, "PROC FREQ")
  expect_equal(result$input_datasets, "tmp")
  expect_equal(result$end_line, 2L)
})

test_that("tables plural keyword", {
  result <- .parse_generic(
    paste0(
      "proc freq data=src;\n",
      "  tables a*b / out=cross_tab;\n",
      "run;\n"
    ),
    "freq"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "cross_tab")
  expect_equal(result$input_datasets, "src")
})

test_that("data option with where clause", {
  result <- .parse_generic(
    paste0(
      "proc freq data=sejpat(where=(ghmv&group in &l_ghm_erreur)) noprint;\n",
      "\ttable cret /out=freq_code_err ;\n",
      "run;\n"
    ),
    "freq"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "freq_code_err")
  expect_equal(result$input_datasets, "sejpat")
})

test_that("no out= returns NULL", {
  result <- .parse_generic(
    paste0(
      "proc freq data=src;\n",
      "  table x;\n",
      "run;\n"
    ),
    "freq"
  )
  expect_null(result)
})

test_that("does not confuse data= with table out=", {
  result <- .parse_generic(
    paste0(
      "proc freq data=src noprint;\n",
      "  tables foo / out=out_ds;\n",
      "run;\n"
    ),
    "freq"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "out_ds")
  expect_equal(result$input_datasets, "src")
})

# --- PROC MEANS ---

test_that("output out= with where and drop options (regression)", {
  result <- .parse_generic(
    paste0(
      "proc means data=tabdial5_tmp n mean min q1 median q3 max;\n",
      "\tvar mnt_nbs;\n",
      "\tclass finess type sa libelle_sa;\n",
      '\toutput out=tabdial5(where=(finess ne "" and type ne ""',
      ' and sa ne "" and libelle_sa ne "") drop=_type_ _freq_)\n',
      "\t\tn=n mean=mean min=min q1=q1 median=median q3=q3 max=max;\n",
      "run;\n"
    ),
    "means"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "tabdial5")
  expect_equal(result$operation_type, "PROC MEANS")
  expect_equal(result$input_datasets, "tabdial5_tmp")
})

test_that("output out= plain dataset", {
  result <- .parse_generic(
    paste0(
      "proc means data=src;\n",
      "  var x;\n",
      "  output out=stats n=n mean=mean;\n",
      "run;\n"
    ),
    "means"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "stats")
  expect_equal(result$input_datasets, "src")
})

test_that("means stops at next data/proc block before any output", {
  result <- .parse_generic(
    paste0(
      "proc means data=src;\n",
      "  var x;\n",
      "data next; set y; run;\n"
    ),
    "means"
  )
  expect_null(result)
})

test_that("means returns NULL when no output and no run; before EOF", {
  result <- .parse_generic(
    paste0(
      "proc means data=src;\n",
      "  var x;\n",
      "  more statements;"
    ),
    "means"
  )
  expect_null(result)
})

# --- PROC FREQ edge branches ---

test_that("freq stops at next data/proc block before any table out=", {
  result <- .parse_generic(
    paste0(
      "proc freq data=src;\n",
      "  tables x;\n",
      "data next; set y; run;\n"
    ),
    "freq"
  )
  expect_null(result)
})

test_that("freq returns NULL when no slash-out and no run; before EOF", {
  result <- .parse_generic(
    paste0(
      "proc freq data=src;\n",
      "  tables x;\n",
      "  trailing;"
    ),
    "freq"
  )
  expect_null(result)
})

# --- PROC TRANSPOSE / inline out= ---

test_that("transpose reads inline out= dataset", {
  result <- .parse_generic(
    "proc transpose data=src out=trans_out;\n run;\n",
    "transpose"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "trans_out")
  expect_equal(result$operation_type, "PROC TRANSPOSE")
  expect_equal(result$input_datasets, "src")
})

test_that("transpose inline out= strips dataset options in parentheses", {
  result <- .parse_generic(
    "proc transpose data=src out=outds(drop=x);\n run;\n",
    "transpose"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "outds")
})

test_that("unknown proc type falls back to inline out=", {
  result <- .parse_generic(
    "proc tabulate data=src out=tab_out;\n run;\n",
    "tabulate"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "tab_out")
  expect_equal(result$operation_type, "PROC TABULATE")
})

test_that("returns NULL when output token cleans to an empty name", {
  result <- .parse_generic(
    "proc transpose data=src out=.;\n run;\n",
    "transpose"
  )
  expect_null(result)
})

# --- token extraction edge cases (driven through the public parser) ---

test_that("transpose with no out= keyword yields no output dataset", {
  result <- .parse_generic(
    "proc transpose data=src;\n run;\n",
    "transpose"
  )
  expect_null(result)
})

test_that("transpose with out= at end of line yields no token", {
  result <- .parse_generic(
    "proc transpose data=src out=",
    "transpose"
  )
  expect_null(result)
})

test_that("transpose with unbalanced close paren after out= yields no token", {
  result <- .parse_generic(
    "proc transpose data=src out=)abc;\n run;\n",
    "transpose"
  )
  expect_null(result)
})

test_that("transpose with empty out= token yields no output dataset", {
  result <- .parse_generic(
    "proc transpose data=src out= ;\n run;\n",
    "transpose"
  )
  expect_null(result)
})

# --- PROC UNIVARIATE ---

test_that("proc univariate output out=", {
  result <- .parse_generic(
    paste0(
      "proc univariate data=sejmat2_calcul(where=(type_date='Presente')) noprint;\n",
      "  by typecd;\n",
      "  var mnt_jour;\n",
      "  output out=tab62b_sejmat2 n=nblignes min=minimum",
      " q1=q1 median=mediane q3=q3 max=maximum mean=mean;\n",
      "run;\n"
    ),
    "univariate"
  )
  expect_false(is.null(result))
  expect_equal(result$dataset, "tab62b_sejmat2")
  expect_equal(result$operation_type, "PROC UNIVARIATE")
  expect_equal(result$input_datasets, "sejmat2_calcul")
})
