# test-ods.R — Tests for ods.R (ODS tagsets.csv parsing)

.parse_ods <- function(source, filename_refs = NULL) {
  f <- tempfile(fileext = ".sas")
  on.exit(unlink(f), add = TRUE)
  writeLines(source, f, useBytes = TRUE)
  lines <- readLines(f, warn = FALSE)
  parse_ods_tagsets_csv(lines, 1L, f, NULL, filename_refs)
}

test_that("fileref resolves to csv basename", {
  refs <- list(
    tab1_011 = list("/path/to/alloc.sas", 710L,
      "&resultsn/&finess..&an_cours..encmco.tab01_01_1.csv")
  )
  result <- .parse_ods(
    paste0(
      'ods tagsets.csv file=tab1_011 options(delimiter=";");\n',
      "proc print data=src; run;\n",
      "ods tagsets.csv close;\n"
    ),
    filename_refs = refs
  )
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "tab01_01_1")
  expect_equal(result$operation$input_datasets, "src")
})

test_that("fileref resolves when normalized match", {
  refs <- list(
    grpv = list("/path/file.sas", 10L, "&rep./tab06_03_1.csv")
  )
  result <- .parse_ods(
    paste0(
      "ods tagsets.csv file=grpv&group options();\n",
      "proc print data=ds; run;\n",
      "ods tagsets.csv close;\n"
    ),
    filename_refs = refs
  )
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "tab06_03_1")
})

test_that("unknown fileref falls back to token", {
  result <- .parse_ods(
    paste0(
      'ods tagsets.csv file=myref options(delimiter=";");\n',
      "proc print data=src; run;\n",
      "ods tagsets.csv close;\n"
    ),
    filename_refs = list()
  )
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "myref")
})

test_that("no filename_refs argument keeps token", {
  result <- .parse_ods(
    paste0(
      "ods tagsets.csv file=tab00_01 options();\n",
      "proc print data=src; run;\n",
      "ods tagsets.csv close;\n"
    )
  )
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "tab00_01")
})

test_that("ods xml recognised same as tagsets.csv", {
  result <- .parse_ods(
    paste0(
      "ods xml file=tab01_00 type=csv;\n",
      "proc print data=version; run;\n",
      "ods xml close;\n"
    )
  )
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "tab01_00")
  expect_equal(result$operation$input_datasets, "version")
  expect_equal(result$operation$operation_type, "ODS CSV")
})

test_that("bare ods csv destination recognised same as tagsets.csv", {
  result <- .parse_ods(
    paste0(
      'ods csv file=sej10_01 options(delimiter=";");\n',
      "proc print data=tab101_liste_sej noobs label; run;\n",
      "ods csv close;\n"
    )
  )
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "sej10_01")
  expect_equal(result$operation$input_datasets, "tab101_liste_sej")
  expect_equal(result$operation$operation_type, "ODS CSV")
})

# ---------------------------------------------------------------------------
# Added coverage tests
# ---------------------------------------------------------------------------

test_that("ods block with open and close on a single line is parsed", {
  result <- .parse_ods(
    "ods csv file=t1 options(); proc print data=src; run; ods csv close;\n"
  )
  expect_false(is.null(result))
  expect_equal(result$operation$dataset, "t1")
  expect_equal(result$operation$input_datasets, "src")
  expect_equal(result$end_idx, 1L)
})

test_that("ods block with no matching close returns NULL", {
  result <- .parse_ods(
    paste0(
      "ods csv file=t2 options();\n",
      "proc print data=src; run;\n"
    )
  )
  expect_null(result)
})

test_that("parse_ods_tagsets_csv returns NULL on a non-ODS line", {
  expect_null(parse_ods_tagsets_csv(c("data x; run;"), 1L, "x.sas"))
})

test_that("ods block scans data refs inside a called macro body", {
  lines <- c(
    "ods csv file=t1 options();",
    "%mymacro();",
    "%if cond %then;",
    "ods csv close;"
  )
  macro_defs <- list(
    mymacro = list(body = c("proc print data=hidden_input; run;"))
  )
  result <- parse_ods_tagsets_csv(lines, 1L, "x.sas", macro_defs)
  expect_false(is.null(result))
  expect_true("hidden_input" %in% result$operation$input_datasets)
})

test_that("ods block ignores macro calls with no matching definition", {
  lines <- c(
    "ods csv file=t1 options();",
    "%unknownmacro();",
    "ods csv close;"
  )
  macro_defs <- list(other = list(body = "proc print data=x; run;"))
  result <- parse_ods_tagsets_csv(lines, 1L, "x.sas", macro_defs)
  expect_false(is.null(result))
  expect_equal(result$operation$input_datasets, character(0))
})

test_that("ods block skips macro control-flow keyword calls", {
  # %do; matches the macro-call regex but is in the skip list, so it must not
  # be looked up among the macro definitions.
  lines <- c(
    "ods csv file=t1 options();",
    "%do;",
    "ods csv close;"
  )
  macro_defs <- list(other = list(body = ""))
  result <- parse_ods_tagsets_csv(lines, 1L, "x.sas", macro_defs)
  expect_false(is.null(result))
  expect_equal(result$operation$input_datasets, character(0))
})
