# test-macro.R — Tests for macro.R (definition, expansion, resolution)

test_that("macro definition parsing", {
  with_temp_sas(
    paste0(
      "\n",
      "%macro test_macro(param1, param2);\n",
      "    data output;\n",
      "        set input;\n",
      "    run;\n",
      "%mend;\n"
    ),
    function(f) {
      analyzer <- SASLineageAnalyzer$new(".")
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_macro_defs(f)
      expect_true("test_macro" %in% names(analyzer$macro_definitions))
      md <- analyzer$macro_definitions[["test_macro"]]
      expect_equal(md$params, c("param1", "param2"))
      expect_true(length(md$body) > 0L)
    }
  )
})

test_that("macro expansion with params", {
  analyzer <- SASLineageAnalyzer$new(".")
  analyzer$macro_definitions[["test"]] <- list(
    params = "ds",
    body = "data output&ds; set input&ds; run;",
    body_source_lines = 2L,
    file = "test.sas",
    line = 1L
  )
  private_env <- analyzer$.__enclos_env__$private
  result <- private_env$expand_macro_call("test", c("123"))
  expanded <- result$lines
  source_lines <- result$source_lines
  expect_length(expanded, 1L)
  expect_true(grepl("output123", expanded[1]))
  expect_true(grepl("input123", expanded[1]))
  expect_equal(source_lines, 2L)
})

test_that("macro expansion empty param", {
  analyzer <- SASLineageAnalyzer$new(".")
  analyzer$macro_definitions[["test"]] <- list(
    params = "year",
    body = "data table&year._final; run;",
    body_source_lines = 2L,
    file = "test.sas",
    line = 1L
  )
  private_env <- analyzer$.__enclos_env__$private
  result <- private_env$expand_macro_call("test", c(""))
  expect_true(grepl("table_final", result$lines[1]))
})

test_that("macro expansion named args bind by name (regression)", {
  analyzer <- SASLineageAnalyzer$new(".")
  analyzer$macro_definitions[["icr_dial"]] <- list(
    params = c("var", "var2", "ghm", "tab"),
    body = c(
      "create table uo_&var.3 as select * from foo;",
      "data atyp4_&tab; set bar; run;"
    ),
    body_source_lines = c(2L, 3L),
    file = "test.sas",
    line = 1L
  )
  private_env <- analyzer$.__enclos_env__$private
  result <- private_env$expand_macro_call(
    "icr_dial",
    c("var=chimio", "var2=chimio", "ghm=28Z17Z", "tab=11_2")
  )
  expect_true(grepl("uo_chimio3", result$lines[1]))
  expect_false(grepl("var=chimio", result$lines[1]))
  expect_true(grepl("atyp4_11_2", result$lines[2]))
})

test_that("macro expansion named args out of order", {
  analyzer <- SASLineageAnalyzer$new(".")
  analyzer$macro_definitions[["m"]] <- list(
    params = c("a", "b", "c"),
    body = "data &a._&b._&c.; run;",
    body_source_lines = 2L,
    file = "test.sas",
    line = 1L
  )
  private_env <- analyzer$.__enclos_env__$private

  result <- private_env$expand_macro_call("m", c("c=zee", "a=eh", "b=bee"))
  expect_true(grepl("data eh_bee_zee", result$lines[1]))

  result2 <- private_env$expand_macro_call("m", c("eh", "c=zee", "bee"))
  expect_true(grepl("data eh_bee_zee", result2$lines[1]))
})

test_that("macro definition ignores commented macro", {
  with_temp_sas(
    paste0(
      "\n",
      "/* %macro fake_macro();\n",
      "    data fake_out;\n",
      "        set fake_in;\n",
      "    run;\n",
      "%mend; */\n",
      "%macro real_macro();\n",
      "    data real_out;\n",
      "        set real_in;\n",
      "    run;\n",
      "%mend;\n"
    ),
    function(f) {
      analyzer <- SASLineageAnalyzer$new(".")
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_macro_defs(f)
      expect_false("fake_macro" %in% names(analyzer$macro_definitions))
      expect_true("real_macro" %in% names(analyzer$macro_definitions))
    }
  )
})

test_that("macro definition ignores commented mend in body scan", {
  with_temp_sas(
    paste0(
      "\n",
      "%macro test_macro();\n",
      "/* %mend; */\n",
      "data output;\n",
      "    set input;\n",
      "run;\n",
      "%mend;\n"
    ),
    function(f) {
      analyzer <- SASLineageAnalyzer$new(".")
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_macro_defs(f)
      expect_true("test_macro" %in% names(analyzer$macro_definitions))
      body <- paste0(analyzer$macro_definitions[["test_macro"]]$body, collapse = "")
      body <- tolower(body)
      expect_true(grepl("data output", body))
      expect_true(grepl("set input", body))
    }
  )
})

test_that("macro resolution uses latest visible same-file definition", {
  with_temp_sas(
    paste0(
      "%macro mk();\n",
      "%mend;\n",
      "%mk();\n",
      "%macro mk();\n",
      "%mend;\n",
      "%mk();\n"
    ),
    function(f) {
      analyzer <- SASLineageAnalyzer$new(".")
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_macro_defs(f)
      first_def <- private_env$resolve_macro_def("mk", f, 3L)
      second_def <- private_env$resolve_macro_def("mk", f, 6L)
      expect_false(is.null(first_def))
      expect_false(is.null(second_def))
      expect_equal(first_def$line, 1L)
      expect_equal(second_def$line, 4L)
    }
  )
})

test_that("macro resolution: include after local overrides local", {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  inc_file <- file.path(dir, "inc.sas")
  main_file <- file.path(dir, "main.sas")
  writeLines(c("%macro mk();", "%mend;"), inc_file)
  writeLines(c(
    "%macro mk();",
    "%mend;",
    paste0('%include "', inc_file, '";'),
    "%mk();"
  ), main_file)

  analyzer <- SASLineageAnalyzer$new(dir)
  private_env <- analyzer$.__enclos_env__$private
  private_env$parse_macro_defs(inc_file)
  private_env$parse_include_stmts(main_file)
  private_env$parse_macro_defs(main_file)

  resolved <- private_env$resolve_macro_def("mk", main_file, 4L)
  expect_false(is.null(resolved))
  expect_equal(
    normalizePath(resolved$file, mustWork = FALSE),
    normalizePath(inc_file, mustWork = FALSE)
  )
})

test_that("macro resolution: local after include overrides include", {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  inc_file <- file.path(dir, "inc.sas")
  main_file <- file.path(dir, "main.sas")
  writeLines(c("%macro mk();", "%mend;"), inc_file)
  writeLines(c(
    paste0('%include "', inc_file, '";'),
    "%macro mk();",
    "%mend;",
    "%mk();"
  ), main_file)

  analyzer <- SASLineageAnalyzer$new(dir)
  private_env <- analyzer$.__enclos_env__$private
  private_env$parse_macro_defs(inc_file)
  private_env$parse_include_stmts(main_file)
  private_env$parse_macro_defs(main_file)

  resolved <- private_env$resolve_macro_def("mk", main_file, 4L)
  expect_false(is.null(resolved))
  expect_equal(
    normalizePath(resolved$file, mustWork = FALSE),
    normalizePath(main_file, mustWork = FALSE)
  )
  expect_equal(resolved$line, 2L)
})

test_that("macro resolution: safe unresolved without same-file or include", {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  other_file <- file.path(dir, "other.sas")
  call_file <- file.path(dir, "caller.sas")
  writeLines(c("%macro mk();", "%mend;"), other_file)
  writeLines("%mk();", call_file)

  analyzer <- SASLineageAnalyzer$new(dir)
  private_env <- analyzer$.__enclos_env__$private
  private_env$parse_macro_defs(other_file)
  private_env$parse_include_stmts(call_file)
  private_env$parse_macro_defs(call_file)

  resolved <- private_env$resolve_macro_def("mk", call_file, 1L)
  expect_null(resolved)
})

test_that("empty stub does not shadow real macro in global lookup", {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  real_file <- file.path(dir, "real.sas")
  stub_file <- file.path(dir, "stub.sas")
  writeLines(c(
    "%macro mk;",
    "    data target; set source; run;",
    "%mend;"
  ), real_file)
  writeLines("%macro mk; %mend;", stub_file)

  # Real first, then stub
  analyzer <- SASLineageAnalyzer$new(".")
  private_env <- analyzer$.__enclos_env__$private
  private_env$parse_macro_defs(real_file)
  private_env$parse_macro_defs(stub_file)
  kept <- analyzer$macro_definitions[["mk"]]
  body <- paste0(kept$body, collapse = "")
  expect_true(grepl("data target", body))

  # Stub first, then real
  analyzer2 <- SASLineageAnalyzer$new(".")
  private_env2 <- analyzer2$.__enclos_env__$private
  private_env2$parse_macro_defs(stub_file)
  private_env2$parse_macro_defs(real_file)
  kept2 <- analyzer2$macro_definitions[["mk"]]
  body2 <- paste0(kept2$body, collapse = "")
  expect_true(grepl("data target", body2))
})
