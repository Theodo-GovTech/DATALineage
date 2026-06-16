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

# --- find_macro_end: direct coverage of edge branches ---

test_that("find_macro_end returns -1 when no %mend closes the macro", {
  lines <- c(
    "%macro never_closed();",
    "    data out; set in; run;"
  )
  expect_equal(find_macro_end(lines, 1L), -1L)
})

test_that("find_macro_end skips a block comment with trailing code in body", {
  # The comment ends on line 3 with a trailing '%mend;' remainder, exercising
  # the replacement-rewrite branch inside the body scan.
  lines <- c(
    "%macro m();",
    "    data out; set in; run;",
    "/* multi",
    "   line comment */ %mend;"
  )
  expect_equal(find_macro_end(lines, 1L), 4L)
})

test_that("find_macro_end handles inline empty macro on first line", {
  lines <- c("%macro stub; %mend;")
  expect_equal(find_macro_end(lines, 1L), 1L)
})

test_that("find_macro_end tracks nesting depth across lines", {
  lines <- c(
    "%macro outer();",
    "    %macro inner();",
    "    %mend;",
    "%mend;"
  )
  expect_equal(find_macro_end(lines, 1L), 4L)
})

# --- parse_let_statement: direct, public-API coverage ---

test_that("parse_let_statement parses a plain assignment with no refs", {
  op <- parse_let_statement("%let year = 2024;", "f.sas", 7L)
  expect_false(is.null(op))
  expect_equal(op$dataset, "mv:year")
  expect_equal(op$operation_type, "MACRO LET")
  expect_equal(op$file, "f.sas")
  expect_equal(op$line_number, 7L)
  expect_equal(op$end_line, 7L)
  expect_equal(op$input_datasets, character(0))
})

test_that("parse_let_statement returns NULL on a non-%let line", {
  expect_null(parse_let_statement("data out; set in; run;", "f.sas", 1L))
})

test_that("parse_let_statement extracts single macro-var reference as input", {
  op <- parse_let_statement("%let target = &source.;", "f.sas", 3L)
  expect_equal(op$dataset, "mv:target")
  expect_equal(op$input_datasets, "mv:source")
})

test_that("parse_let_statement deduplicates repeated refs and lowercases", {
  op <- parse_let_statement("%let x = &A.&A.&B;", "f.sas", 2L)
  expect_equal(op$input_datasets, c("mv:a", "mv:b"))
})

test_that("parse_let_statement handles double-ampersand reference", {
  op <- parse_let_statement("%let v = &&inner;", "f.sas", 1L)
  expect_equal(op$input_datasets, "mv:inner")
})

test_that("parse_let_statement strips a trailing newline from the snippet", {
  op <- parse_let_statement("%let z = 1;\n", "f.sas", 4L)
  expect_equal(op$code_snippet, "%let z = 1;")
})

test_that("parse_let_statement is case-insensitive on the %LET keyword", {
  op <- parse_let_statement("%LET Region = NORTH;", "f.sas", 1L)
  expect_equal(op$dataset, "mv:region")
})

# --- parse_macro_definitions: block-comment-with-remainder at top level ---

test_that("parse_macro_definitions sees a macro after an inline-closed comment", {
  with_temp_sas(
    paste0(
      "/* leading comment */ %macro after_comment();\n",
      "    data out; set in; run;\n",
      "%mend;\n"
    ),
    function(f) {
      defs <- parse_macro_definitions(f)
      names_found <- vapply(defs, function(d) {
        d$name
      }, character(1))
      expect_true("after_comment" %in% names_found)
    }
  )
})

# --- .try_parse_macro_definition body scan: comment-with-remainder branch ---

test_that("parse_macro_definitions keeps body code trailing a block comment", {
  with_temp_sas(
    paste0(
      "%macro bodycmt();\n",
      "/* note */ data kept; set src; run;\n",
      "%mend;\n"
    ),
    function(f) {
      defs <- parse_macro_definitions(f)
      def <- Filter(function(d) {
        d$name == "bodycmt"
      }, defs)[[1]]
      body <- paste0(def$body, collapse = " ")
      expect_true(grepl("data kept", body))
    }
  )
})

test_that("parse_macro_definitions captures params and bare-macro defs", {
  with_temp_sas(
    paste0(
      "%macro withp(a, b);\n",
      "    data o; run;\n",
      "%mend;\n",
      "%macro bare;\n",
      "    data o2; run;\n",
      "%mend;\n"
    ),
    function(f) {
      defs <- parse_macro_definitions(f)
      by_name <- setNames(defs, vapply(defs, function(d) {
        d$name
      }, character(1)))
      expect_equal(by_name[["withp"]]$params, c("a", "b"))
      expect_equal(by_name[["bare"]]$params, character(0))
    }
  )
})

# --- expand_macro: direct public-API branches ---

test_that("expand_macro returns empty result on unknown macro name", {
  result <- expand_macro("missing", args = character(0), macro_definitions = list())
  expect_equal(result$lines, character(0))
  expect_equal(result$source_lines, integer(0))
})

test_that("expand_macro derives source lines from def$line when none given", {
  defs <- list(
    m = list(
      params = "ds",
      body = c("data out&ds; run;", "data two&ds; run;"),
      line = 10L
    )
  )
  result <- expand_macro("m", args = "X", macro_definitions = defs)
  expect_equal(result$source_lines, c(11L, 12L))
  expect_true(grepl("outX", result$lines[1]))
})

test_that("expand_macro positional binding skips a name-bound param", {
  # 'a' is bound by name; the positional value must skip 'a' and land on 'b'.
  defs <- list(
    m = list(
      params = c("a", "b"),
      body = "data &a._&b.; run;",
      body_source_lines = 5L,
      line = 1L
    )
  )
  result <- expand_macro("m", args = c("a=first", "second"), macro_definitions = defs)
  expect_true(grepl("data first_second", result$lines[1]))
})

test_that("expand_macro accepts a macro_def passed directly", {
  def <- list(
    params = "v",
    body = "data &v.; run;",
    body_source_lines = 9L,
    line = 1L
  )
  result <- expand_macro("ignored", args = "tab", macro_definitions = list(), macro_def = def)
  expect_true(grepl("data tab", result$lines[1]))
})

# --- unroll_do_loops: the loop machinery ---

test_that("unroll_do_loops passes through lines with no %do loop", {
  result <- unroll_do_loops(c("data a; run;", "data b; run;"), source_lines = c(1L, 2L))
  expect_equal(result$lines, c("data a; run;", "data b; run;"))
  expect_equal(result$source_lines, c(1L, 2L))
})

test_that("unroll_do_loops expands a simple integer loop", {
  lines <- c(
    "%do i = 1 %to 3;",
    "    data t&i.; run;",
    "%end;"
  )
  result <- unroll_do_loops(lines, source_lines = c(1L, 2L, 3L))
  body <- paste0(result$lines, collapse = "\n")
  expect_true(grepl("data t1;", body))
  expect_true(grepl("data t2;", body))
  expect_true(grepl("data t3;", body))
  expect_equal(length(result$lines), 3L)
  expect_equal(result$source_lines, c(2L, 2L, 2L))
})

test_that("unroll_do_loops unrolls a nested loop", {
  lines <- c(
    "%do i = 1 %to 2;",
    "  %do j = 1 %to 2;",
    "    data t&i._&j.; run;",
    "  %end;",
    "%end;"
  )
  result <- unroll_do_loops(lines, source_lines = c(1L, 2L, 3L, 4L, 5L))
  body <- paste0(result$lines, collapse = "\n")
  expect_true(grepl("data t1_1;", body))
  expect_true(grepl("data t1_2;", body))
  expect_true(grepl("data t2_1;", body))
  expect_true(grepl("data t2_2;", body))
  expect_equal(length(result$lines), 4L)
})

test_that("unroll_do_loops leaves an unterminated loop untouched", {
  lines <- c(
    "%do i = 1 %to 3;",
    "    data t&i.; run;"
  )
  result <- unroll_do_loops(lines, source_lines = c(1L, 2L))
  expect_equal(result$lines, lines)
  expect_equal(result$source_lines, c(1L, 2L))
})

test_that("unroll_do_loops leaves a loop exceeding max_iterations untouched", {
  lines <- c(
    "%do i = 1 %to 10;",
    "    data t&i.; run;",
    "%end;"
  )
  result <- unroll_do_loops(lines, source_lines = c(1L, 2L, 3L), max_iterations = 3L)
  expect_equal(result$lines, lines)
  expect_equal(result$source_lines, c(1L, 2L, 3L))
})

test_that("expand_macro unrolls a %do loop in the macro body end to end", {
  defs <- list(
    mk = list(
      params = character(0),
      body = c(
        "%do n = 1 %to 2;",
        "    data out&n.; set src&n.; run;",
        "%end;"
      ),
      body_source_lines = c(2L, 3L, 4L),
      line = 1L
    )
  )
  result <- expand_macro("mk", args = character(0), macro_definitions = defs)
  body <- paste0(result$lines, collapse = "\n")
  expect_true(grepl("data out1;", body))
  expect_true(grepl("data out2;", body))
})
