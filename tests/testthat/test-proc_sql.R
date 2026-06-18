# test-proc_sql.R — Tests for proc_sql.R (PROC SQL parsing)

test_that("simple create table", {
  with_temp_sas(
    "\ncreate table output as\nselect * from input;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql(lines, 2L, f)
      expect_equal(result$dataset, "output")
      expect_equal(result$operation_type, "PROC SQL")
      expect_true("input" %in% result$input_datasets)
    }
  )
})

test_that("create table with macro var ref", {
  with_temp_sas(
    "create table s1 as select '1' as nolig, count(*) as ind, &eff as eff from temp;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql(lines, 1L, f)
      expect_equal(result$dataset, "s1")
      expect_true("temp" %in% result$input_datasets)
      expect_true("mv:eff" %in% result$input_datasets)
    }
  )
})

test_that("create table with join", {
  with_temp_sas(
    "\ncreate table output as\nselect a.*, b.col\nfrom table1 a\nleft join table2 b on a.id = b.id;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql(lines, 2L, f)
      expect_true("table1" %in% result$input_datasets)
      expect_true("table2" %in% result$input_datasets)
    }
  )
})

test_that("create table with subquery", {
  with_temp_sas(
    "\ncreate table output as\nselect * from table1 a\nleft join (select * from table2) b on a.id = b.id;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql(lines, 2L, f)
      expect_true("table1" %in% result$input_datasets)
      expect_true("table2" %in% result$input_datasets)
    }
  )
})

test_that("comma cross-join with subquery picks up subquery table (regression)", {
  with_temp_sas(
    paste0(
      "\ncreate table output as\n",
      "select a.*, b.mnt_transmis\n",
      "from outer_tab a,\n",
      "(select typecd, type_date, mnt_transmis\n",
      "from inner_tab\n",
      "where type_date=\"T\") b\n",
      "where a.typecd=b.typecd;\n"
    ),
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql(lines, 2L, f)
      expect_true("outer_tab" %in% result$input_datasets)
      expect_true("inner_tab" %in% result$input_datasets)
      expect_false("type_date" %in% result$input_datasets)
      expect_false("mnt_transmis" %in% result$input_datasets)
      expect_false("typecd" %in% result$input_datasets)
    }
  )
})

test_that("no duplicate tables", {
  with_temp_sas(
    "\ncreate table output as\nselect a.*, b.*\nfrom table1 a, table1 b;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql(lines, 2L, f)
      expect_equal(sum(result$input_datasets == "table1"), 1L)
    }
  )
})

test_that("multiple create tables in block", {
  with_temp_sas(
    "proc sql;\ncreate table output1 as\nselect * from input1;\ncreate table output2 as\nselect * from input2;\nquit;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql_block(lines, 1L, f)
      ops <- result$operations
      expect_length(ops, 2L)
      expect_equal(ops[[1]]$line_number, 2L)
      expect_equal(ops[[1]]$end_line, 3L)
      expect_equal(ops[[2]]$line_number, 4L)
      expect_equal(ops[[2]]$end_line, 5L)
      expect_equal(result$end_line, 6L)
      expect_equal(ops[[1]]$dataset, "output1")
      expect_true("input1" %in% ops[[1]]$input_datasets)
      expect_equal(ops[[2]]$dataset, "output2")
      expect_true("input2" %in% ops[[2]]$input_datasets)
    }
  )
})

test_that("proc sql block SELECT INTO emits macro-var op", {
  with_temp_sas(
    "proc sql noprint;\nselect count(*) into :cnt from mytable;\nquit;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql_block(lines, 1L, f)
      ops <- result$operations
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "mv:cnt")
      expect_equal(ops[[1]]$operation_type, "PROC SQL INTO")
      expect_equal(ops[[1]]$input_datasets, "mytable")
      expect_equal(result$end_line, 3L)
    }
  )
})

test_that("inline proc sql create table on opener line (regression)", {
  with_temp_sas(
    paste0(
      "proc sql; create table date_rpu as select a.date\n",
      "from lst_jour as a left join rpu_entree as b on a.date=b.date_entree;\n",
      "quit;\n"
    ),
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql_block(lines, 1L, f)
      ops <- result$operations
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "date_rpu")
      expect_equal(ops[[1]]$operation_type, "PROC SQL")
      expect_equal(ops[[1]]$line_number, 1L)
      expect_true("lst_jour" %in% ops[[1]]$input_datasets)
      expect_true("rpu_entree" %in% ops[[1]]$input_datasets)
      expect_equal(result$end_line, 3L)
    }
  )
})

test_that("inline proc sql select into on opener line (regression)", {
  with_temp_sas(
    paste0(
      "proc sql noprint; select count(*) into :nbfin from sejpat;\n",
      "quit;\n"
    ),
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql_block(lines, 1L, f)
      ops <- result$operations
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "mv:nbfin")
      expect_equal(ops[[1]]$operation_type, "PROC SQL INTO")
      expect_equal(ops[[1]]$input_datasets, "sejpat")
      expect_equal(result$end_line, 2L)
    }
  )
})

# ---------------------------------------------------------------------------
# Added coverage tests
# ---------------------------------------------------------------------------

test_that("extract_sql_tables closes open FROM contexts at end of buffer (no semicolon)", {
  expect_equal(extract_sql_tables("select * from mytab"), "mytab")
})

test_that("extract_sql_tables skips empty entries in a comma table list", {
  # The empty slot between the two commas yields a NULL extracted token.
  expect_equal(extract_sql_tables("select * from a, , b;"), c("a", "b"))
})

test_that("extract_sql_tables closes a FROM context with no table before a terminator", {
  # FROM is immediately followed by WHERE at the same depth: no table token.
  expect_equal(extract_sql_tables("select * from where x=1;"), character(0))
})

test_that("parse_proc_sql returns NULL when the line is not a CREATE TABLE", {
  expect_null(parse_proc_sql(c("select * from x;"), 1L, "x.sas"))
})

test_that("collect_sql_statement_inputs scans PUT format references", {
  result <- parse_proc_sql(c("create table t as select put(x, myfmt.) from src;"),
    1L, "x.sas")
  expect_true("fmt:myfmt" %in% result$input_datasets)
  expect_true("src" %in% result$input_datasets)
})

test_that("collect_sql_statement_inputs stops after 50 unterminated lines", {
  lines <- c("create table t as select * from src", rep("col,", 60L))
  result <- parse_proc_sql(lines, 1L, "x.sas")
  expect_equal(result$end_line, 52L)
})

test_that(".collect_select_into_targets stops after 50 unterminated lines", {
  lines <- c(
    "proc sql noprint;",
    "select count(*) into :cnt from src",
    rep("x", 60L)
  )
  result <- parse_proc_sql_block(lines, 1L, "x.sas")
  expect_length(result$operations, 1L)
  expect_equal(result$operations[[1]]$dataset, "mv:cnt")
})

test_that("parse_proc_sql_block emits a MACRO LET op for %let var = &sqlobs", {
  lines <- c(
    "proc sql;",
    "create table t as select * from src;",
    "%let n = &sqlobs;",
    "quit;"
  )
  result <- parse_proc_sql_block(lines, 1L, "x.sas")
  ops <- result$operations
  expect_length(ops, 2L)
  expect_equal(ops[[2]]$dataset, "mv:n")
  expect_equal(ops[[2]]$operation_type, "MACRO LET")
  expect_equal(ops[[2]]$input_datasets, "t")
})

test_that("parse_proc_sql_block ends at the next DATA/PROC block before quit", {
  lines <- c(
    "proc sql;",
    "create table t as select * from src;",
    "data later;",
    "set t;",
    "run;"
  )
  result <- parse_proc_sql_block(lines, 1L, "x.sas")
  expect_length(result$operations, 1L)
  expect_equal(result$end_line, 2L)
})

test_that("parse_proc_sql_block clamps end_line to file length when there is no quit", {
  lines <- c(
    "proc sql;",
    "create table t as select * from src;"
  )
  result <- parse_proc_sql_block(lines, 1L, "x.sas")
  expect_equal(result$end_line, 2L)
})
