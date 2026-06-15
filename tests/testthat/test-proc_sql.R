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
