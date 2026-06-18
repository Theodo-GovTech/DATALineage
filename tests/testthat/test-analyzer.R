# test-analyzer.R — Integration tests for the SASLineageAnalyzer R6 class

# --- End-to-end lineage ---

test_that("simple lineage chain", {
  with_temp_sas_dir(
    list(
      "step1.sas" = paste0(
        "\n",
        'filename input "/data/input.csv";\n',
        "\n",
        "data intermediate;\n",
        "    infile input;\n",
        "    input var1 var2;\n",
        "run;\n"
      ),
      "step2.sas" = paste0(
        "\n",
        "data final_output;\n",
        "    set intermediate;\n",
        "    var3 = var1 + var2;\n",
        "run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()

      manifest_file <- tempfile(fileext = ".json")
      on.exit(unlink(manifest_file), add = TRUE)
      analyzer$generate_json_manifest("final_output", manifest_file)
      manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)

      expect_equal(manifest$target_dataset, "final_output")
      datasets <- vapply(manifest$operations, `[[`, character(1), "dataset")
      expect_true("final_output" %in% datasets)
      expect_true("intermediate" %in% datasets)
      expect_true("infile:input" %in% datasets)

      final_op <- Filter(function(op) op$dataset == "final_output",
        manifest$operations)[[1]]
      expect_true("intermediate" %in% final_op$input_datasets)

      inter_op <- Filter(function(op) op$dataset == "intermediate",
        manifest$operations)[[1]]
      expect_true("INFILE:input" %in% inter_op$input_datasets)
    }
  )
})

test_that("macro expansion integration", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "\n",
        "%macro create_table(suffix);\n",
        "    data output_&suffix.;\n",
        "        set input_&suffix.;\n",
        "    run;\n",
        "%mend;\n",
        "\n",
        "data input_a;\n",
        "    x = 1;\n",
        "run;\n",
        "\n",
        "%create_table(a);\n",
        "\n",
        "data final;\n",
        "    set output_a;\n",
        "run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()

      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("output_a" %in% ds)

      manifest_file <- tempfile(fileext = ".json")
      on.exit(unlink(manifest_file), add = TRUE)
      analyzer$generate_json_manifest("final", manifest_file)
      manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)

      m_ds <- vapply(manifest$operations, `[[`, character(1), "dataset")
      expect_true("final" %in% m_ds)
      expect_true("output_a" %in% m_ds)
      expect_true("input_a" %in% m_ds)
    }
  )
})

test_that("proc sql block via parse_sas_file", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "proc sql;\n",
        "create table out1 as\n",
        "select * from src1;\n",
        "create table out2 as\n",
        "select * from src2;\n",
        "quit;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "test.sas"))
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("out1" %in% ds)
      expect_true("out2" %in% ds)
    }
  )
})

# --- Uncalled top-level wrapper macro ---

test_that("uncalled top-level wrapper macro is expanded", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro lance(param);\n",
        "    data tot; set raw; run;\n",
        "    proc export data=tot\n",
        '    outfile="&rep./&param..tab1_1.csv"\n',
        '    dbms=dlm replace; delimiter=";";\n',
        "    run;\n",
        "%mend;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      ds_set <- unique(ds)
      expect_true("tab1_1" %in% ds_set,
        info = paste("tab1_1 should be discovered; got", paste(sort(ds_set), collapse = ", ")))
      expect_true("tot" %in% ds_set)
    }
  )
})

test_that("uncalled nested macro is not expanded", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro outer();\n",
        "    %macro inner(ds);\n",
        "        data &ds._x; set raw; run;\n",
        "    %mend;\n",
        "    %inner(foo);\n",
        "%mend;\n",
        "%outer();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      ds_set <- unique(ds)
      expect_true("foo_x" %in% ds_set,
        info = paste("nested macro should expand through outer call; got",
          paste(sort(ds_set), collapse = ", ")))
      expect_false("_x" %in% ds_set,
        info = "nested macro must not be expanded standalone with empty args")
    }
  )
})

test_that("called top-level macro not redundantly expanded", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro pipeline();\n",
        "    data target; set src; run;\n",
        "%mend;\n",
        "%pipeline();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      target_ops <- Filter(function(op) op$dataset == "target",
        analyzer$dataset_operations)
      expect_length(target_ops, 1L)
    }
  )
})

# --- Trace fileref alias resolution ---

test_that("trace via fileref alias resolves to canonical dataset", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        'filename tab08_31 "/results/encmco.tab08_03_1.csv";\n',
        "\n",
        "data src;\n",
        "    x = 1;\n",
        "run;\n",
        "\n",
        'ods tagsets.csv file=tab08_31 options(delimiter=";");\n',
        "    proc print data=src; run;\n",
        "ods tagsets.csv close;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()

      lineage <- analyzer$trace_dependencies("tab08_31")
      datasets <- vapply(lineage, `[[`, character(1), "dataset")
      expect_true("tab08_03_1" %in% datasets,
        info = paste("Expected canonical 'tab08_03_1'; got", paste(datasets, collapse = ", ")))
      expect_true("src" %in% datasets)

      manifest_file <- tempfile(fileext = ".json")
      on.exit(unlink(manifest_file), add = TRUE)
      analyzer$generate_json_manifest("tab08_31", manifest_file)
      manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)
      expect_equal(manifest$target_dataset, "tab08_03_1")
    }
  )
})

test_that("unknown target unchanged in alias resolution", {
  with_temp_sas_dir(
    list("main.sas" = "data a; x=1; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()
      private_env <- analyzer$.__enclos_env__$private
      expect_equal(private_env$resolve_fileref_alias("nonexistent"), "nonexistent")
    }
  )
})

test_that("known dataset target unchanged in alias resolution", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        'filename tab08_31 "/results/encmco.tab08_03_1.csv";\n',
        "data tab08_31; x=1; run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()
      private_env <- analyzer$.__enclos_env__$private
      expect_equal(private_env$resolve_fileref_alias("tab08_31"), "tab08_31")
    }
  )
})

# --- End line calculation ---

test_that("data step end_line >= line_number", {
  with_temp_sas(
    "\ndata output;\n    set input;\n    x = 1;\n    y = 2;\nrun;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 2L, f)
      op <- result$operations[[1]]
      expect_true(op$end_line >= op$line_number)
    }
  )
})

test_that("data step end_line is 1-indexed", {
  with_temp_sas(
    "data output;\n    set input;\nrun;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 1L, f)
      op <- result$operations[[1]]
      expect_equal(op$line_number, 1L)
      expect_equal(op$end_line, 3L)
    }
  )
})

test_that("proc sql end_line >= line_number", {
  with_temp_sas(
    "\nproc sql;\ncreate table output as\nselect * from input\nwhere x = 1;\nquit;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql_block(lines, 2L, f)
      op <- result$operations[[1]]
      expect_true(op$end_line >= op$line_number)
    }
  )
})

test_that("proc sql end_line is 1-indexed", {
  with_temp_sas(
    "proc sql;\ncreate table output as select * from input;\nquit;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql_block(lines, 1L, f)
      op <- result$operations[[1]]
      expect_equal(op$line_number, 2L)
      expect_equal(op$end_line, 2L)
      expect_equal(result$end_line, 3L)
    }
  )
})

test_that("one-liner data step end_line", {
  with_temp_sas(
    "data output; set input; run;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 1L, f)
      op <- result$operations[[1]]
      expect_equal(op$line_number, 1L)
      expect_true(op$end_line >= op$line_number)
    }
  )
})

test_that("multi-line data step end_line correct", {
  with_temp_sas(
    "data output;\n    set input1;\n    merge input2;\n    x = 1;\n    y = 2;\nrun;\ndata other;\n    set foo;\nrun;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 1L, f)
      op <- result$operations[[1]]
      expect_equal(op$line_number, 1L)
      expect_equal(op$end_line, 6L)
    }
  )
})

test_that("macro expansion end_line adjusted", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "\n",
        "%macro create_ds(name);\n",
        "    data &name.;\n",
        "        set input;\n",
        "        x = 1;\n",
        "    run;\n",
        "%mend;\n",
        "\n",
        "%create_ds(output);\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) op$dataset == "output",
        analyzer$dataset_operations)
      expect_true(length(ops) >= 1L)
      for (op in ops) {
        expect_true(op$end_line >= op$line_number)
      }
    }
  )
})

# --- Macro call-site tracking ---

test_that("macro data step line is call site", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "%macro mk(ds);\n",
        "  data out_&ds.;\n",
        "    set in_&ds.;\n",
        "  run;\n",
        "%mend;\n",
        "\n",
        "%mk(abc);\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) op$dataset == "out_abc",
        analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$line_number, 7L)
      expect_equal(ops[[1]]$end_line, 7L)
      expect_false(is.null(ops[[1]]$macro_end_line))
      expect_true(ops[[1]]$macro_end_line >= ops[[1]]$macro_source_line)
    }
  )
})

test_that("macro data step file is call site", {
  with_temp_sas_dir(
    list(
      "caller.sas" = paste0(
        "%macro mk();\n",
        "  data out1; set in1; run;\n",
        "%mend;\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) op$dataset == "out1",
        analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_true(grepl("caller\\.sas$", ops[[1]]$file))
    }
  )
})

test_that("macro proc sql line is call site", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "%macro mk();\n",
        "  proc sql;\n",
        "    create table out1 as select * from in1;\n",
        "  quit;\n",
        "%mend;\n",
        "\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) op$dataset == "out1",
        analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$line_number, 7L)
      expect_equal(ops[[1]]$end_line, 7L)
    }
  )
})

test_that("macro source_file and source_line set", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "%macro mk();\n",
        "  data out1;\n",
        "    set in1;\n",
        "  run;\n",
        "%mend;\n",
        "\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) op$dataset == "out1",
        analyzer$dataset_operations)
      expect_length(ops, 1L)
      op <- ops[[1]]
      expect_false(is.null(op$macro_source_file))
      expect_true(grepl("test\\.sas$", op$macro_source_file))
      expect_false(is.null(op$macro_source_line))
      expect_true(op$macro_source_line > 1L)
      expect_false(is.null(op$macro_end_line))
      expect_true(op$macro_end_line >= op$macro_source_line)
    }
  )
})

test_that("macro source_line reflects position in body", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "%macro mk();\n",
        "  data first; set in1; run;\n",
        "  data second; set in2; run;\n",
        "%mend;\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      first <- Filter(function(op) op$dataset == "first",
        analyzer$dataset_operations)
      second <- Filter(function(op) op$dataset == "second",
        analyzer$dataset_operations)
      expect_length(first, 1L)
      expect_length(second, 1L)
      expect_equal(first[[1]]$line_number, second[[1]]$line_number)
      expect_false(first[[1]]$macro_source_line == second[[1]]$macro_source_line)
    }
  )
})

test_that("non-macro operations have no macro source", {
  with_temp_sas_dir(
    list("test.sas" = "data out1; set in1; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) op$dataset == "out1",
        analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_null(ops[[1]]$macro_source_file)
      expect_null(ops[[1]]$macro_source_line)
      expect_null(ops[[1]]$macro_end_line)
    }
  )
})

test_that("macro source in JSON manifest", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "%macro mk();\n",
        "  data out1; set in1; run;\n",
        "%mend;\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()

      manifest_file <- tempfile(fileext = ".json")
      on.exit(unlink(manifest_file), add = TRUE)
      analyzer$generate_json_manifest("out1", manifest_file)
      manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)

      macro_ops <- Filter(function(op) op$dataset == "out1",
        manifest$operations)
      expect_length(macro_ops, 1L)
      expect_true("macro_source_file" %in% names(macro_ops[[1]]))
      expect_true("macro_source_line" %in% names(macro_ops[[1]]))
    }
  )
})

test_that("non-macro source absent in JSON manifest", {
  with_temp_sas_dir(
    list("test.sas" = "data out1; set in1; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()

      manifest_file <- tempfile(fileext = ".json")
      on.exit(unlink(manifest_file), add = TRUE)
      analyzer$generate_json_manifest("out1", manifest_file)
      manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)

      regular_ops <- Filter(function(op) op$dataset == "out1",
        manifest$operations)
      expect_length(regular_ops, 1L)
      expect_false("macro_source_file" %in% names(regular_ops[[1]]))
      expect_false("macro_source_line" %in% names(regular_ops[[1]]))
    }
  )
})

test_that("proc export inside macro strips substituted params (regression)", {
  with_temp_sas_dir(
    list(
      "test.sas" = paste0(
        "%macro wrap(param_ipe, param_periode);\n",
        "  proc export data=s4\n",
        '  outfile= "&rep./&param_ipe..&param_periode..tab15_1.csv"\n',
        "  dbms=dlm replace;\n",
        "  run;\n",
        "%mend;\n",
        "%wrap(param_ipe=020000063, param_periode=12);\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      export_ops <- Filter(function(op) op$operation_type == "PROC EXPORT",
        analyzer$dataset_operations)
      expect_length(export_ops, 1L)
      op <- export_ops[[1]]
      expect_equal(op$dataset, "tab15_1")
      expect_equal(op$input_datasets, "s4")
      expect_equal(op$macro_name, "wrap")
      expect_false(is.null(op$macro_source_file))
      expect_false(is.null(op$macro_source_line))
    }
  )
})

# --- Regression cases ---

test_that("one-liner regression: data dsp; set sejsac; run;", {
  with_temp_sas(
    "data dsp; set sejsac; run;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 1L, f)
      expect_length(result$operations, 1L)
      expect_equal(result$operations[[1]]$dataset, "dsp")
      expect_true("sejsac" %in% result$operations[[1]]$input_datasets)
    }
  )
})

test_that("subquery detection regression", {
  with_temp_sas(
    paste0(
      "\n",
      "create table sejcd1 as\n",
      "select * from sejcd1tmp a\n",
      "left join (select * from sejsac_ordre) b on a.id = b.id;\n"
    ),
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_proc_sql(lines, 2L, f)
      expect_true("sejcd1tmp" %in% result$input_datasets)
      expect_true("sejsac_ordre" %in% result$input_datasets)
    }
  )
})

# --- process_output fails on unknown target ---

test_that("unknown target fails group and reports all failures", {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  sas_dir <- file.path(dir, "sas")
  dir.create(sas_dir, recursive = TRUE)
  writeLines(c(
    'filename input "/data/input.csv";',
    "",
    "data resolved_out;",
    "    infile input;",
    "    input var1 var2;",
    "run;"
  ), file.path(sas_dir, "main.sas"))

  output_dir_1 <- file.path(dir, "lineage", "resolved_out")
  output_dir_2 <- file.path(dir, "lineage", "ghost_out")
  output_dir_3 <- file.path(dir, "lineage", "ghost_two")

  outputs <- c("resolved_out", "ghost_out", "ghost_two")
  output_dirs <- c(output_dir_1, output_dir_2, output_dir_3)

  rc <- run_multiple(sas_dir, outputs, output_dirs)
  expect_equal(rc, 1L)
  expect_true(file.exists(file.path(output_dir_1, "lineage-manifest.json")))
  expect_false(file.exists(file.path(output_dir_2, "lineage-manifest.json")))
  expect_false(file.exists(file.path(output_dir_3, "lineage-manifest.json")))
})

# --- Coverage-targeted tests (added) ---

test_that(".has_body returns FALSE for NULL macro_def", {
  fn <- get(".has_body", envir = asNamespace("DATALineage"))
  expect_false(fn(NULL))
})

test_that(".has_body returns FALSE for empty / whitespace body", {
  fn <- get(".has_body", envir = asNamespace("DATALineage"))
  expect_false(fn(list(body = character(0))))
  expect_false(fn(list(body = c("", "   "))))
  expect_true(fn(list(body = c("", "data x; run;"))))
})

test_that("file-level PROC SORT with out= records input and output", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "proc sort data=raw out=sorted;\n",
        "  by id;\n",
        "run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) {
        op$operation_type == "PROC SORT"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "sorted")
      expect_true("raw" %in% ops[[1]]$input_datasets)
    }
  )
})

test_that("file-level generic PROC TRANSPOSE records operation", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "proc transpose data=src out=transposed;\n",
        "  var x;\n",
        "run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) {
        op$operation_type == "PROC TRANSPOSE"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "transposed")
      expect_true("src" %in% ops[[1]]$input_datasets)
    }
  )
})

test_that("file-level generic PROC with no output token is still handled", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "proc means data=src;\n",
        "  var x;\n",
        "run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) {
        op$operation_type == "PROC MEANS"
      }, analyzer$dataset_operations)
      expect_length(ops, 0L)
    }
  )
})

test_that("file-level PROC EXPORT records operation", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "proc export data=mydata\n",
        '  outfile="/out/report.tab9_1.csv"\n',
        "  dbms=dlm replace;\n",
        "run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) {
        op$operation_type == "PROC EXPORT"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "tab9_1")
      expect_true("mydata" %in% ops[[1]]$input_datasets)
    }
  )
})

test_that("file-level PROC EXPORT with no outfile is skipped", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "proc export data=mydata dbms=dlm replace;\n",
        "run;\n",
        "data after; set tail; run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      export_ops <- Filter(function(op) {
        op$operation_type == "PROC EXPORT"
      }, analyzer$dataset_operations)
      expect_length(export_ops, 0L)
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("after" %in% ds)
    }
  )
})

test_that("file-level PROC FORMAT records format operations", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "proc format;\n",
        "  value sexf 1='M' 2='F';\n",
        "run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) {
        op$operation_type == "PROC FORMAT"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "fmt:sexf")
    }
  )
})

test_that("%let with mv:sqlobs input is rewritten to last SQL output", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "proc sql;\n",
        "  create table sql_out as select * from src;\n",
        "quit;\n",
        "%let nobs = &sqlobs.;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "main.sas"))
      let_ops <- Filter(function(op) {
        op$dataset == "mv:nobs"
      }, analyzer$dataset_operations)
      expect_length(let_ops, 1L)
      expect_true("sql_out" %in% let_ops[[1]]$input_datasets)
      expect_false("mv:sqlobs" %in% let_ops[[1]]$input_datasets)
    }
  )
})

test_that("%let with mv:sqlobs but no prior SQL output keeps original input", {
  with_temp_sas_dir(
    list(
      "main.sas" = "%let nobs = &sqlobs.;\n"
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "main.sas"))
      let_ops <- Filter(function(op) {
        op$dataset == "mv:nobs"
      }, analyzer$dataset_operations)
      expect_length(let_ops, 1L)
      expect_true("mv:sqlobs" %in% let_ops[[1]]$input_datasets)
    }
  )
})

test_that("plain %let without sqlobs is recorded as macro variable op", {
  with_temp_sas_dir(
    list(
      "main.sas" = "%let myvar = somelib.sometable;\n"
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "main.sas"))
      let_ops <- Filter(function(op) {
        op$dataset == "mv:myvar"
      }, analyzer$dataset_operations)
      expect_length(let_ops, 1L)
    }
  )
})

test_that("unterminated %macro block stops parsing at end of file", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "data before; set src; run;\n",
        "%macro never_closed();\n",
        "  data inside; set raw; run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "main.sas"))
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("before" %in% ds)
      expect_false("inside" %in% ds)
    }
  )
})

test_that("macro-call handler ignores control-flow keyword pseudo-calls", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%if (1=1) %then %do;\n",
        "  data conditional; set src; run;\n",
        "%end;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      expect_no_error(analyzer$parse_sas_file(file.path(dir, "main.sas")))
    }
  )
})

test_that("trace_dependencies stops on already-visited dataset (cycle)", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "data alpha; set beta; run;\n",
        "data beta; set alpha; run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()
      lineage <- analyzer$trace_dependencies("alpha")
      datasets <- vapply(lineage, `[[`, character(1), "dataset")
      expect_true("alpha" %in% datasets)
      expect_true("beta" %in% datasets)
    }
  )
})

test_that("trace_dependencies respects max_depth limit", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "data alpha; set beta; run;\n",
        "data beta; set gamma; run;\n",
        "data gamma; set delta; run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()
      lineage <- analyzer$trace_dependencies("alpha", max_depth = 1L)
      datasets <- vapply(lineage, `[[`, character(1), "dataset")
      expect_true("alpha" %in% datasets)
      expect_true("beta" %in% datasets)
      expect_false("gamma" %in% datasets)
    }
  )
})

test_that("static %let macro variable reference is substituted (fixed-point)", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%let root = base;\n",
        "%let full = &root._suffix;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      expect_equal(analyzer$macro_variables[["full"]], "base_suffix")
    }
  )
})

test_that("static %substr macro function is evaluated", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%let base = abcdef;\n",
        "%let part = %substr(abcdef, 2, 3);\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      expect_equal(analyzer$macro_variables[["part"]], "bcd")
    }
  )
})

test_that("static %substr without length runs to end of string", {
  with_temp_sas_dir(
    list(
      "main.sas" = "%let part = %substr(abcdef, 3);\n"
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      expect_equal(analyzer$macro_variables[["part"]], "cdef")
    }
  )
})

test_that("static %substr with non-numeric start leaves unresolved value filtered out", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%let kept = plainvalue;\n",
        "%let part = %substr(abcdef, x);\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      expect_equal(analyzer$macro_variables[["kept"]], "plainvalue")
      expect_null(analyzer$macro_variables[["part"]])
    }
  )
})

test_that("evaluate_static_macro_funcs handles substr edge cases directly", {
  with_temp_sas_dir(
    list("main.sas" = "data a; x=1; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ev <- analyzer$.__enclos_env__$private$evaluate_static_macro_funcs
      expect_equal(ev("%substr(hello, 2, 3)"), "ell")
      expect_equal(ev("%substr(hello, 3)"), "llo")
      expect_equal(ev("%substr(&v, 2, 3)"), "%substr(&v, 2, 3)")
      expect_equal(ev("%substr(hello, x)"), "%substr(hello, x)")
      expect_equal(ev("%substr(hello, 2, y)"), "%substr(hello, 2, y)")
      expect_equal(ev("no funcs here"), "no funcs here")
    }
  )
})

test_that("cross-procedure %include pulls in additional SAS files", {
  dir <- tempfile("proj_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  proc_a_sas <- file.path(dir, "procedures", "migration-a", "sas")
  proc_b_sas <- file.path(dir, "procedures", "migration-b", "sas")
  dir.create(proc_a_sas, recursive = TRUE)
  dir.create(proc_b_sas, recursive = TRUE)

  included <- file.path(proc_b_sas, "helper.sas")
  writeLines(c(
    "data helper_out;",
    "  set helper_src;",
    "run;"
  ), included)

  writeLines(c(
    "data main_out;",
    "  set main_src;",
    "run;",
    sprintf('%%include "%s";', included)
  ), file.path(proc_a_sas, "main.sas"))

  analyzer <- SASLineageAnalyzer$new(proc_a_sas)
  expect_message(
    analyzer$parse_all_sas_files(),
    regexp = "additional SAS file"
  )
  ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
  expect_true("helper_out" %in% ds)
})

test_that("default_include_search_roots finds sibling migration sas dirs", {
  dir <- tempfile("proj_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # sas_dir at <root>/migration-a/sas so dirname(dirname(sas_dir)) == <root>,
  # and the sibling migration sas dirs live under <root>/procedures/.
  own_sas <- file.path(dir, "migration-a", "sas")
  proc_a_sas <- file.path(dir, "procedures", "migration-a", "sas")
  proc_b_sas <- file.path(dir, "procedures", "migration-b", "sas")
  dir.create(own_sas, recursive = TRUE)
  dir.create(proc_a_sas, recursive = TRUE)
  dir.create(proc_b_sas, recursive = TRUE)
  writeLines("data x; run;", file.path(own_sas, "main.sas"))

  analyzer <- SASLineageAnalyzer$new(own_sas)
  roots <- analyzer$.include_search_roots
  expect_true(any(grepl("migration-a", roots)))
  expect_true(any(grepl("migration-b", roots)))
})

test_that("default_include_search_roots returns empty when no procedures dir", {
  with_temp_sas_dir(
    list("main.sas" = "data x; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      expect_equal(analyzer$.include_search_roots, character(0))
    }
  )
})

test_that("nested macro CALL inside expanded macro is recursively parsed", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro inner(suffix);\n",
        "  data inner_&suffix.; set inner_src; run;\n",
        "%mend;\n",
        "\n",
        "%macro outer();\n",
        "  data outer_ds; set os; run;\n",
        "  %inner(z);\n",
        "%mend;\n",
        "\n",
        "%outer();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("outer_ds" %in% ds)
      expect_true("inner_z" %in% ds)
    }
  )
})

test_that("PROC SORT inside expanded macro is captured", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro mk();\n",
        "  proc sort data=msrc out=msorted;\n",
        "    by id;\n",
        "  run;\n",
        "%mend;\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) {
        op$operation_type == "PROC SORT"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "msorted")
      expect_equal(ops[[1]]$macro_name, "mk")
    }
  )
})

test_that("PROC FORMAT inside expanded macro is captured", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro mk();\n",
        "  proc format;\n",
        "    value myfmt 1='a';\n",
        "  run;\n",
        "%mend;\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) {
        op$operation_type == "PROC FORMAT"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "fmt:myfmt")
    }
  )
})

test_that("generic PROC inside expanded macro is captured", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro mk();\n",
        "  proc transpose data=tsrc out=tout;\n",
        "    var v;\n",
        "  run;\n",
        "%mend;\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ops <- Filter(function(op) {
        op$operation_type == "PROC TRANSPOSE"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "tout")
    }
  )
})

test_that("ODS CSV inside expanded macro is captured", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        'filename out_ref "/results/encmco.tab12_1.csv";\n',
        "%macro mk();\n",
        '  ods tagsets.csv file=out_ref options(delimiter=";");\n',
        "    proc print data=psrc; run;\n",
        "  ods tagsets.csv close;\n",
        "%mend;\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("tab12_1" %in% ds)
    }
  )
})

test_that("%let inside expanded macro is captured with macro source", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro mk();\n",
        "  %let inner_var = somelib.sometab;\n",
        "  data after_let; set src; run;\n",
        "%mend;\n",
        "%mk();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      let_ops <- Filter(function(op) {
        op$dataset == "mv:inner_var"
      }, analyzer$dataset_operations)
      expect_length(let_ops, 1L)
      expect_equal(let_ops[[1]]$macro_name, "mk")
    }
  )
})

test_that("nested macro DEFINITION inside expansion is skipped", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro outer();\n",
        "  %macro nested_def(ds);\n",
        "    data &ds._n; set ns; run;\n",
        "  %mend;\n",
        "  data direct; set ds_src; run;\n",
        "  %nested_def(used);\n",
        "%mend;\n",
        "%outer();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("direct" %in% ds)
      expect_true("used_n" %in% ds)
      expect_false("_n" %in% ds)
    }
  )
})

test_that("PROC EXPORT inside uncalled top-level macro records source lines", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro lance(param);\n",
        "  data tot; set raw; run;\n",
        "  proc export data=tot\n",
        '  outfile="&rep./&param..tab1_1.csv"\n',
        "  dbms=dlm replace;\n",
        "  run;\n",
        "%mend;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      export_ops <- Filter(function(op) {
        op$operation_type == "PROC EXPORT"
      }, analyzer$dataset_operations)
      expect_length(export_ops, 1L)
      op <- export_ops[[1]]
      expect_equal(op$dataset, "tab1_1")
      expect_false(is.null(op$macro_source_line))
    }
  )
})

test_that("two top-level macro defs in one file are not auto-expanded", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro first();\n",
        "  data first_ds; set fs; run;\n",
        "%mend;\n",
        "%macro second();\n",
        "  data second_ds; set ss; run;\n",
        "%mend;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_false("first_ds" %in% ds)
      expect_false("second_ds" %in% ds)
    }
  )
})

test_that("uncalled top-level macro with params expands with empty args", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro only(param1, param2);\n",
        "  data fixed_ds; set fixed_src; run;\n",
        "%mend;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("fixed_ds" %in% ds)
    }
  )
})

test_that("macro_end_line returns NULL for out-of-range start line", {
  with_temp_sas_dir(
    list("main.sas" = "%macro m(); data x; run; %mend;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      mel <- analyzer$.__enclos_env__$private$macro_end_line
      fp <- file.path(dir, "main.sas")
      expect_null(mel(fp, 0L))
      expect_null(mel(fp, 9999L))
    }
  )
})

test_that("macro_end_line returns NULL when no %mend is found", {
  with_temp_sas_dir(
    list("unclosed.sas" = "%macro m();\n  data x; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      mel <- analyzer$.__enclos_env__$private$macro_end_line
      fp <- file.path(dir, "unclosed.sas")
      expect_null(mel(fp, 1L))
    }
  )
})

# --- Coverage-targeted tests, batch 2 ---

test_that("evaluate_static_macro_funcs returns input on integer overflow start", {
  with_temp_sas_dir(
    list("main.sas" = "data a; x=1; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ev <- analyzer$.__enclos_env__$private$evaluate_static_macro_funcs
      expect_equal(ev("%substr(hello, 99999999999)"), "%substr(hello, 99999999999)")
      expect_equal(ev("%substr(hello, 2, 99999999999)"), "%substr(hello, 2, 99999999999)")
    }
  )
})

test_that("%let without = is detected but not recorded as operation", {
  with_temp_sas_dir(
    list("main.sas" = "%let novalue;\ndata after; set src; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "main.sas"))
      mv_ops <- Filter(function(op) {
        grepl("^mv:", op$dataset)
      }, analyzer$dataset_operations)
      expect_length(mv_ops, 0L)
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("after" %in% ds)
    }
  )
})

test_that("ODS trigger with quoted file= does not parse and parsing continues", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        'ods csv file="/tmp/x.csv";\n',
        "data after_ods; set src; run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "main.sas"))
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("after_ods" %in% ds)
    }
  )
})

test_that("standalone %mend line is ignored by macro-call handler", {
  with_temp_sas_dir(
    list("main.sas" = "%mend;\ndata after_mend; set src; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      expect_no_error(analyzer$parse_sas_file(file.path(dir, "main.sas")))
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("after_mend" %in% ds)
    }
  )
})

test_that("%include with non-existent target is skipped during pass1 closure", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "data main_ds; set ms; run;\n",
        '%include "/nonexistent/path/missing.sas";\n'
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      expect_no_error(analyzer$parse_all_sas_files())
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("main_ds" %in% ds)
    }
  )
})

test_that("uncalled top-level macro with empty body produces no operations", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro shell();\n",
        "%mend;\n",
        "data outside; set os; run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("outside" %in% ds)
    }
  )
})

test_that("resolve_fileref_alias returns input when canonical has no operations", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        'filename orphan "/results/encmco.orphan_tab.csv";\n',
        "data unrelated; set us; run;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      analyzer$deduplicate_operations()
      priv <- analyzer$.__enclos_env__$private
      expect_equal(priv$resolve_fileref_alias("orphan"), "orphan")
    }
  )
})

test_that("nested macro CALL falls back to global macro_definitions when unresolved", {
  with_temp_sas_dir(
    list(
      "lib.sas" = paste0(
        "%macro helper(suffix);\n",
        "  data helper_&suffix.; set hs; run;\n",
        "%mend;\n"
      ),
      "main.sas" = paste0(
        "%macro driver();\n",
        "  data driver_ds; set ds; run;\n",
        "  %helper(q);\n",
        "%mend;\n",
        "%driver();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("driver_ds" %in% ds)
      expect_true("helper_q" %in% ds)
    }
  )
})

test_that("nested unterminated macro DEFINITION inside expansion breaks cleanly", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro outer();\n",
        "  data before_nested; set bs; run;\n",
        "  %macro broken(ds);\n",
        "    data &ds._b; set bb; run;\n",
        "%mend;\n",
        "%outer();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      expect_no_error(analyzer$parse_all_sas_files())
      expect_type(analyzer$dataset_operations, "list")
    }
  )
})

# --- Coverage-targeted tests, batch 3 ---

test_that("static %let with both resolvable and unresolvable refs", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%let known = base;\n",
        "%let mixed = &known.&unknownref.;\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      # known ref resolves; the unknown ref token is left verbatim.
      expect_equal(analyzer$macro_variables[["known"]], "base")
      expect_equal(analyzer$macro_variables[["mixed"]], "base&unknownref.")
    }
  )
})

test_that("cyclic %include between two files terminates and parses both", {
  dir <- tempfile("proj_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sas_dir <- file.path(dir, "sas")
  dir.create(sas_dir, recursive = TRUE)

  file_a <- file.path(sas_dir, "a.sas")
  file_b <- file.path(sas_dir, "b.sas")
  writeLines(c(
    "data a_ds; set a_src; run;",
    sprintf('%%include "%s";', file_b)
  ), file_a)
  writeLines(c(
    "data b_ds; set b_src; run;",
    sprintf('%%include "%s";', file_a)
  ), file_b)

  analyzer <- SASLineageAnalyzer$new(sas_dir)
  expect_no_error(analyzer$parse_all_sas_files())
  ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
  expect_true("a_ds" %in% ds)
  expect_true("b_ds" %in% ds)
})

test_that("proc export in macro body that cannot be parsed is skipped", {
  with_temp_sas_dir(
    list(
      "main.sas" = paste0(
        "%macro wrap();\n",
        "  proc export data=src dbms=dlm replace;\n",
        "  run;\n",
        "  data wrap_ds; set ws; run;\n",
        "%mend;\n",
        "%wrap();\n"
      )
    ),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      export_ops <- Filter(function(op) {
        op$operation_type == "PROC EXPORT"
      }, analyzer$dataset_operations)
      expect_length(export_ops, 0L)
      ds <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("wrap_ds" %in% ds)
    }
  )
})

# --- Coverage-targeted tests, batch 4 (defensive expansion paths) ---
# parse_expanded_macro_lines and collect_proc_exports_from_macro are shared by
# two public entry points (handle_macro_call and expand_uncalled_top_level_macros);
# these malformed-expansion guards are stricter than what either wrapper exposes,
# so they are exercised directly.

test_that("parse_expanded_macro_lines breaks on unterminated nested %macro", {
  with_temp_sas_dir(
    list("main.sas" = "data seed; x=1; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      pem <- analyzer$.__enclos_env__$private$parse_expanded_macro_lines
      fp <- file.path(dir, "main.sas")
      before <- length(analyzer$dataset_operations)
      expanded <- c("%macro nested_inner(x)", "  data z; set zz; run;")
      pem(
        expanded, c(10L, 11L), 5L, fp,
        macro_name = "m", macro_def_file = fp
      )
      # The unterminated nested def aborts the scan; nothing is added after it.
      expect_equal(length(analyzer$dataset_operations), before)
    }
  )
})

test_that("parse_expanded_macro_lines tolerates empty source-line mapping", {
  with_temp_sas_dir(
    list("main.sas" = "data seed; x=1; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      pem <- analyzer$.__enclos_env__$private$parse_expanded_macro_lines
      fp <- file.path(dir, "main.sas")
      expanded <- c("data mapped_ds;", "  set mapped_src;", "run;")
      pem(
        expanded, integer(0), 5L, fp,
        macro_name = "m", macro_def_file = fp
      )
      ops <- Filter(function(op) {
        op$dataset == "mapped_ds"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_null(ops[[1]]$macro_source_line)
    }
  )
})

test_that("collect_proc_exports_from_macro falls back when end_idx exceeds source lines", {
  with_temp_sas_dir(
    list("main.sas" = "data seed; x=1; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_all_sas_files()
      cpe <- analyzer$.__enclos_env__$private$collect_proc_exports_from_macro
      fp <- file.path(dir, "main.sas")
      macro_def <- list(
        body = c(
          "proc export data=src",
          '  outfile="/tmp/r.tabx_1.csv" dbms=dlm;',
          "run;"
        ),
        body_source_lines = c(20L),
        file = fp
      )
      cpe(macro_def, call_line = 5L, call_file = fp, macro_name = "m")
      ops <- Filter(function(op) {
        op$operation_type == "PROC EXPORT"
      }, analyzer$dataset_operations)
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "tabx_1")
      expect_equal(ops[[1]]$macro_source_line, 20L)
      expect_equal(ops[[1]]$macro_end_line, 20L)
    }
  )
})
