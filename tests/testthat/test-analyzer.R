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
