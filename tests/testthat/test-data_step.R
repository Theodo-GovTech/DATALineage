# test-data_step.R — Tests for data_step.R (DATA step parsing)

test_that("simple data step", {
  with_temp_sas(
    "  \ndata output;\n    set input;\nrun;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 2L, f)
      ops <- result$operations
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "output")
      expect_equal(ops[[1]]$operation_type, "DATA")
      expect_true("input" %in% ops[[1]]$input_datasets)
    }
  )
})

test_that("one-line data step", {
  with_temp_sas(
    "data dsp; set sejsac; run;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 1L, f)
      ops <- result$operations
      expect_length(ops, 1L)
      expect_equal(ops[[1]]$dataset, "dsp")
      expect_true("sejsac" %in% ops[[1]]$input_datasets)
    }
  )
})

test_that("multi-output data step", {
  with_temp_sas(
    "\ndata out1(keep=a) out2(keep=b) out3;\n    set input;\nrun;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 2L, f)
      ops <- result$operations
      expect_length(ops, 3L)
      ds_names <- vapply(ops, `[[`, character(1), "dataset")
      expect_true("out1" %in% ds_names)
      expect_true("out2" %in% ds_names)
      expect_true("out3" %in% ds_names)
      for (op in ops) {
        expect_true("input" %in% op$input_datasets)
      }
    }
  )
})

test_that("data step with nested parens", {
  with_temp_sas(
    "\ndata output;\n    merge dataset1(in=a where=(x=1)) dataset2(in=b where=(y=2));\nrun;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 2L, f)
      ops <- result$operations
      expect_length(ops, 1L)
      inputs <- ops[[1]]$input_datasets
      expect_true("dataset1" %in% inputs)
      expect_true("dataset2" %in% inputs)
      expect_false("in" %in% tolower(inputs))
      expect_false("where" %in% tolower(inputs))
    }
  )
})

test_that("data step with infile", {
  with_temp_sas(
    "\ndata output;\n    infile myfile;\n    input var1 var2;\nrun;\n",
    function(f) {
      lines <- readLines(f, warn = FALSE)
      result <- parse_data_step(lines, 2L, f)
      ops <- result$operations
      expect_length(ops, 1L)
      expect_true("INFILE:myfile" %in% ops[[1]]$input_datasets)
      expect_length(result$infile_records, 1L)
      expect_equal(result$infile_records[[1]][[2]], "myfile")
    }
  )
})

test_that("data step SET with macro %if/%then/%do captures all datasets (regression)", {
  sas_code <- paste0(
    "data justif_sa;\n",
    "    set %if &sejmin_existe=1 %then %do;\n",
    "        justif_tx_sal\n",
    "        %end;\n",
    "        justif_tx_lib\n",
    "        justif_heure_100_sal\n",
    "        justif_heure_varb_sal\n",
    "        justif_passage_varb_sal\n",
    "        justif_passage_varb_lib\n",
    "        justif_bcmss_n_1\n",
    "        justif_bcmss_ref\n",
    "        justif_cs_n_1\n",
    "        justif_cs_ref;\n",
    "run;\n"
  )
  with_temp_sas(sas_code, function(f) {
    lines <- readLines(f, warn = FALSE)
    result <- parse_data_step(lines, 1L, f)
    ops <- result$operations
    expect_length(ops, 1L)
    inputs <- ops[[1]]$input_datasets
    expected <- c(
      "justif_tx_sal", "justif_tx_lib", "justif_heure_100_sal",
      "justif_heure_varb_sal", "justif_passage_varb_sal",
      "justif_passage_varb_lib", "justif_bcmss_n_1",
      "justif_bcmss_ref", "justif_cs_n_1", "justif_cs_ref"
    )
    for (ds in expected) {
      expect_true(ds %in% inputs, info = paste(ds, "missing from SET inputs"))
    }
    lowered <- tolower(inputs)
    for (bogus in c("if", "then", "do", "end", "else")) {
      expect_false(bogus %in% lowered,
        info = paste("macro keyword", bogus, "leaked into SET inputs"))
    }
  })
})

test_that("data step SET with %if/%then/%do inside dataset list - 7 dataset variant", {
  sas_code <- paste0(
    "data justif_sa;\n",
    "    set %if &sejmin_existe=1 %then %do;\n",
    "        justif_tx_sal\n",
    "        %end;\n",
    "        justif_tx_lib\n",
    "        justif_heure_100_sal\n",
    "        justif_heure_varb_sal;\n",
    "run;\n"
  )
  with_temp_sas(sas_code, function(f) {
    lines <- readLines(f, warn = FALSE)
    result <- parse_data_step(lines, 1L, f)
    ops <- result$operations
    expect_length(ops, 1L)
    inputs <- ops[[1]]$input_datasets
    expect_true("justif_tx_sal" %in% inputs)
    expect_true("justif_tx_lib" %in% inputs)
  })
})
