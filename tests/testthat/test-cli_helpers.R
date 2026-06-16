# test-cli_helpers.R — Tests for cli_helpers.R (run_analyzer / run_multiple /
# .parse_sources / .process_output)

# A lightweight fake analyzer standing in for SASLineageAnalyzer$new(), so the
# CLI orchestration can be exercised without parsing real SAS sources.
.make_fake_analyzer <- function(lineage_for = function(target) {
                                  c(target, "dep1")
                                }) {
  list(
    dataset_operations = list(1, 2, 3),
    dataset_to_ops = list(a = 1, b = 2),
    infile_usage = list(x = 1),
    parse_all_sas_files = function() {
      invisible(NULL)
    },
    deduplicate_operations = function() {
      invisible(NULL)
    },
    trace_dependencies = function(target) {
      lineage_for(target)
    },
    generate_report = function(target, file, filter_orphan_nodes = TRUE) {
      writeLines("report", file)
    },
    generate_json_manifest = function(target, file, filter_orphan_nodes = TRUE) {
      writeLines("{}", file)
    }
  )
}

test_that("run_multiple errors when output_dirs length does not match outputs", {
  rc <- NULL
  msg <- capture_messages({
    rc <- run_multiple(
      sas_dir = "/nonexistent",
      outputs = c("a", "b"),
      output_dirs = c("only-one")
    )
  })
  expect_equal(rc, 1L)
  expect_true(any(grepl("must match outputs length", msg)))
})

test_that(".parse_sources errors when sas_dir does not exist", {
  missing <- file.path(tempfile("nope_"), "sub")
  result <- NULL
  msg <- capture_messages({
    result <- DATALineage:::.parse_sources(missing)
  })
  expect_null(result)
  expect_true(any(grepl("SAS directory not found", msg)))
})

test_that(".parse_sources errors when sas_dir is a file, not a directory", {
  f <- tempfile(fileext = ".sas")
  writeLines("data x; run;", f)
  on.exit(unlink(f), add = TRUE)
  result <- NULL
  msg <- capture_messages({
    result <- DATALineage:::.parse_sources(f)
  })
  expect_null(result)
  expect_true(any(grepl("Not a directory", msg)))
})

test_that("run_multiple returns 1 when .parse_sources fails", {
  rc <- NULL
  suppressMessages({
    rc <- run_multiple(
      sas_dir = file.path(tempfile("nope_")),
      outputs = c("a"),
      output_dirs = c(tempfile("od_"))
    )
  })
  expect_equal(rc, 1L)
})

test_that("run_multiple succeeds and writes report + manifest", {
  out_dir <- tempfile("od_")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    .parse_sources = function(sas_dir) {
      .make_fake_analyzer()
    },
    clean_dataset_name = function(name) {
      tolower(name)
    }
  )

  rc <- NULL
  msg <- capture_messages({
    rc <- run_multiple(
      sas_dir = "ignored",
      outputs = c("out1"),
      output_dirs = c(out_dir)
    )
  })
  expect_equal(rc, 0L)
  expect_true(any(grepl("Analysis complete!", msg)))
  expect_true(file.exists(file.path(out_dir, "lineage-report.md")))
  expect_true(file.exists(file.path(out_dir, "lineage-manifest.json")))
})

test_that("run_multiple reports failure when an output has no lineage", {
  out_dir <- tempfile("od_")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    .parse_sources = function(sas_dir) {
      .make_fake_analyzer(lineage_for = function(target) {
        character(0)
      })
    },
    clean_dataset_name = function(name) {
      tolower(name)
    }
  )

  rc <- NULL
  msg <- capture_messages({
    rc <- run_multiple(
      sas_dir = "ignored",
      outputs = c("ghost"),
      output_dirs = c(out_dir)
    )
  })
  expect_equal(rc, 1L)
  expect_true(any(grepl("produced no lineage", msg)))
})

test_that(".process_output falls back to tolower when clean_dataset_name is NULL", {
  out_dir <- tempfile("od_")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    clean_dataset_name = function(name) {
      NULL
    }
  )

  analyzer <- .make_fake_analyzer(lineage_for = function(target) {
    expect_equal(target, "myout")
    c(target)
  })

  rc <- NULL
  suppressMessages({
    rc <- DATALineage:::.process_output(analyzer, "MyOut", out_dir)
  })
  expect_equal(rc, 0L)
})

test_that("run_analyzer composes output_dirs and forwards to run_multiple", {
  captured <- NULL
  local_mocked_bindings(
    run_multiple = function(sas_dir, outputs, output_dirs) {
      captured <<- list(sas_dir = sas_dir, outputs = outputs,
                        output_dirs = output_dirs)
      0L
    }
  )
  rc <- run_analyzer(
    sas_dir = "/some/sas",
    output_dir = "/base/out",
    outputs = c("a", "b")
  )
  expect_equal(rc, 0L)
  expect_equal(captured$sas_dir, "/some/sas")
  expect_equal(captured$outputs, c("a", "b"))
  expect_equal(captured$output_dirs,
               file.path("/base/out", c("a", "b")))
})
