# Tests for run_lineage_and_graph() cascade
# Ported from: test_generate_lineage_and_graph.py

# Helper: create a side-effect function for run_multiple that materialises
# stub manifests for specified targets (or all outputs if none specified).
# Mirrors _make_manifests_side_effect() in the Python tests.
.make_manifests_side_effect <- function(...) {
  targets <- c(...)
  function(sas_dir, outputs, output_dirs) {
    wanted <- if (length(targets) > 0L) targets else outputs
    for (i in seq_along(outputs)) {
      if (outputs[i] %in% wanted) {
        dir.create(output_dirs[i], recursive = TRUE, showWarnings = FALSE)
        writeLines("{}", file.path(output_dirs[i], "lineage-manifest.json"))
      }
    }
    0L
  }
}

# ===========================================================================
# TestMain: success / failure / forwarding
# ===========================================================================
test_that("run_lineage_and_graph: calls both stages and returns 0 on success", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  stage2_called <- FALSE
  stage2_args <- NULL

  local_mocked_bindings(
    run_multiple = .make_manifests_side_effect(),
    run_operations_graph = function(...) {
      stage2_called <<- TRUE
      stage2_args <<- list(...)
      0L
    }
  )

  rc <- run_lineage_and_graph(
    procedure = "my-proc",
    path_to_sas_entrypoint = "main.sas",
    group = "grp",
    outputs = c("out1"),
    format = "dot",
    verbose = FALSE,
    project_root = tmp
  )

  expect_equal(rc, 0L)
  expect_true(stage2_called)
  expect_equal(stage2_args$procedure, "my-proc")
  expect_equal(stage2_args$group, "grp")
  expect_equal(stage2_args$outputs, "out1")
})

test_that("run_lineage_and_graph: trace_lineage failure skips graph", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  stage2_called <- FALSE

  local_mocked_bindings(
    run_multiple = function(sas_dir, outputs, output_dirs) 1L,
    run_operations_graph = function(...) {
      stage2_called <<- TRUE
      0L
    }
  )

  rc <- run_lineage_and_graph(
    procedure = "my-proc",
    path_to_sas_entrypoint = "main.sas",
    group = "grp",
    outputs = c("out1"),
    project_root = tmp
  )

  expect_equal(rc, 1L)
  expect_false(stage2_called)
})

test_that("run_lineage_and_graph: graph failure propagates", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    run_multiple = .make_manifests_side_effect(),
    run_operations_graph = function(...) 2L
  )

  rc <- run_lineage_and_graph(
    procedure = "my-proc",
    path_to_sas_entrypoint = "main.sas",
    group = "grp",
    outputs = c("out1"),
    project_root = tmp
  )

  expect_equal(rc, 2L)
})

test_that("run_lineage_and_graph: passes format and verbose to graph stage", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  captured_args <- NULL

  local_mocked_bindings(
    run_multiple = .make_manifests_side_effect(),
    run_operations_graph = function(...) {
      captured_args <<- list(...)
      0L
    }
  )

  run_lineage_and_graph(
    procedure = "my-proc",
    path_to_sas_entrypoint = "main.sas",
    group = "grp",
    outputs = c("out1"),
    format = "llm",
    verbose = TRUE,
    project_root = tmp
  )

  expect_equal(captured_args$format, "llm")
  expect_true(captured_args$verbose)
})

test_that("run_lineage_and_graph: multiple outputs runs combined graph", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  tl_args <- NULL
  gr_args <- NULL

  local_mocked_bindings(
    run_multiple = function(sas_dir, outputs, output_dirs) {
      tl_args <<- list(sas_dir = sas_dir, outputs = outputs,
                       output_dirs = output_dirs)
      # Create manifests for all outputs
      for (i in seq_along(outputs)) {
        dir.create(output_dirs[i], recursive = TRUE, showWarnings = FALSE)
        writeLines("{}", file.path(output_dirs[i], "lineage-manifest.json"))
      }
      0L
    },
    run_operations_graph = function(...) {
      gr_args <<- list(...)
      0L
    }
  )

  rc <- run_lineage_and_graph(
    procedure = "my-proc",
    path_to_sas_entrypoint = "main.sas",
    group = "grp",
    outputs = c("a", "b", "c"),
    project_root = tmp
  )

  expect_equal(rc, 0L)

  # trace_lineage called with all outputs and correct sas_dir
  sas_dir_suffix <- file.path("migration-my-proc", "sas")
  expect_true(grepl(sas_dir_suffix, tl_args$sas_dir, fixed = TRUE))
  expect_equal(tl_args$outputs, c("a", "b", "c"))

  # per-output dirs are under migration-data/grp/lineage/
  group_dir_suffix <- file.path("migration-my-proc", "migration-data", "grp", "lineage")
  expect_true(grepl(group_dir_suffix, tl_args$output_dirs[1], fixed = TRUE))
  expect_equal(basename(tl_args$output_dirs), c("a", "b", "c"))

  # graph stage called with all outputs
  expect_equal(gr_args$procedure, "my-proc")
  expect_equal(gr_args$path_to_sas_entrypoint, "main.sas")
  expect_equal(gr_args$group, "grp")
  expect_equal(gr_args$outputs, c("a", "b", "c"))
})

test_that("run_lineage_and_graph: tolerates whitespace around commas in parse_graph_outputs", {
  # parse_graph_outputs is the function that handles whitespace; verify it
  # works correctly since the cascade relies on it for outputs parsing
  expect_equal(parse_graph_outputs(" a , b , c "), c("a", "b", "c"))
})

# ===========================================================================
# TestMainFailsWhenOutputsHaveNoLineage
# ===========================================================================
test_that("run_lineage_and_graph: fails and reports when any output has no manifest", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Pre-create one manifest but not the other
  procedure_root <- file.path(tmp, "procedures", "migration-toy")
  group_dir <- file.path(procedure_root, "migration-data", "grp", "lineage")
  dir.create(file.path(group_dir, "out_resolved"), recursive = TRUE)
  writeLines("{}", file.path(group_dir, "out_resolved", "lineage-manifest.json"))
  dir.create(file.path(group_dir, "out_ghost"), recursive = TRUE)
  # No manifest for out_ghost

  stage2_called <- FALSE

  local_mocked_bindings(
    run_multiple = function(sas_dir, outputs, output_dirs) 1L,
    run_operations_graph = function(...) {
      stage2_called <<- TRUE
      0L
    }
  )

  output <- capture.output({
    rc <- run_lineage_and_graph(
      procedure = "toy",
      path_to_sas_entrypoint = "main.sas",
      group = "grp",
      outputs = c("out_resolved", "out_ghost"),
      project_root = tmp
    )
  })

  expect_equal(rc, 1L)
  expect_false(stage2_called)

  combined <- paste(output, collapse = "\n")
  expect_true(grepl("CASCADE FAILED", combined, fixed = TRUE))
  expect_true(grepl("out_ghost", combined, fixed = TRUE))
  expect_true(grepl("1/2 requested output(s) produced", combined, fixed = TRUE))
})

test_that("run_lineage_and_graph: errors when no outputs resolve", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Create dir but no manifests
  procedure_root <- file.path(tmp, "procedures", "migration-toy")
  group_dir <- file.path(procedure_root, "migration-data", "grp", "lineage")
  dir.create(file.path(group_dir, "out_ghost"), recursive = TRUE)

  stage2_called <- FALSE

  local_mocked_bindings(
    run_multiple = function(sas_dir, outputs, output_dirs) 1L,
    run_operations_graph = function(...) {
      stage2_called <<- TRUE
      0L
    }
  )

  output <- capture.output({
    rc <- run_lineage_and_graph(
      procedure = "toy",
      path_to_sas_entrypoint = "main.sas",
      group = "grp",
      outputs = c("out_ghost"),
      project_root = tmp
    )
  })

  expect_equal(rc, 1L)
  expect_false(stage2_called)

  combined <- paste(output, collapse = "\n")
  expect_true(grepl("CASCADE FAILED", combined, fixed = TRUE))
  expect_true(grepl("out_ghost", combined, fixed = TRUE))
})

# ===========================================================================
# TestMain: directory structure verification
# ===========================================================================
test_that("run_lineage_and_graph: per-output dirs follow expected structure", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  captured_output_dirs <- NULL

  local_mocked_bindings(
    run_multiple = function(sas_dir, outputs, output_dirs) {
      captured_output_dirs <<- output_dirs
      for (i in seq_along(outputs)) {
        dir.create(output_dirs[i], recursive = TRUE, showWarnings = FALSE)
        writeLines("{}", file.path(output_dirs[i], "lineage-manifest.json"))
      }
      0L
    },
    run_operations_graph = function(...) 0L
  )

  run_lineage_and_graph(
    procedure = "my-proc",
    path_to_sas_entrypoint = "main.sas",
    group = "grp",
    outputs = c("out1"),
    project_root = tmp
  )

  # Verify the path ends with the expected structure
  expect_true(grepl(
    file.path("migration-my-proc", "migration-data", "grp", "lineage", "out1"),
    captured_output_dirs[1], fixed = TRUE
  ))
})

test_that("run_lineage_and_graph: resolves project_root from package when NULL", {
  resolved_root <- NULL

  local_mocked_bindings(
    run_multiple = function(sas_dir, outputs, output_dirs) {
      resolved_root <<- sub(
        file.path("", "procedures", "migration-my-proc", "sas") , "",
        sas_dir, fixed = TRUE
      )
      1L
    },
    run_operations_graph = function(...) 0L
  )

  rc <- run_lineage_and_graph(
    procedure = "my-proc",
    path_to_sas_entrypoint = "main.sas",
    group = "grp",
    outputs = c("out1"),
    project_root = NULL
  )

  expect_equal(rc, 1L)
  expect_true(nzchar(resolved_root))
})

test_that("run_lineage_and_graph: sas_dir and project_root are forwarded correctly", {
  tmp <- tempfile("lag_test_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  tl_sas_dir <- NULL
  gr_project_root <- NULL

  local_mocked_bindings(
    run_multiple = function(sas_dir, outputs, output_dirs) {
      tl_sas_dir <<- sas_dir
      for (i in seq_along(outputs)) {
        dir.create(output_dirs[i], recursive = TRUE, showWarnings = FALSE)
        writeLines("{}", file.path(output_dirs[i], "lineage-manifest.json"))
      }
      0L
    },
    run_operations_graph = function(...) {
      gr_project_root <<- list(...)$project_root
      0L
    }
  )

  run_lineage_and_graph(
    procedure = "my-proc",
    path_to_sas_entrypoint = "main.sas",
    group = "grp",
    outputs = c("out1"),
    project_root = tmp
  )

  expected_sas_dir <- file.path(tmp, "procedures", "migration-my-proc", "sas")
  expect_equal(tl_sas_dir, expected_sas_dir)
  expect_equal(gr_project_root, tmp)
})
