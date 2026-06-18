# test-operations_graph_cli.R — Unit tests for the operations graph CLI helpers
#
# Targets run_operations_graph_from_manifests() error and verbose branches and
# the run_operations_graph() project_root = NULL discovery branch.

# ===========================================================================
# Local fixture builders (do not touch helper-utils.R)
# ===========================================================================
.cli_make_dir <- function() {
  dir <- tempfile("cli_test_")
  dir.create(dir, recursive = TRUE)
  dir
}

#' Write a manifest JSON describing one DATA operation that produces `target`.
.cli_write_manifest <- function(path, target, sas_file, line_number = 1L) {
  manifest <- list(
    target_dataset = target,
    operations = list(
      list(
        dataset = target,
        operation_type = "DATA",
        file = sas_file,
        line_number = line_number,
        end_line = line_number,
        input_datasets = list("raw")
      )
    )
  )
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), path)
}

# ===========================================================================
# parse_graph_outputs (covered by lineage tests but pinned here too)
# ===========================================================================
test_that("parse_graph_outputs: empty string errors", {
  expect_error(
    parse_graph_outputs("   ,  , "),
    regexp = "at least one non-empty output"
  )
})

# ===========================================================================
# run_operations_graph_from_manifests: entrypoint not found (lines 37-38)
# ===========================================================================
test_that("run_operations_graph_from_manifests: missing entrypoint returns 1 with error", {
  dir <- .cli_make_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sas_dir <- file.path(dir, "sas")
  dir.create(sas_dir)

  output <- character(0)
  rc <- withr::with_output_sink(
    new = textConnection("output", open = "w", local = TRUE),
    code = run_operations_graph_from_manifests(
      sas_dir = sas_dir,
      entrypoint = file.path(sas_dir, "absent.sas"),
      output_dir = file.path(dir, "out"),
      manifest_paths = character(0)
    )
  )
  expect_equal(rc, 1L)
  expect_true(any(grepl("Entrypoint file not found", output)))
})

# ===========================================================================
# run_operations_graph_from_manifests: entrypoint not under sas_dir (43-46)
# ===========================================================================
test_that("run_operations_graph_from_manifests: entrypoint outside sas_dir returns 1", {
  dir <- .cli_make_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sas_dir <- file.path(dir, "sas")
  other_dir <- file.path(dir, "elsewhere")
  dir.create(sas_dir)
  dir.create(other_dir)
  ep <- file.path(other_dir, "main.sas")
  writeLines("data output; set raw; run;", ep)

  output <- character(0)
  rc <- withr::with_output_sink(
    new = textConnection("output", open = "w", local = TRUE),
    code = run_operations_graph_from_manifests(
      sas_dir = sas_dir,
      entrypoint = ep,
      output_dir = file.path(dir, "out"),
      manifest_paths = character(0)
    )
  )
  expect_equal(rc, 1L)
  expect_true(any(grepl("is not under", output)))
})

# ===========================================================================
# run_operations_graph_from_manifests: success + isolated-node verbose (145-146)
# ===========================================================================
test_that("run_operations_graph_from_manifests: verbose lists isolated nodes (txt format)", {
  dir <- .cli_make_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sas_dir <- file.path(dir, "sas")
  dir.create(sas_dir)
  ep <- file.path(sas_dir, "main.sas")
  writeLines("data output; set raw; run;", ep)
  mp <- file.path(dir, "manifest.json")
  .cli_write_manifest(mp, target = "output", sas_file = "main.sas", line_number = 1L)
  out_dir <- file.path(dir, "out")

  output <- character(0)
  rc <- withr::with_output_sink(
    new = textConnection("output", open = "w", local = TRUE),
    code = run_operations_graph_from_manifests(
      sas_dir = sas_dir,
      entrypoint = ep,
      output_dir = out_dir,
      manifest_paths = mp,
      format = "txt",
      verbose = TRUE
    )
  )
  expect_equal(rc, 0L)
  expect_true(file.exists(file.path(out_dir, "lineage-graph.txt")))
  joined <- paste(output, collapse = "\n")
  expect_true(grepl("isolated nodes", joined))
})

# ===========================================================================
# run_operations_graph_from_manifests: dot format render hint + llm format
# ===========================================================================
test_that("run_operations_graph_from_manifests: dot format prints render hint", {
  dir <- .cli_make_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sas_dir <- file.path(dir, "sas")
  dir.create(sas_dir)
  ep <- file.path(sas_dir, "main.sas")
  writeLines("data output; set raw; run;", ep)
  mp <- file.path(dir, "manifest.json")
  .cli_write_manifest(mp, target = "output", sas_file = "main.sas", line_number = 1L)
  out_dir <- file.path(dir, "out")

  output <- character(0)
  rc <- withr::with_output_sink(
    new = textConnection("output", open = "w", local = TRUE),
    code = run_operations_graph_from_manifests(
      sas_dir = sas_dir,
      entrypoint = ep,
      output_dir = out_dir,
      manifest_paths = mp,
      format = "dot"
    )
  )
  expect_equal(rc, 0L)
  expect_true(file.exists(file.path(out_dir, "lineage-graph.dot")))
  expect_true(any(grepl("To render: dot", output)))
})

test_that("run_operations_graph_from_manifests: llm format writes four artifacts", {
  dir <- .cli_make_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sas_dir <- file.path(dir, "sas")
  dir.create(sas_dir)
  ep <- file.path(sas_dir, "main.sas")
  writeLines("data output; set raw; run;", ep)
  mp <- file.path(dir, "manifest.json")
  .cli_write_manifest(mp, target = "output", sas_file = "main.sas", line_number = 1L)
  out_dir <- file.path(dir, "out")

  rc <- withr::with_output_sink(
    new = tempfile(),
    code = run_operations_graph_from_manifests(
      sas_dir = sas_dir,
      entrypoint = ep,
      output_dir = out_dir,
      manifest_paths = mp,
      format = "llm"
    )
  )
  expect_equal(rc, 0L)
  expect_true(file.exists(file.path(out_dir, "lineage-graph.md")))
  expect_true(file.exists(file.path(out_dir, "lineage-code-extracts.md")))
  expect_true(file.exists(file.path(out_dir, "lineage-spec-index.md")))
  expect_true(file.exists(file.path(out_dir, "lineage-spec-index.json")))
})

# ===========================================================================
# run_operations_graph_from_manifests: target not reached returns 1
# ===========================================================================
test_that("run_operations_graph_from_manifests: unreachable target returns 1", {
  dir <- .cli_make_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sas_dir <- file.path(dir, "sas")
  dir.create(sas_dir)
  ep <- file.path(sas_dir, "main.sas")
  writeLines("data something_else; set raw; run;", ep)
  mp <- file.path(dir, "manifest.json")
  # Manifest points at a line that does not produce the target during the walk.
  .cli_write_manifest(mp, target = "never_reached", sas_file = "other.sas", line_number = 99L)
  out_dir <- file.path(dir, "out")

  output <- character(0)
  rc <- withr::with_output_sink(
    new = textConnection("output", open = "w", local = TRUE),
    code = run_operations_graph_from_manifests(
      sas_dir = sas_dir,
      entrypoint = ep,
      output_dir = out_dir,
      manifest_paths = mp,
      format = "txt"
    )
  )
  expect_equal(rc, 1L)
  expect_true(any(grepl("not found during code walk", output)))
})

# ===========================================================================
# run_operations_graph: project_root = NULL discovery branch (170-173)
# ===========================================================================
test_that("run_operations_graph: missing manifest with default project_root returns 1", {
  # project_root = NULL exercises the system.file()/getwd() discovery branch.
  # No procedures dir is present so the manifest lookup fails and returns 1.
  output <- character(0)
  rc <- withr::with_output_sink(
    new = textConnection("output", open = "w", local = TRUE),
    code = run_operations_graph(
      procedure = "does-not-exist",
      path_to_sas_entrypoint = "main.sas",
      group = "grp",
      outputs = c("out1")
    )
  )
  expect_equal(rc, 1L)
  expect_true(any(grepl("Manifest file not found", output)))
})

test_that("run_operations_graph: explicit project_root resolves manifest path", {
  dir <- .cli_make_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # Missing manifest under explicit root still returns 1, but exercises the
  # path-assembly branch with project_root supplied.
  output <- character(0)
  rc <- withr::with_output_sink(
    new = textConnection("output", open = "w", local = TRUE),
    code = run_operations_graph(
      procedure = "p",
      path_to_sas_entrypoint = "main.sas",
      group = "grp",
      outputs = c("out1"),
      project_root = dir
    )
  )
  expect_equal(rc, 1L)
  expect_true(any(grepl("Manifest file not found", output)))
})
