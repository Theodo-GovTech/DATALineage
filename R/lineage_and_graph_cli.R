# lineage_and_graph_cli.R — cascade: trace_lineage then operations graph
#
# Ported from: generate_lineage_and_graph.py

#' Run the two-stage lineage-and-graph cascade
#'
#' Stage 1 runs \code{run_multiple()} to trace lineage for each output,
#' producing per-output manifests and reports. Stage 2 runs
#' \code{run_operations_graph()} to build a combined operations graph from
#' the manifests.
#'
#' If Stage 1 fails to produce a manifest for ANY requested output, the
#' cascade fails loudly and does NOT build a partial graph.
#'
#' @param procedure Character procedure name
#' @param path_to_sas_entrypoint Character path to the SAS entrypoint file
#' @param group Character outputs group name
#' @param outputs Character vector of target output dataset names
#' @param format Character output format: "dot", "txt", or "llm"
#' @param verbose Logical enable verbose debug output
#' @param project_root Character path to the migration-factory root (auto-detected if NULL)
#' @return Integer 0 on success, non-zero on error
#' @export
run_lineage_and_graph <- function(procedure, path_to_sas_entrypoint,
                                   group, outputs,
                                   format = "dot", verbose = FALSE,
                                   project_root = NULL) {
  if (is.null(project_root)) {
    project_root <- tryCatch({
      pkg_dir <- system.file(package = "saslineager")
      if (nzchar(pkg_dir)) dirname(dirname(pkg_dir)) else getwd()
    }, error = function(e) getwd())
  }

  procedure_root <- file.path(project_root, "procedures",
                               paste0("migration-", procedure))
  sas_dir <- file.path(procedure_root, "sas")
  group_dir <- file.path(procedure_root, "migration-data", group, "lineage")
  per_output_dirs <- file.path(group_dir, outputs)

  cat(sprintf("Outputs group '%s' with %d output(s): %s\n",
              group, length(outputs), paste(outputs, collapse = ", ")))

  # ------------------------------------------------------------------
  # Stage 1: trace_lineage
  # ------------------------------------------------------------------
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("Stage 1/2: trace_lineage\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")

  rc <- run_multiple(sas_dir, outputs, per_output_dirs)

  # A missing output must never be silently swallowed into a passing run.
  # If Stage 1 failed to produce a lineage for ANY requested output, fail
  # loudly and report every failed output.
  failed_outputs <- character(0)
  for (i in seq_along(outputs)) {
    manifest_path <- file.path(per_output_dirs[i], "lineage-manifest.json")
    if (!file.exists(manifest_path)) {
      failed_outputs <- c(failed_outputs, outputs[i])
    }
  }

  if (rc != 0L || length(failed_outputs) > 0L) {
    cat("\n", paste(rep("#", 80), collapse = ""), "\n", sep = "")
    cat("# CASCADE FAILED \u2014 outputs with no lineage\n")
    cat(paste(rep("#", 80), collapse = ""), "\n")
    cat(sprintf("# %d/%d requested output(s) produced no lineage and were NOT written to the graph:\n",
                length(failed_outputs), length(outputs)))
    for (output in failed_outputs) {
      cat(sprintf("#   - %s\n", output))
    }
    cat("#\n")
    cat("# Each failed output is one of:\n")
    cat("#   (a) a LIVE producer the analyzer failed to parse -> fix the analyzer; or\n")
    cat("#   (b) a DEAD output with no producer in the SAS source -> remove it from\n")
    cat("#       the outputs list (the SAS code never writes it).\n")
    cat("# The graph was NOT built. Resolve every failed output above, then re-run.\n")
    cat(paste(rep("#", 80), collapse = ""), "\n")
    return(if (rc != 0L) rc else 1L)
  }

  # ------------------------------------------------------------------
  # Stage 2: generate_operations_graph
  # ------------------------------------------------------------------
  cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
  cat("Stage 2/2: generate_operations_graph\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")

  run_operations_graph(
    procedure = procedure,
    path_to_sas_entrypoint = path_to_sas_entrypoint,
    group = group,
    outputs = outputs,
    format = format,
    verbose = verbose,
    project_root = project_root
  )
}
