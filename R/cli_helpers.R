# cli_helpers.R — CLI entrypoint helpers (ported from trace_lineage.py CLI)

#' Run the lineage analyzer for an outputs group
#'
#' @param sas_dir Character path to the directory containing SAS source files
#' @param output_dir Character path to the directory where per-output
#'   subdirectories will be created
#' @param outputs Character vector of target output dataset names
#' @return Integer 0 on success, non-zero on error
#' @export
run_analyzer <- function(sas_dir, output_dir, outputs) {
  output_dirs <- file.path(output_dir, outputs)
  run_multiple(sas_dir, outputs, output_dirs)
}

#' Run the lineage analyzer for multiple outputs, parsing once
#'
#' @param sas_dir Character path to the directory containing SAS source files
#' @param outputs Character vector of target output dataset names
#' @param output_dirs Character vector of per-output directories (same length
#'   as \code{outputs})
#' @return Integer 0 on success, non-zero on error
#' @export
run_multiple <- function(sas_dir, outputs, output_dirs) {
  if (length(output_dirs) != length(outputs)) {
    message(sprintf(
      "\nError: output_dirs length (%d) must match outputs length (%d)",
      length(output_dirs), length(outputs)
    ))
    return(1L)
  }

  message(paste(rep("=", 80), collapse = ""))
  message("SAS Data Lineage Analyzer v2 (R)")
  message(paste(rep("=", 80), collapse = ""))
  message(sprintf("\nConfiguration:"))
  message(sprintf("  SAS Directory:  %s", sas_dir))
  message(sprintf("  Outputs:        %s", paste(outputs, collapse = ", ")))

  analyzer <- .parse_sources(sas_dir)
  if (is.null(analyzer)) return(1L)

  failed_outputs <- character(0)
  for (i in seq_along(outputs)) {
    rc <- .process_output(analyzer, outputs[i], output_dirs[i])
    if (rc != 0L) failed_outputs <- c(failed_outputs, outputs[i])
  }

  if (length(failed_outputs) > 0L) {
    message(paste(rep("=", 80), collapse = ""))
    message(sprintf(
      "FAILED: %d/%d output(s) produced no lineage: %s",
      length(failed_outputs), length(outputs), paste(failed_outputs, collapse = ", ")
    ))
    message(paste(rep("=", 80), collapse = ""))
    return(1L)
  }

  message(paste(rep("=", 80), collapse = ""))
  message("Analysis complete!")
  message(paste(rep("=", 80), collapse = ""))
  0L
}

.parse_sources <- function(sas_dir) {
  if (!file.exists(sas_dir)) {
    message(sprintf("\nError: SAS directory not found: %s", sas_dir))
    return(NULL)
  }
  if (!dir.exists(sas_dir)) {
    message(sprintf("\nError: Not a directory: %s", sas_dir))
    return(NULL)
  }

  analyzer <- SASLineageAnalyzer$new(sas_dir)

  message("\nParsing SAS files...")
  analyzer$parse_all_sas_files()
  analyzer$deduplicate_operations()
  message(sprintf("\nFound %d dataset operations", length(analyzer$dataset_operations)))
  message(sprintf("Unique datasets: %d", length(analyzer$dataset_to_ops)))
  message(sprintf("Input files tracked: %d", length(analyzer$infile_usage)))

  analyzer
}

.process_output <- function(analyzer, output, output_dir) {
  target_dataset <- clean_dataset_name(output)
  if (is.null(target_dataset)) target_dataset <- tolower(output)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  report_file <- file.path(output_dir, "lineage-report.md")
  manifest_file <- file.path(output_dir, "lineage-manifest.json")

  message(sprintf("\nTracing dependencies for '%s'...", target_dataset))
  lineage <- analyzer$trace_dependencies(target_dataset)

  if (length(lineage) == 0L) {
    message(sprintf("\nERROR: No dependencies found for dataset '%s'", target_dataset))
    return(1L)
  }

  message(sprintf("Found %d datasets in dependency chain", length(lineage)))

  message("\nGenerating reports...")
  analyzer$generate_report(target_dataset, report_file, filter_orphan_nodes = TRUE)
  analyzer$generate_json_manifest(target_dataset, manifest_file, filter_orphan_nodes = TRUE)

  message(sprintf("\n## Output Files (%s):", output))
  message(sprintf("  - Report:   %s", report_file))
  message(sprintf("  - Manifest: %s", manifest_file))

  0L
}
