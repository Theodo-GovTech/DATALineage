# lineage_and_graph.R — cascade orchestration (trace lineage, then build graph)

#' Run the lineage-and-graph cascade for an outputs group
#'
#' Runs both stages in sequence for a procedure: first trace lineage for each
#' requested output (producing a per-output manifest), then build the combined
#' operations graph. If any requested output fails to produce a manifest, the
#' cascade fails loudly and the graph stage is skipped.
#'
#' @param procedure Character procedure name (the `migration-<procedure>`
#'   directory under `procedures/`).
#' @param path_to_sas_entrypoint Character path to the SAS entrypoint file.
#' @param group Character outputs group name.
#' @param outputs Character vector of target output dataset names.
#' @param format Character output format passed to the graph stage: `"dot"`,
#'   `"txt"`, or `"llm"`.
#' @param verbose Logical enable verbose debug output in the graph stage.
#' @param project_root Character path to the migration-factory root. When
#'   `NULL`, it is resolved from the installed package location.
#' @return Integer `0` on success, non-zero when a stage fails or when one or
#'   more requested outputs produced no lineage manifest.
#' @export
run_lineage_and_graph <- function(procedure, path_to_sas_entrypoint,
                                   group, outputs,
                                   format = "dot", verbose = FALSE,
                                   project_root = NULL) {
  if (is.null(project_root)) {
    project_root <- tryCatch({
      pkg_dir <- system.file(package = "saslineager")
      if (nzchar(pkg_dir)) {
        dirname(dirname(pkg_dir))
      } else {
        getwd()
      }
    }, error = function(e) {
      getwd()
    })
  }

  procedure_root <- file.path(
    project_root, "procedures",
    paste0("migration-", procedure)
  )
  sas_dir <- file.path(procedure_root, "sas")
  group_dir <- file.path(procedure_root, "migration-data", group, "lineage")

  output_dirs <- file.path(group_dir, outputs)

  run_multiple(
    sas_dir = sas_dir,
    outputs = outputs,
    output_dirs = output_dirs
  )

  manifest_exists <- vapply(
    output_dirs,
    function(dir) {
      file.exists(file.path(dir, "lineage-manifest.json"))
    },
    logical(1)
  )

  resolved <- outputs[manifest_exists]
  missing <- outputs[!manifest_exists]

  if (length(missing) > 0L) {
    cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
    cat("CASCADE FAILED\n")
    cat(sprintf(
      "%d/%d requested output(s) produced a lineage manifest.\n",
      length(resolved), length(outputs)
    ))
    cat(sprintf(
      "Outputs without lineage: %s\n",
      paste(missing, collapse = ", ")
    ))
    cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
    return(1L)
  }

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
