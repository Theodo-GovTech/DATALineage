# operations_graph_cli.R — CLI helpers for the operations graph generator
#
# Ported from: generate_operations_graph.py run() / _parse_outputs() / main()

#' Parse a comma-separated OUTPUTS token into a character vector of output names
#'
#' @param raw Character string with comma-separated output names
#' @return Character vector of trimmed, non-empty output names
#' @export
parse_graph_outputs <- function(raw) {
  tokens <- strsplit(raw, ",", fixed = TRUE)[[1]]
  outputs <- trimws(tokens)
  outputs <- outputs[nzchar(outputs)]
  if (length(outputs) == 0L) {
    stop("OUTPUTS must contain at least one non-empty output name (comma-separated).",
         call. = FALSE)
  }
  outputs
}

#' Run the operations-graph generator from explicit paths
#'
#' @param sas_dir Character path to the SAS source directory
#' @param entrypoint Character path to the SAS entrypoint file (absolute or relative)
#' @param output_dir Character path to the output directory
#' @param manifest_paths Character vector of paths to lineage manifest JSON files
#' @param format Character output format: "dot", "txt", or "llm"
#' @param verbose Logical enable verbose debug output
#' @return Integer 0 on success, 1 on error
#' @export
run_operations_graph_from_manifests <- function(sas_dir, entrypoint,
                                                output_dir, manifest_paths,
                                                format = "dot", verbose = FALSE) {
  # Resolve entrypoint relative to sas_dir
  entrypoint_file <- normalizePath(entrypoint, mustWork = FALSE)
  if (!file.exists(entrypoint_file)) {
    cat(sprintf("Error: Entrypoint file not found: %s\n", entrypoint_file))
    return(1L)
  }

  sas_dir_resolved <- normalizePath(sas_dir, mustWork = FALSE)
  if (!startsWith(entrypoint_file, paste0(sas_dir_resolved, "/"))) {
    if (!startsWith(entrypoint_file, sas_dir_resolved)) {
      cat(sprintf("Error: Entrypoint %s is not under %s\n",
                  entrypoint_file, sas_dir_resolved))
      return(1L)
    }
  }
  entrypoint_rel <- substring(entrypoint_file, nchar(sas_dir_resolved) + 2L)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Build generator
  generator <- OperationsGraphGenerator$new(
    sas_dir = sas_dir,
    entrypoint = entrypoint_rel,
    manifest_paths = manifest_paths
  )
  generator$verbose <- verbose

  cat(sprintf("Loading %d manifest(s):\n", length(manifest_paths)))
  for (mp in manifest_paths) {
    cat(sprintf("  - %s\n", mp))
  }
  generator$load_manifests()

  n_ops <- length(as.list(generator$operation_lookup))
  total_ops <- sum(vapply(as.list(generator$operation_lookup),
                          length, integer(1)))
  cat(sprintf("  Operations in manifest: %d\n", total_ops))

  cat("Building filename alias map...\n")
  generator$build_filename_alias_map()
  cat(sprintf("  Filename aliases: %d\n", length(generator$filename_aliases)))

  cat("Building macro definition map...\n")
  generator$build_macro_map()
  total_macros <- sum(vapply(generator$macro_definitions, length, integer(1)))
  cat(sprintf("  Macro definitions: %d (%d unique names)\n",
              total_macros, length(generator$macro_definitions)))

  cat(sprintf("Walking code from: %s\n", entrypoint_rel))
  generator$walk_code()
  cat(sprintf("  Operations found: %d\n", length(generator$graph_nodes)))
  cat(sprintf("  Edges created: %d\n", length(generator$graph_edges)))

  # Validate targets
  found_targets <- unique(vapply(generator$graph_nodes,
                                  function(n) n$dataset, character(1)))
  found_targets <- intersect(found_targets, generator$target_datasets)
  missing_targets <- setdiff(generator$target_datasets, found_targets)
  if (length(missing_targets) > 0L) {
    cat(sprintf("\nError: target dataset(s) not found during code walk: %s\n",
                paste(sort(missing_targets), collapse = ", ")))
    cat(sprintf("  The entrypoint '%s' does not reach the SAS file(s) that produce these outputs.\n",
                entrypoint_rel))
    cat("  Use the SAS file that directly creates the target dataset(s) as the entrypoint.\n")
    return(1L)
  }

  # Generate output
  output_paths <- character(0)
  if (format == "llm") {
    artifacts <- list(
      "lineage-graph.md" = generator$generate_graph_md(),
      "lineage-code-extracts.md" = generator$generate_code_extracts_md(),
      "lineage-spec-index.md" = generator$generate_spec_index_md(),
      "lineage-spec-index.json" = generator$generate_spec_index_json()
    )
    for (filename in names(artifacts)) {
      path <- file.path(output_dir, filename)
      writeLines(artifacts[[filename]], path, useBytes = TRUE)
      output_paths <- c(output_paths, path)
    }
  } else {
    ext <- if (format == "dot") "dot" else "txt"
    content <- if (format == "dot") generator$generate_dot() else generator$generate_txt()
    single_path <- file.path(output_dir, paste0("lineage-graph.", ext))
    writeLines(content, single_path, useBytes = TRUE)
    output_paths <- c(output_paths, single_path)
  }

  # Stats
  unique_nodes <- generator$.__enclos_env__$private$iter_unique_nodes()
  unique_node_ids <- vapply(unique_nodes, function(x) x$node_id, character(1))
  unique_edges <- generator$.__enclos_env__$private$iter_unique_edges()

  for (p in output_paths) {
    cat(sprintf("\nGraph generated: %s\n", p))
  }
  cat(sprintf("  Unique nodes: %d\n", length(unique_node_ids)))
  cat(sprintf("  Edges: %d\n", length(unique_edges)))

  if (format == "dot") {
    cat(sprintf("\nTo render: dot -Tpng %s -o %s\n",
                output_paths[1],
                sub("\\.dot$", ".png", output_paths[1])))
  }

  nodes_with_edges <- unique(unlist(unique_edges))
  isolated <- setdiff(unique_node_ids, nodes_with_edges)
  if (length(isolated) > 0L) {
    cat(sprintf("\nWarning: %d isolated nodes (no edges)\n", length(isolated)))
    if (verbose) {
      for (nid in head(isolated, 10L)) {
        cat(sprintf("  - %s\n", nid))
      }
    }
  }

  0L
}

#' Run the operations-graph generator (legacy interface)
#'
#' @param procedure Character procedure name
#' @param path_to_sas_entrypoint Character path to the SAS entrypoint
#' @param group Character outputs group name
#' @param outputs Character vector of target output dataset names
#' @param format Character output format: "dot", "txt", or "llm"
#' @param verbose Logical enable verbose debug output
#' @param project_root Character path to the migration-factory root
#' @return Integer 0 on success, 1 on error
#' @export
run_operations_graph <- function(procedure, path_to_sas_entrypoint,
                                  group, outputs,
                                  format = "dot", verbose = FALSE,
                                  project_root = NULL) {
  if (is.null(project_root)) {
    project_root <- tryCatch({
      pkg_dir <- system.file(package = "DATALineage")
      if (nzchar(pkg_dir)) dirname(dirname(pkg_dir)) else getwd()
    }, error = function(e) getwd())
  }

  procedure_root <- file.path(project_root, "procedures",
                               paste0("migration-", procedure))
  sas_dir <- file.path(procedure_root, "sas")
  output_dir <- file.path(procedure_root, "migration-data", group, "lineage")

  # Resolve manifest paths
  manifest_paths <- character(0)
  for (output in outputs) {
    mp <- file.path(output_dir, output, "lineage-manifest.json")
    if (!file.exists(mp)) {
      cat(sprintf("Error: Manifest file not found: %s\n", mp))
      return(1L)
    }
    manifest_paths <- c(manifest_paths, mp)
  }

  run_operations_graph_from_manifests(
    sas_dir = sas_dir,
    entrypoint = path_to_sas_entrypoint,
    output_dir = output_dir,
    manifest_paths = manifest_paths,
    format = format,
    verbose = verbose
  )
}
