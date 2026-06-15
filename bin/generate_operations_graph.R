#!/usr/bin/env Rscript
# generate_operations_graph.R — CLI entrypoint for operations graph generation
#
# Usage:
#   Rscript bin/generate_operations_graph.R <sas_dir> <entrypoint> <output_dir> <manifest1> [<manifest2> ...] [-f format] [-v]
#
# Example:
#   Rscript bin/generate_operations_graph.R sas/ sas/mco.enc.enc.2024.sas output/rsf/ output/rsf/rsf1_1/lineage-manifest.json -f llm

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4L || any(args %in% c("-h", "--help"))) {
  cat("
generate_operations_graph.R — SAS Operations Graph Generator

Builds an operations graph from one or more lineage manifests (produced by
trace_lineage.R). The graph is constructed by walking SAS source code in
execution order — following %include directives and macro expansion — starting
from a given entrypoint file, then pruning to the subgraph of ancestors that
feed the target datasets.

USAGE
  Rscript bin/generate_operations_graph.R <sas_dir> <entrypoint> <output_dir> \\
      <manifest> [<manifest> ...] [OPTIONS]

ARGUMENTS
  sas_dir
      Path to the directory containing SAS source files (.sas).
      All .sas files (recursively) are scanned for FILENAME statements,
      macro definitions, and %include resolution. Cross-procedure
      include targets are also resolved when sibling migration-*
      directories exist under the parent 'procedures/' folder.

  entrypoint
      Path to the SAS file that serves as the execution starting point
      (absolute or relative). The code walker begins here and follows
      %include directives and macro calls in execution order. This must
      be the top-level SAS program that ultimately produces the target
      datasets.

  output_dir
      Path to the directory where output files are written. Created
      automatically if it does not exist. The files produced depend on
      the chosen format (see -f/--format below).

  manifest
      One or more paths to lineage-manifest.json files (produced by
      trace_lineage.R). Each manifest declares a target dataset and
      the set of operations (with file/line locations) that build it.
      Multiple manifests can be provided to generate a combined graph
      covering several target datasets.

OPTIONS
  -f, --format FORMAT
      Output format. One of:
        dot   (default) Graphviz DOT file (lineage-graph.dot).
              Render with: dot -Tpng lineage-graph.dot -o lineage-graph.png
        txt   Plain-text node/edge list (lineage-graph.txt).
        llm   LLM-optimized bundle. Produces four files:
                lineage-graph.md          — dependency graph + infile mapping
                lineage-code-extracts.md  — source code for every node
                lineage-spec-index.md     — bucket layout for spec generation
                lineage-spec-index.json   — machine-readable spec index

  -v, --verbose
      Enable verbose debug output. Prints detailed information about
      macro resolution, include following, operation matching, and
      edge creation. Also lists isolated nodes (up to 10) in the
      final summary.

EXAMPLES
  # Generate a DOT graph from a single manifest:
  Rscript bin/generate_operations_graph.R sas/ sas/main.sas output/ \\
      output/rsf1_1/lineage-manifest.json

  # Generate the LLM bundle from multiple manifests:
  Rscript bin/generate_operations_graph.R sas/ sas/mco.enc.enc.2024.sas output/rsf/ \\
      output/rsf/rsf1_1/lineage-manifest.json \\
      output/rsf/rsf1_2/lineage-manifest.json \\
      -f llm

  # Verbose mode for debugging:
  Rscript bin/generate_operations_graph.R sas/ sas/main.sas output/ \\
      output/dataset/lineage-manifest.json -v

EXIT CODES
  0   Graph generated successfully.
  1   Runtime error (missing manifest, missing entrypoint, target dataset
      not reached by the entrypoint).
  2   Invalid arguments.
", file = stderr())
  quit(status = 2L)
}

# Resolve package root from the --file= argument passed by Rscript
pkg_root <- {
  file_arg <- grep("--file=", commandArgs(), value = TRUE)
  if (length(file_arg) > 0L) {
    dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1L]))))
  } else {
    getwd()
  }
}

# Load the package
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg_root, quiet = TRUE)
} else {
  # Fallback: source all R files directly
  r_dir <- file.path(pkg_root, "R")
  for (f in sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))) {
    source(f, local = FALSE)
  }
}

sas_dir    <- args[1]
entrypoint <- args[2]
output_dir <- args[3]

# Parse optional flags and collect manifest paths from remaining positional args
format  <- "dot"
verbose <- FALSE
manifest_paths <- character(0)
i <- 4L
while (i <= length(args)) {
  if (args[i] %in% c("-f", "--format") && i < length(args)) {
    format <- args[i + 1L]
    i <- i + 2L
  } else if (args[i] %in% c("-v", "--verbose")) {
    verbose <- TRUE
    i <- i + 1L
  } else {
    manifest_paths <- c(manifest_paths, args[i])
    i <- i + 1L
  }
}

if (length(manifest_paths) == 0L) {
  cat("Error: at least one manifest path is required.\n", file = stderr())
  quit(status = 2L)
}

# Validate manifest files exist
for (mp in manifest_paths) {
  if (!file.exists(mp)) {
    cat(sprintf("Error: Manifest file not found: %s\n", mp), file = stderr())
    quit(status = 1L)
  }
}

cat(sprintf("Manifests: %d file(s)\n", length(manifest_paths)))
for (mp in manifest_paths) {
  cat(sprintf("  - %s\n", mp))
}

rc <- run_operations_graph_from_manifests(
  sas_dir = sas_dir,
  entrypoint = entrypoint,
  output_dir = output_dir,
  manifest_paths = manifest_paths,
  format = format,
  verbose = verbose
)

quit(status = rc)
