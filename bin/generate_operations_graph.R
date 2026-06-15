#!/usr/bin/env Rscript
# generate_operations_graph.R — CLI entrypoint for operations graph generation
#
# Usage:
#   Rscript bin/generate_operations_graph.R <sas_dir> <entrypoint> <output_dir> <manifest1> [<manifest2> ...] [-f format] [-v]
#
# Example:
#   Rscript bin/generate_operations_graph.R sas/ sas/mco.enc.enc.2024.sas output/rsf/ output/rsf/rsf1_1/lineage-manifest.json -f llm

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4L) {
  cat("Usage: Rscript bin/generate_operations_graph.R <sas_dir> <entrypoint> <output_dir> <manifest1> [<manifest2> ...] [-f format] [-v]\n",
      file = stderr())
  cat("\n  format:  dot (default), txt, llm\n", file = stderr())
  cat("  -v:      verbose debug output\n", file = stderr())
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
