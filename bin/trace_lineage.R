#!/usr/bin/env Rscript
# trace_lineage.R — CLI entrypoint for SAS lineage analysis (R equivalent of trace_lineage.py)
#
# Usage:
#   Rscript bin/trace_lineage.R <sas_dir> <output_dir> <output1> [<output2> ...]
#
# Example:
#   Rscript bin/trace_lineage.R procedures/migration-had/sas lineage compta_exploit2

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3L) {
  cat("Usage: Rscript bin/trace_lineage.R <sas_dir> <output_dir> <output_dataset1> [<output_dataset2> ...]\n",
      file = stderr())
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
output_dir <- args[2]
outputs    <- args[3:length(args)]

rc <- run_analyzer(sas_dir, output_dir, outputs)
quit(status = rc)
