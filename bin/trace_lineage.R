#!/usr/bin/env Rscript
# trace_lineage.R — CLI entrypoint for SAS lineage analysis (R equivalent of trace_lineage.py)
#
# Usage:
#   Rscript bin/trace_lineage.R <sas_dir> <output_dir> <output1> [<output2> ...]
#
# Example:
#   Rscript bin/trace_lineage.R procedures/migration-had/sas lineage compta_exploit2

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3L || any(args %in% c("-h", "--help"))) {
  cat("
trace_lineage.R — SAS Data Lineage Analyzer

Parses all SAS source files in a directory, traces the full dependency chain
for one or more target output datasets, and generates a Markdown report and
a JSON manifest for each target.

USAGE
  Rscript bin/trace_lineage.R <sas_dir> <output_dir> <output_dataset> [<output_dataset> ...]

ARGUMENTS
  sas_dir
      Path to the directory containing SAS source files (.sas).
      All .sas files in this directory (and subdirectories) are parsed.
      Cross-procedure %include targets are also resolved automatically
      when sibling migration-* directories exist under the parent
      'procedures/' folder.

  output_dir
      Path to the base output directory. For each target dataset, a
      subdirectory named after the dataset is created under this path,
      containing:
        <output_dir>/<dataset>/lineage-report.md
        <output_dir>/<dataset>/lineage-manifest.json

  output_dataset
      One or more target dataset names to trace (case-insensitive).
      Each name should match a SAS dataset produced by the code
      (e.g. 'compta_exploit2', 'work.my_table'). The analyzer walks
      the dependency graph backwards from each target, collecting every
      intermediate dataset and input file that contributes to it.

EXAMPLES
  # Trace a single output:
  Rscript bin/trace_lineage.R procedures/migration-had/sas lineage compta_exploit2

  # Trace multiple outputs in one run (SAS files are parsed only once):
  Rscript bin/trace_lineage.R sas/ output/ rsf1_1 rsf1_2

EXIT CODES
  0   All targets traced successfully.
  1   One or more targets produced no lineage (missing dataset).
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
output_dir <- args[2]
outputs    <- args[3:length(args)]

rc <- run_analyzer(sas_dir, output_dir, outputs)
quit(status = rc)
