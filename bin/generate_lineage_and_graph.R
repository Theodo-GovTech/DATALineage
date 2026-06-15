#!/usr/bin/env Rscript
# generate_lineage_and_graph.R — cascade: trace_lineage then operations graph
#
# Usage:
#   Rscript bin/generate_lineage_and_graph.R <procedure> <entrypoint> <group> [-f format] [-v] <output_dataset1> [<output_dataset2> ...]
#
# Example:
#   Rscript bin/generate_lineage_and_graph.R enc-mco sas/mco.enc.enc.2024.sas rsf -f llm rsf1_1 rsf1_2

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4L) {
  cat("Usage: Rscript bin/generate_lineage_and_graph.R <procedure> <entrypoint> <group> [-f format] [-v] <output_dataset1> [<output_dataset2> ...]\n",
      file = stderr())
  cat("\n  Runs trace_lineage (Stage 1) then generate_operations_graph (Stage 2) in cascade.\n",
      file = stderr())
  cat("\nArguments:\n", file = stderr())
  cat("  procedure    Procedure name. Maps to procedures/migration-<procedure>/sas/\n", file = stderr())
  cat("               for SAS sources and procedures/migration-<procedure>/migration-data/\n", file = stderr())
  cat("               for outputs.\n", file = stderr())
  cat("  group        Outputs group name. Outputs are written under\n", file = stderr())
  cat("               migration-data/<group>/lineage/.\n", file = stderr())
  cat("  entrypoint   Path to the SAS entrypoint file (absolute or relative to CWD).\n", file = stderr())
  cat("               Must be a file inside the procedure's sas/ directory.\n", file = stderr())
  cat("\nOptions:\n", file = stderr())
  cat("  -f, --format FORMAT   Output format: dot (default), txt, llm\n", file = stderr())
  cat("  -v, --verbose         Enable verbose debug output\n", file = stderr())
  cat("\nExample:\n", file = stderr())
  cat("  Rscript bin/generate_lineage_and_graph.R enc-mco \\\n", file = stderr())
  cat("    ../procedures/migration-enc-mco/sas/mco.enc.enc.2024.sas \\\n", file = stderr())
  cat("    compta -f dot compta_exploit2\n", file = stderr())
  quit(status = 2L)
}

# Load the package
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(
    file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "."),
    quiet = TRUE
  )
} else {
  # Fallback: source all R files directly
  pkg_root <- dirname(dirname(normalizePath(commandArgs()[grep("--file=", commandArgs())])))
  if (!nzchar(pkg_root)) pkg_root <- getwd()
  r_dir <- file.path(pkg_root, "R")
  for (f in sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))) {
    source(f, local = FALSE)
  }
}

procedure  <- args[1]
entrypoint <- args[2]
group      <- args[3]

# Parse optional flags and collect output datasets from remaining positional args
format  <- "dot"
verbose <- FALSE
outputs <- character(0)
i <- 4L
while (i <= length(args)) {
  if (args[i] %in% c("-f", "--format") && i < length(args)) {
    format <- args[i + 1L]
    i <- i + 2L
  } else if (args[i] %in% c("-v", "--verbose")) {
    verbose <- TRUE
    i <- i + 1L
  } else {
    outputs <- c(outputs, args[i])
    i <- i + 1L
  }
}

if (length(outputs) == 0L) {
  cat("Error: at least one output dataset is required.\n", file = stderr())
  quit(status = 2L)
}

rc <- run_lineage_and_graph(
  procedure = procedure,
  path_to_sas_entrypoint = entrypoint,
  group = group,
  outputs = outputs,
  format = format,
  verbose = verbose
)

quit(status = rc)
