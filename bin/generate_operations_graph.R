#!/usr/bin/env Rscript
# generate_operations_graph.R — CLI entrypoint for operations graph generation
#
# Usage:
#   Rscript bin/generate_operations_graph.R <procedure> <entrypoint> <group> <output_dataset1> [<output_dataset2> ...] [-f format] [-v]
#
# Example:
#   Rscript bin/generate_operations_graph.R enc-mco sas/mco.enc.enc.2024.sas rsf rsf1_1 rsf1_2 -f llm

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4L) {
  cat("Usage: Rscript bin/generate_operations_graph.R <procedure> <path_to_sas_entrypoint> <group> <output_dataset1> [<output_dataset2> ...] [-f format] [-v]\n",
      file = stderr())
  cat("\n  format:  dot (default), txt, llm\n", file = stderr())
  cat("  -v:      verbose debug output\n", file = stderr())
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

cat(sprintf("Outputs group '%s' with %d output(s): %s\n",
            group, length(outputs), paste(outputs, collapse = ", ")))

rc <- run_operations_graph(
  procedure = procedure,
  path_to_sas_entrypoint = entrypoint,
  group = group,
  outputs = outputs,
  format = format,
  verbose = verbose
)

quit(status = rc)
