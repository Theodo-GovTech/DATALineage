# DATALineage

R package for SAS lineage analysis. It traces dataset lineage across source files, produces JSON manifests and Markdown reports of the dependency graph, and generates operations graphs that show how data flows through a SAS pipeline.

Given a target SAS dataset, the **lineage analyzer** works backwards through the code to identify all input datasets, the operations that create or modify them (DATA steps, PROC SQL, PROC SORT, PROC EXPORT, etc.), and the full dependency tree.

The **operations graph generator** takes lineage manifests and walks the SAS code forward, building a directed graph of operations with edges representing dataflow. It produces DOT, plain text, and LLM-optimized output formats for downstream migration tooling.

The **cascade script** (`generate_lineage_and_graph`) runs both stages in sequence: trace lineage for each output, then build the combined operations graph. If any output fails to produce a manifest, the cascade fails loudly without building a partial graph.

## Installation

```r
# From source
devtools::install("path/to/sas-lineage-analyzer-R")

# Or load in development
devtools::load_all()
```

Requires R >= 4.1.0.

### Dependencies

R6, jsonlite, stringr, fs, cli, rlang

## Usage

### CLI -- Lineage Analyzer

```bash
Rscript bin/trace_lineage.R <sas_dir> <output_dir> <output1> [<output2> ...]
```

```bash
# Single output
bin/trace_lineage.R path/to/sas output/ compta_exploit2

# Multiple outputs (parsed once, traced separately)
bin/trace_lineage.R path/to/sas output/ output1 output2 output3
```

### CLI -- Operations Graph

```bash
bin/generate_operations_graph.R <sas_dir> <entrypoint> <output_dir> <manifest1> [<manifest2> ...] [-f format] [-v]
```

```bash
# DOT format (default)
bin/generate_operations_graph.R path/to/sas path/to/sas/main.sas output/graph/ output/rsf1_1/lineage-manifest.json

# Multiple manifests, LLM bundle
bin/generate_operations_graph.R path/to/sas path/to/sas/main.sas output/graph/ \
  output/rsf1_1/lineage-manifest.json output/rsf1_2/lineage-manifest.json -f llm

# Verbose debug output
bin/generate_operations_graph.R path/to/sas path/to/sas/main.sas output/graph/ \
  output/rsf1_1/lineage-manifest.json -f dot -v
```

## Tests

```r
devtools::test()
```

Tests cover the lineage analyzer, operations graph generator, and cascade orchestration (611 tests total).

## License

MIT
