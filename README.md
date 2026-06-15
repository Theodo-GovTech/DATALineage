# saslineager

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
Rscript bin/trace_lineage.R <procedure> <group> <output1> [<output2> ...]
```

```bash
# Single output
Rscript bin/trace_lineage.R had compta_group compta_exploit2

# Multiple outputs (parsed once, traced separately)
Rscript bin/trace_lineage.R had compta_group output1 output2 output3
```

### CLI -- Cascade (Lineage + Graph)

```bash
Rscript bin/generate_lineage_and_graph.R <procedure> <entrypoint> <group> <outputs> [-f format] [-v]
```

```bash
# Run both stages: trace lineage then build operations graph
Rscript bin/generate_lineage_and_graph.R enc-mco sas/mco.enc.enc.2024.sas rsf "rsf1_1,rsf1_2" -f llm
```

### CLI -- Operations Graph

```bash
Rscript bin/generate_operations_graph.R <procedure> <entrypoint> <group> <outputs> [-f format] [-v]
```

```bash
# DOT format (default)
Rscript bin/generate_operations_graph.R enc-mco sas/mco.enc.enc.2024.sas rsf "rsf1_1,rsf1_2"

# LLM bundle (markdown graph, code extracts, spec index)
Rscript bin/generate_operations_graph.R enc-mco sas/mco.enc.enc.2024.sas rsf "rsf1_1,rsf1_2" -f llm

# Verbose debug output
Rscript bin/generate_operations_graph.R enc-mco sas/mco.enc.enc.2024.sas rsf "rsf1_1" -f dot -v
```

### Programmatic -- Lineage Analyzer

```r
library(saslineager)

analyzer <- SASLineageAnalyzer$new("/path/to/sas/directory")
analyzer$parse_all_sas_files()
analyzer$deduplicate_operations()

# Trace a target dataset
lineage <- analyzer$trace_dependencies("my_dataset")

# Generate outputs
analyzer$generate_json_manifest("my_dataset", "manifest.json")
analyzer$generate_report("my_dataset", "report.md")
```

### Programmatic -- Operations Graph

```r
library(saslineager)

generator <- OperationsGraphGenerator$new(
  sas_dir = "/path/to/sas",
  entrypoint = "main.sas",
  manifest_paths = c("path/to/lineage-manifest.json")
)

generator$load_manifests()
generator$build_filename_alias_map()
generator$build_macro_map()
generator$walk_code()

# Output formats
dot_output <- generator$generate_dot()       # Graphviz DOT
txt_output <- generator$generate_txt()       # Plain text (token-efficient)
md_output  <- generator$generate_graph_md()  # Markdown graph
```

## Output

### Lineage Analyzer

Two files per target dataset:

- **`lineage-manifest.json`** -- machine-readable dependency graph with operations, inputs, depths, and macro source tracking
- **`lineage-report.md`** -- human-readable report with the dependency tree organized by depth, code snippets, and input file identification

### Operations Graph

In `dot` or `txt` mode, one file:

- **`lineage-graph.dot`** or **`lineage-graph.txt`** -- the operations graph in DOT or plain text format

In `llm` mode, four files:

- **`lineage-graph.md`** -- Markdown representation of the operations graph
- **`lineage-code-extracts.md`** -- code snippets for each operation node
- **`lineage-spec-index.md`** -- spec-build index with bucket layout and node metadata
- **`lineage-spec-index.json`** -- machine-readable spec-build index

## What it parses

| SAS construct | Details |
|---|---|
| DATA steps | Input/output datasets, INFILE, SET, MERGE |
| PROC SQL | CREATE TABLE, INSERT, SELECT with table detection |
| PROC SORT | Data/out datasets |
| PROC EXPORT | Output file and source dataset |
| PROC FORMAT | CNTLIN datasets, format catalogs |
| PROC TRANSPOSE, PROC APPEND | Generic PROC handling |
| ODS tagsets.csv | ODS CSV output capture |
| Macros | %macro/%mend definitions, %let variables, nested expansion (up to 12 levels), parameter substitution |
| %include | Path resolution with macro variable substitution, cross-file following |
| FILENAME | Fileref-to-path mapping |

## Project structure

```
R/
  analyzer.R              # SASLineageAnalyzer R6 class (core)
  base.R                  # Operation factory & utilities
  cli_helpers.R           # CLI entrypoint helpers (trace_lineage)
  comments.R              # Block comment handling
  data_step.R             # DATA step parsing
  filename.R              # FILENAME statement parsing
  includes.R              # %include resolution
  macro.R                 # Macro definition & expansion
  ods.R                   # ODS CSV output parsing
  operations_graph.R      # OperationsGraphGenerator R6 class
  operations_graph_cli.R  # CLI entrypoint helpers (operations graph)
  operations_graph_nodes.R # Graph node data structures & styling
  lineage_and_graph_cli.R # Cascade: trace_lineage + operations graph
  proc_export.R           # PROC EXPORT parsing
  proc_format.R           # PROC FORMAT parsing
  proc_generic.R          # Generic PROC parsing
  proc_sort.R             # PROC SORT parsing
  proc_sql.R              # PROC SQL parsing
bin/
  trace_lineage.R           # CLI entrypoint for lineage tracing
  generate_operations_graph.R   # CLI entrypoint for operations graph
  generate_lineage_and_graph.R  # CLI entrypoint for cascade
inst/
  cross-language-harness.R  # Python/R comparison harness
tests/
  testthat/        # Test modules
```

## Tests

```r
devtools::test()
```

Tests cover the lineage analyzer, operations graph generator, and cascade orchestration (611 tests total).

## License

MIT
