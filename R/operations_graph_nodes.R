# operations_graph_nodes.R — Data structure factories for the operations graph
#
# Ported from: generate_operations_graph.py (CallSite, OperationNode,
# MacroDefinition, CallStackFrame dataclasses)

# ---------------------------------------------------------------------------
# Color scheme for operation types (matches Python OPERATION_COLORS)
# ---------------------------------------------------------------------------
.OG_OPERATION_COLORS <- list(
  "DATA"            = list(fillcolor = "#bbdefb", color = "#1976d2"),
  "DATA STEP"       = list(fillcolor = "#bbdefb", color = "#1976d2"),
  "PROC SQL"        = list(fillcolor = "#e1bee7", color = "#7b1fa2"),
  "PROC SORT"       = list(fillcolor = "#fff9c4", color = "#f9a825"),
  "PROC TRANSPOSE"  = list(fillcolor = "#ffe0b2", color = "#ef6c00"),
  "INFILE"          = list(fillcolor = "#c8e6c9", color = "#388e3c")
)
.OG_DEFAULT_PROC_COLOR <- list(fillcolor = "#f5f5f5", color = "#616161")
.OG_TARGET_COLOR       <- list(fillcolor = "#ffcdd2", color = "#f44336")

# ---------------------------------------------------------------------------
# CallSite key — replaces frozen dataclass used as dict key in Python
# ---------------------------------------------------------------------------

#' Create a string key for a call site (file + line)
#'
#' Used as named-list / environment key instead of a hashable object.
#' @param file Character basename or full path
#' @param line Integer line number
#' @return Character string key
#' @keywords internal
call_site_key <- function(file, line) {
  paste0(file, "|", as.integer(line))
}

# ---------------------------------------------------------------------------
# OperationNode — named list factory
# ---------------------------------------------------------------------------

#' Create a new OperationNode (named list)
#'
#' @param dataset Character output dataset name
#' @param file_path Character full path to SAS file
#' @param line_number Integer line where operation starts
#' @param operation_type Character: DATA, PROC SQL, PROC SORT, INFILE, etc.
#' @param input_datasets Character vector of input dataset names
#' @param depth Integer recursion depth
#' @param resolved_path Optional character: resolved file path for INFILE ops
#' @param end_line Optional integer: end line of the operation
#' @param feeds Character vector of terminal dataset names this node feeds
#' @return Named list representing an operation node
#' @keywords internal
new_operation_node <- function(dataset, file_path, line_number, operation_type,
                               input_datasets, depth, resolved_path = NULL,
                               end_line = NULL, feeds = character(0)) {
  list(
    dataset        = dataset,
    file_path      = file_path,
    line_number    = as.integer(line_number),
    operation_type = operation_type,
    input_datasets = input_datasets,
    depth          = as.integer(depth),
    resolved_path  = resolved_path,
    end_line       = if (!is.null(end_line)) as.integer(end_line) else NULL,
    feeds          = feeds
  )
}

#' Create a node identity key (file_path, line_number, dataset)
#'
#' Mirrors Python OperationNode.__hash__ / __eq__ — identity is determined
#' by (file_path, line_number, dataset) only.
#' @param node Named list (OperationNode)
#' @return Character string identity key
#' @keywords internal
node_identity <- function(node) {
  paste0(node$file_path, "|", node$line_number, "|", node$dataset)
}

# ---------------------------------------------------------------------------
# MacroDefinition — named list factory
# ---------------------------------------------------------------------------

#' Create a new MacroDefinition (named list)
#'
#' @param name Character macro name (lower-cased)
#' @param file_path Character full path to where macro is defined
#' @param start_line Integer line with \%macro (1-based)
#' @param end_line Integer line with \%mend (1-based)
#' @param params Character vector of parameter names
#' @return Named list
#' @keywords internal
new_og_macro_definition <- function(name, file_path, start_line, end_line,
                                    params = character(0)) {
  list(
    name       = name,
    file_path  = file_path,
    start_line = as.integer(start_line),
    end_line   = as.integer(end_line),
    params     = params
  )
}

# ---------------------------------------------------------------------------
# CallStackFrame — named list factory
# ---------------------------------------------------------------------------

#' Create a new CallStackFrame (named list)
#'
#' @param file_path Character full path
#' @param line Integer resume line after include/macro call
#' @param frame_type Character: "include" or "macro"
#' @param macro_name Optional character: macro name if frame_type == "macro"
#' @return Named list
#' @keywords internal
new_call_stack_frame <- function(file_path, line, frame_type,
                                 macro_name = NULL) {
  list(
    file_path  = file_path,
    line       = as.integer(line),
    frame_type = frame_type,
    macro_name = macro_name
  )
}

# ---------------------------------------------------------------------------
# Node ID helpers
# ---------------------------------------------------------------------------

#' Create a valid Graphviz node ID from an operation node
#'
#' Sanitizes the combination of dataset + display_file + line_number.
#' @param op_node Named list (OperationNode)
#' @param display_file Character display path (relative to sas_dir)
#' @return Character valid Graphviz ID
#' @keywords internal
make_node_id <- function(op_node, display_file) {
  raw <- paste0(op_node$dataset, "_", display_file, "_", op_node$line_number)
  sanitized <- gsub("[^a-zA-Z0-9_]", "_", raw)
  if (grepl("^[0-9]", sanitized)) {
    sanitized <- paste0("op_", sanitized)
  }
  sanitized
}

#' Get the DOT style string for an operation node
#'
#' @param op_node Named list (OperationNode)
#' @param target_datasets Character vector of target dataset names
#' @return Character DOT style attribute string
#' @keywords internal
get_operation_style <- function(op_node, target_datasets) {
  if (op_node$dataset %in% target_datasets) {
    colors <- .OG_TARGET_COLOR
  } else if (!is.null(.OG_OPERATION_COLORS[[op_node$operation_type]])) {
    colors <- .OG_OPERATION_COLORS[[op_node$operation_type]]
  } else if (startsWith(op_node$operation_type, "PROC ")) {
    colors <- .OG_DEFAULT_PROC_COLOR
  } else {
    colors <- if (!is.null(.OG_OPERATION_COLORS[["DATA"]])) {
      .OG_OPERATION_COLORS[["DATA"]]
    } else {
      .OG_DEFAULT_PROC_COLOR
    }
  }
  sprintf(', style="filled", fillcolor="%s", color="%s"',
          colors$fillcolor, colors$color)
}

#' Escape a value for txt format (quote if contains spaces or equals)
#'
#' @param value Character value
#' @return Character possibly quoted
#' @keywords internal
escape_txt_value <- function(value) {
  if (grepl(" ", value, fixed = TRUE) || grepl("=", value, fixed = TRUE)) {
    return(paste0('"', value, '"'))
  }
  value
}
