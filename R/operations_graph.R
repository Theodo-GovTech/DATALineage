# operations_graph.R — OperationsGraphGenerator R6 class
#
# Ported from: generate_operations_graph.py
#
# Generates an operations graph from SAS lineage manifests by walking SAS
# source code in execution order (following %include and macro expansion),
# then pruning to the target-ancestor subgraph via backward BFS.

# ---------------------------------------------------------------------------
# Regex constants
# ---------------------------------------------------------------------------
.OG_STATIC_LET_RE     <- "(?i)^\\s*%let\\s+([A-Za-z_]\\w*)\\s*=\\s*([^;]*);"
.OG_SUBSTR_RE         <- "(?i)%substr\\s*\\(\\s*([^,)]+?)\\s*,\\s*(\\d+)\\s*(?:,\\s*(\\d+)\\s*)?\\)"
.OG_FILEREF_TOKEN     <- "[A-Za-z_][\\w&.]*"
.OG_FILENAME_RE       <- "(?i)^\\s*filename\\s+([A-Za-z_][\\w&.]*)\\s+[\"']([^\"']+)[\"']"
.OG_INCLUDE_FILEREF_RE <- "(?i)^\\s*%include\\s+([A-Za-z_][\\w&.]*)\\s*;"
.OG_INCLUDE_QUOTED_RE <- "(?i)^\\s*%include\\s+[\"']([^\"']+)[\"']"
.OG_IF_DO_RE          <- "(?i)^\\s*%if\\b.*?%then\\s+%do\\s*;"
.OG_ELSE_IF_DO_RE     <- "(?i)^\\s*%else\\s+%if\\b.*?%then\\s+%do\\s*;"
.OG_ELSE_DO_RE        <- "(?i)^\\s*%else\\s+%do\\s*;"
.OG_DO_TOKEN_RE       <- "(?i)%do\\b"
.OG_END_TOKEN_RE      <- "(?i)%end\\b"
.OG_CHAIN_SKIP_RE     <- "(?i)^\\s*(?:$|/\\*.*?\\*/\\s*$|\\*[^;]*;\\s*$)"
.OG_MACRO_DEF_RE      <- "(?i)^\\s*%macro\\s+\\w+"
.OG_MACRO_DEF_PARSE_RE <- "(?i)^\\s*%macro\\s+(\\w+)\\s*(?:\\(([^)]*)\\))?\\s*;?"
.OG_INLINE_EMPTY_RE   <- "(?i)%macro\\s+\\w+[^;]*;\\s*%mend"
.OG_MACRO_OPEN_RE     <- "(?i)%macro\\s+\\w+"
.OG_MEND_RE           <- "(?i)%mend"
.OG_MACRO_CALL_RE     <- "(?i)^\\s*%(\\w+)\\s*(?:\\([^)]*\\))?\\s*;?"
.OG_MACRO_VAR_REF_RE  <- "&([A-Za-z_]\\w*)\\.?"

.OG_SKIP_MACROS <- c(
 "if", "do", "else", "end", "let", "put", "include",
 "global", "local", "sysfunc", "eval", "str", "nrstr",
 "then", "goto", "label", "return", "abort", "run"
)

# ---------------------------------------------------------------------------
# Helper: count regex matches in a string
# ---------------------------------------------------------------------------
.count_matches <- function(pattern, text) {
  m <- gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE)[[1]]
  if (m[1] == -1L) 0L else length(m)
}

# ---------------------------------------------------------------------------
# Helper: normalize fileref
# ---------------------------------------------------------------------------
.og_normalize_fileref <- function(fileref) {
  tolower(gsub("&\\w+\\.?", "", fileref))
}

#' Operations Graph Generator R6 Class
#' @export
OperationsGraphGenerator <- R6::R6Class("OperationsGraphGenerator",
  public = list(
    sas_dir = NULL,
    entrypoint = NULL,
    manifest_paths = NULL,
    verbose = FALSE,

    # Loaded from manifests — keyed by call_site_key
    operation_lookup = NULL,   # environment
    target_datasets = NULL,    # character vector

    # Built during parsing
    filename_aliases = NULL,   # named list: fileref -> path
    macro_definitions = NULL,  # named list: macro_name -> list of defs
    file_lines_cache = NULL,   # named list: filepath -> character vector
    macro_variables = NULL,    # named list: var -> value

    # Execution state
    last_modified = NULL,      # named list: dataset -> list of op_nodes
    graph_nodes = NULL,        # list of operation_node
    graph_edges = NULL,        # list of 2-element lists (source, target)
    visited_locations = NULL,  # character vector of call_site_keys
    entered_macros = NULL,     # character vector

    initialize = function(sas_dir, entrypoint, manifest_paths) {
      self$sas_dir <- sas_dir
      self$entrypoint <- entrypoint
      self$manifest_paths <- manifest_paths

      self$operation_lookup <- new.env(hash = TRUE, parent = emptyenv())
      self$target_datasets <- character(0)

      self$filename_aliases <- list()
      self$macro_definitions <- list()
      self$file_lines_cache <- list()
      self$macro_variables <- list()

      self$last_modified <- list()
      self$graph_nodes <- list()
      self$graph_edges <- list()
      self$visited_locations <- character(0)
      self$entered_macros <- character(0)

      private$.include_search_roots <- private$default_include_search_roots()
      private$.all_sas_files <- NULL
    },

    log = function(msg) {
      if (self$verbose) {
        cat(sprintf("  [DEBUG] %s\n", msg))
      }
    },

    load_manifests = function() {
      seen_ops <- character(0)
      for (manifest_path in self$manifest_paths) {
        manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
        target <- tolower(manifest$target_dataset %||% "")
        if (nzchar(target)) {
          self$target_datasets <- unique(c(self$target_datasets, target))
        }

        for (op in manifest$operations %||% list()) {
          if (!is.null(op$macro_source_file) && !is.null(op$macro_source_line)) {
            cs_key <- call_site_key(op$macro_source_file, op$macro_source_line)
          } else {
            cs_key <- call_site_key(op$file, op$line_number)
          }
          op_id <- paste0(cs_key, "||", op$dataset %||% "")
          if (!(op_id %in% seen_ops)) {
            seen_ops <- c(seen_ops, op_id)
            existing <- if (exists(cs_key, envir = self$operation_lookup, inherits = FALSE)) {
              get(cs_key, envir = self$operation_lookup)
            } else {
              list()
            }
            assign(cs_key, c(existing, list(op)), envir = self$operation_lookup)
          }
        }
      }
    },

    build_filename_alias_map = function() {
      sas_files <- private$discover_sas_files()
      for (sas_file in sas_files) {
        lines <- private$read_file(sas_file)
        for (line in lines) {
          m <- regmatches(line, regexec(.OG_FILENAME_RE, line, perl = TRUE))[[1]]
          if (length(m) < 3L) next
          fileref <- .og_normalize_fileref(m[2])
          path <- trimws(sub(";\\s*$", "", trimws(m[3])))

          resolved <- private$resolve_filename_path(path)
          if (!is.null(resolved)) {
            self$filename_aliases[[fileref]] <- resolved
            self$log(sprintf("Alias: %s -> %s", fileref, basename(resolved)))
          }
        }
      }
    },

    build_macro_map = function() {
      sas_files <- private$discover_sas_files()
      for (sas_file in sas_files) {
        lines <- private$read_file(sas_file)
        private$parse_macro_defs(sas_file, lines)
      }
    },

    walk_code = function() {
      entrypoint_path <- file.path(self$sas_dir, self$entrypoint)
      if (!file.exists(entrypoint_path)) {
        cat(sprintf("Error: Entrypoint file not found: %s\n", entrypoint_path))
        return(invisible(NULL))
      }

      private$walk_file(entrypoint_path, 1L, list(), depth = 0L,
                         in_macro = FALSE)

      private$walk_uncalled_top_level_macros()

      # Find target nodes
      targets <- Filter(
        function(n) n$dataset %in% self$target_datasets,
        self$graph_nodes
      )
      if (length(targets) == 0L) return(invisible(NULL))

      # Build predecessors map
      predecessors <- new.env(hash = TRUE, parent = emptyenv())
      for (edge in self$graph_edges) {
        src <- edge[[1]]
        tgt <- edge[[2]]
        tgt_id <- node_identity(tgt)
        existing <- if (exists(tgt_id, envir = predecessors, inherits = FALSE)) {
          get(tgt_id, envir = predecessors)
        } else {
          list()
        }
        assign(tgt_id, c(existing, list(src)), envir = predecessors)
      }

      # Group terminal nodes by dataset name
      terminals_by_dataset <- list()
      for (node in targets) {
        ds <- node$dataset
        terminals_by_dataset[[ds]] <- c(terminals_by_dataset[[ds]], list(node))
      }

      # Backward BFS per terminal dataset
      keep_ids <- character(0)
      # We need to track which nodes feed which terminals
      feeds_map <- new.env(hash = TRUE, parent = emptyenv())

      for (terminal_dataset in names(terminals_by_dataset)) {
        terminal_nodes <- terminals_by_dataset[[terminal_dataset]]
        reached_ids <- character(0)
        stack <- terminal_nodes

        # Collect IDs of terminal nodes
        for (tn in terminal_nodes) {
          reached_ids <- c(reached_ids, node_identity(tn))
        }

        while (length(stack) > 0L) {
          current <- stack[[length(stack)]]
          stack <- stack[-length(stack)]
          cur_id <- node_identity(current)

          preds <- if (exists(cur_id, envir = predecessors, inherits = FALSE)) {
            get(cur_id, envir = predecessors)
          } else {
            list()
          }
          for (pred in preds) {
            pred_id <- node_identity(pred)
            if (!(pred_id %in% reached_ids)) {
              reached_ids <- c(reached_ids, pred_id)
              stack <- c(stack, list(pred))
            }
          }
        }

        # Record feeds for all reached nodes
        for (rid in reached_ids) {
          existing_feeds <- if (exists(rid, envir = feeds_map, inherits = FALSE)) {
            get(rid, envir = feeds_map)
          } else {
            character(0)
          }
          assign(rid, unique(c(existing_feeds, terminal_dataset)), envir = feeds_map)
        }
        keep_ids <- unique(c(keep_ids, reached_ids))
      }

      # Apply feeds to nodes and prune
      self$graph_nodes <- Filter(function(n) {
        nid <- node_identity(n)
        nid %in% keep_ids
      }, self$graph_nodes)

      for (i in seq_along(self$graph_nodes)) {
        nid <- node_identity(self$graph_nodes[[i]])
        if (exists(nid, envir = feeds_map, inherits = FALSE)) {
          self$graph_nodes[[i]]$feeds <- get(nid, envir = feeds_map)
        }
      }

      self$graph_edges <- Filter(function(e) {
        node_identity(e[[1]]) %in% keep_ids &&
          node_identity(e[[2]]) %in% keep_ids
      }, self$graph_edges)
    },

    # --- Output generators ---------------------------------------------------

    generate_dot = function() {
      lines <- c(
        "digraph operations {",
        "    rankdir=TB;",
        '    node [shape=box, fontname="Helvetica", fontsize=9];',
        '    edge [color="#666666"];',
        ""
      )

      unique_nodes <- private$iter_unique_nodes()
      for (item in unique_nodes) {
        op_node <- item$node
        nid <- item$node_id
        line_info <- if (!is.null(op_node$end_line)) {
          sprintf("%d-%d", op_node$line_number, op_node$end_line)
        } else {
          as.character(op_node$line_number)
        }
        display <- private$display_file(op_node)
        label <- sprintf("%s\\n%s\\n%s:%s",
                         op_node$operation_type, op_node$dataset,
                         display, line_info)
        style <- get_operation_style(op_node, self$target_datasets)
        lines <- c(lines, sprintf('    %s [label="%s"%s];', nid, label, style))
      }

      lines <- c(lines, "")
      unique_edges <- private$iter_unique_edges()
      for (edge in unique_edges) {
        lines <- c(lines, sprintf("    %s -> %s;", edge[1], edge[2]))
      }
      lines <- c(lines, "}")
      paste(lines, collapse = "\n")
    },

    generate_txt = function() {
      lines <- "# Nodes"

      unique_nodes <- private$iter_unique_nodes()
      for (item in unique_nodes) {
        op_node <- item$node
        nid <- item$node_id
        display <- private$display_file(op_node)
        attrs <- c(
          sprintf("type=%s", escape_txt_value(op_node$operation_type)),
          sprintf("dataset=%s", escape_txt_value(op_node$dataset)),
          sprintf("file=%s", escape_txt_value(display)),
          sprintf("start_line=%d", op_node$line_number)
        )
        if (!is.null(op_node$end_line)) {
          attrs <- c(attrs, sprintf("end_line=%d", op_node$end_line))
        }
        if (op_node$dataset %in% self$target_datasets) {
          attrs <- c(attrs, "target=true")
        }
        if (length(op_node$feeds) > 0L) {
          attrs <- c(attrs, sprintf("feeds=%s",
            escape_txt_value(paste(sort(op_node$feeds), collapse = ","))))
        }
        if (!is.null(op_node$resolved_path)) {
          attrs <- c(attrs, sprintf("resolved_path=%s",
            escape_txt_value(op_node$resolved_path)))
        }
        lines <- c(lines, sprintf("N %s %s", nid, paste(attrs, collapse = " ")))
      }

      lines <- c(lines, "", "# Edges")
      unique_edges <- private$iter_unique_edges()
      for (edge in unique_edges) {
        lines <- c(lines, sprintf("E %s -> %s", edge[1], edge[2]))
      }
      paste(lines, collapse = "\n")
    },

    generate_graph_md = function() {
      lines <- c(
        sprintf("# Analysis for datasets: %s",
                paste(sort(self$target_datasets), collapse = ", ")),
        "",
        "## Input file mapping",
        "",
        "Mapping of input files to their corresponding SAS datasets:",
        "",
        private$render_infile_mapping_table(),
        "",
        "## Data lineage dependency graph",
        "",
        self$generate_txt(),
        ""
      )
      paste(lines, collapse = "\n")
    },

    generate_code_extracts_md = function() {
      result <- private$build_code_extracts()
      paste(result$lines, collapse = "\n")
    },

    generate_spec_index_md = function() {
      extracts <- private$build_code_extracts()
      ranges <- extracts$ranges
      outputs <- sort(self$target_datasets)
      layout <- private$bucket_layout()
      total_nodes <- sum(vapply(layout, function(b) length(b$nodes), integer(1)))

      lines <- c(
        "# Spec-build index",
        "",
        sprintf("Outputs covered: %s", paste(outputs, collapse = ", ")),
        sprintf("Total nodes:     %d", total_nodes),
        "",
        "## Input file mapping",
        "",
        "Mapping of input files to their corresponding SAS datasets:",
        "",
        private$render_infile_mapping_table(),
        "",
        "## Buckets (shared-first)",
        "",
        "| feeds | nodes |",
        "|-------|-------|"
      )
      for (bucket in layout) {
        lines <- c(lines, sprintf("| %s | %d |",
                                  paste(bucket$feeds, collapse = ", "),
                                  length(bucket$nodes)))
      }
      lines <- c(lines, "")

      for (bucket in layout) {
        lines <- c(lines,
          sprintf("## feeds: %s", paste(bucket$feeds, collapse = ", ")),
          "",
          "| # | node_id | type | file:line | code_extract_lines |",
          "|---|---------|------|-----------|--------------------|"
        )
        for (idx in seq_along(bucket$nodes)) {
          item <- bucket$nodes[[idx]]
          op_node <- item$node
          nid <- item$node_id
          location <- sprintf("%s:%d", private$display_file(op_node),
                              op_node$line_number)
          rng <- ranges[[nid]]
          range_str <- if (!is.null(rng)) {
            sprintf("%d-%d", rng[1], rng[2])
          } else {
            "?"
          }
          lines <- c(lines, sprintf("| %d | %s | %s | %s | %s |",
                                    idx, nid, op_node$operation_type,
                                    location, range_str))
        }
        lines <- c(lines, "")
      }
      paste(lines, collapse = "\n")
    },

    generate_spec_index_json = function() {
      extracts <- private$build_code_extracts()
      ranges <- extracts$ranges
      layout <- private$bucket_layout()

      buckets_list <- lapply(layout, function(bucket) {
        nodes_list <- lapply(bucket$nodes, function(item) {
          op_node <- item$node
          nid <- item$node_id
          rng <- ranges[[nid]]
          list(
            node_id = nid,
            type = op_node$operation_type,
            file = private$display_file(op_node),
            line = op_node$line_number,
            code_extract_lines = if (!is.null(rng)) as.list(rng) else list(0L, 0L)
          )
        })
        list(feeds = as.list(bucket$feeds), nodes = nodes_list)
      })

      payload <- list(
        outputs = as.list(sort(self$target_datasets)),
        total_nodes = sum(vapply(layout, function(b) length(b$nodes), integer(1))),
        buckets = buckets_list
      )
      jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE)
    }
  ),

  private = list(
    .include_search_roots = NULL,
    .all_sas_files = NULL,

    # --- Discovery & resolution ---------------------------------------------

    default_include_search_roots = function() {
      pkg_root <- tryCatch({
        # Try to locate project root relative to the package
        pkg_dir <- system.file(package = "DATALineage")
        if (nzchar(pkg_dir)) dirname(dirname(pkg_dir)) else getwd()
      }, error = function(e) getwd())
      procedures_dir <- file.path(pkg_root, "procedures")
      if (!dir.exists(procedures_dir)) return(character(0))
      dirs <- sort(list.dirs(procedures_dir, recursive = FALSE))
      sas_dirs <- file.path(dirs[grepl("^migration-", basename(dirs))], "sas")
      sas_dirs[dir.exists(sas_dirs)]
    },

    read_file = function(filepath) {
      if (!is.null(self$file_lines_cache[[filepath]])) {
        return(self$file_lines_cache[[filepath]])
      }
      lines <- tryCatch({
        readLines(filepath, encoding = "latin1", warn = FALSE)
      }, warning = function(w) {
        self$log(sprintf("File not found: %s", filepath))
        character(0)
      }, error = function(e) {
        self$log(sprintf("File not found: %s", filepath))
        character(0)
      })
      self$file_lines_cache[[filepath]] <- lines
      lines
    },

    collect_static_let_values = function(sas_files) {
      for (sas_file in sas_files) {
        lines <- tryCatch(
          readLines(sas_file, encoding = "latin1", warn = FALSE),
          error = function(e) character(0)
        )
        for (line in lines) {
          m <- regmatches(line, regexec(.OG_STATIC_LET_RE, line, perl = TRUE))[[1]]
          if (length(m) < 3L) next
          name <- tolower(m[2])
          raw_value <- trimws(m[3])
          if (is.null(self$macro_variables[[name]])) {
            self$macro_variables[[name]] <- raw_value
          }
        }
      }

      # Iterative fixed-point resolution
      current <- self$macro_variables
      for (iter in seq_len(10L)) {
        previous <- current
        for (name in names(current)) {
          value <- current[[name]]
          new_value <- gsub("&([A-Za-z_]\\w*)\\.?", "\\1", value, perl = TRUE)
          # Actually substitute references
          refs <- gregexpr("&([A-Za-z_]\\w*)\\.?", value, perl = TRUE)
          if (refs[[1]][1] != -1L) {
            matches <- regmatches(value, refs)[[1]]
            for (ref in matches) {
              ref_name <- tolower(sub("^&", "", sub("\\.?$", "", ref)))
              if (!is.null(current[[ref_name]])) {
                value <- sub(ref, current[[ref_name]], value, fixed = TRUE)
              }
            }
          }
          current[[name]] <- private$evaluate_static_macro_funcs(value)
        }
        if (identical(current, previous)) break
      }
      self$macro_variables <- Filter(function(v) !grepl("%", v, fixed = TRUE),
                                     current)
    },

    evaluate_static_macro_funcs = function(value) {
      previous <- NULL
      current <- value
      for (iter in seq_len(5L)) {
        if (identical(current, previous)) break
        previous <- current
        # Replace %substr(arg, start[, length])
        m <- regexec(.OG_SUBSTR_RE, current, perl = TRUE)
        positions <- m[[1]]
        if (positions[1] != -1L) {
          parts <- regmatches(current, m)[[1]]
          arg <- trimws(parts[2])
          if (grepl("&", arg, fixed = TRUE)) next
          start <- tryCatch(as.integer(parts[3]), warning = function(w) NA_integer_)
          if (is.na(start)) next
          start_idx <- max(1L, start)
          if (nzchar(parts[4])) {
            len <- tryCatch(as.integer(parts[4]), warning = function(w) NA_integer_)
            if (is.na(len)) next
            replacement <- substr(arg, start_idx, start_idx + len - 1L)
          } else {
            replacement <- substr(arg, start_idx, nchar(arg))
          }
          current <- sub(.OG_SUBSTR_RE, replacement, current, perl = TRUE)
        }
      }
      current
    },

    discover_sas_files = function() {
      if (!is.null(private$.all_sas_files)) return(private$.all_sas_files)

      initial <- sort(list.files(self$sas_dir, pattern = "\\.sas$",
                                  recursive = TRUE, full.names = TRUE))
      private$collect_static_let_values(initial)

      all_files <- initial
      seen <- normalizePath(initial, mustWork = FALSE)
      file_fileref_definitions <- list()
      global_filename_refs <- list()
      worklist <- initial

      while (length(worklist) > 0L) {
        # Sub-pass a: load FILENAME refs
        for (sas_file in worklist) {
          result <- parse_filename_statements(sas_file)
          for (nm in names(result$refs)) {
            global_filename_refs[[nm]] <- result$refs[[nm]]
          }
          key <- normalizePath(sas_file, mustWork = FALSE)
          file_fileref_definitions[[key]] <- c(
            file_fileref_definitions[[key]] %||% list(),
            result$defs
          )
        }

        # Sub-pass b: resolve includes
        next_worklist <- character(0)
        for (sas_file in worklist) {
          includes <- parse_include_statements(
            sas_file, file_fileref_definitions,
            macro_variables = self$macro_variables,
            search_roots = private$.include_search_roots,
            global_filename_refs = global_filename_refs
          )
          for (inc in includes) {
            target <- inc$target
            if (is.null(target) || !nzchar(target)) next
            target_key <- normalizePath(target, mustWork = FALSE)
            if (target_key %in% seen) next
            if (!file.exists(target)) next
            seen <- c(seen, target_key)
            all_files <- c(all_files, target)
            next_worklist <- c(next_worklist, target)
          }
        }

        if (length(next_worklist) == 0L) break
        private$collect_static_let_values(next_worklist)
        worklist <- next_worklist
      }

      private$.all_sas_files <- all_files
      all_files
    },

    resolve_filename_path = function(path) {
      substituted <- substitute_macro_vars(path, self$macro_variables)

      if (!grepl("&", substituted, fixed = TRUE)) {
        if (file.exists(substituted)) {
          return(normalizePath(substituted, mustWork = FALSE))
        }
      }

      # Extract last component
      if (grepl("/", substituted, fixed = TRUE)) {
        base <- tail(strsplit(substituted, "/", fixed = TRUE)[[1]], 1)
      } else {
        base <- substituted
      }

      if (grepl(".sas", tolower(base), fixed = TRUE)) {
        for (sas_file in private$discover_sas_files()) {
          if (private$filename_matches(basename(sas_file), base)) {
            return(sas_file)
          }
        }
      }
      NULL
    },

    filename_matches = function(actual, pattern) {
      regex_pattern <- gsub("&\\w+\\.?", ".*", pattern, perl = TRUE)
      regex_pattern <- paste0("^", regex_pattern, "$")
      grepl(regex_pattern, actual, ignore.case = TRUE, perl = TRUE)
    },

    # --- Macro parsing -------------------------------------------------------

    parse_macro_defs = function(filepath, lines) {
      i <- 1L
      while (i <= length(lines)) {
        line <- lines[[i]]
        m <- regmatches(line, regexec(.OG_MACRO_DEF_PARSE_RE, line,
                                      perl = TRUE))[[1]]
        if (length(m) >= 2L) {
          macro_name <- tolower(m[2])
          params_str <- if (length(m) >= 3L && nzchar(m[3])) m[3] else ""
          params <- if (nzchar(params_str)) {
            trimws(strsplit(params_str, ",")[[1]])
          } else {
            character(0)
          }
          params <- params[nzchar(params)]
          start_line <- i  # 1-indexed

          if (grepl(.OG_INLINE_EMPTY_RE, line, perl = TRUE, ignore.case = TRUE)) {
            end_line <- start_line
          } else {
            end_line <- start_line
            j <- i + 1L
            nest_level <- 1L
            while (j <= length(lines) && nest_level > 0L) {
              macro_count <- .count_matches(.OG_MACRO_OPEN_RE, lines[[j]])
              mend_count <- .count_matches(.OG_MEND_RE, lines[[j]])
              nest_level <- nest_level + macro_count - mend_count
              if (nest_level == 0L) {
                end_line <- j
              }
              j <- j + 1L
            }
          }

          macro_def <- new_og_macro_definition(
            name = macro_name,
            file_path = filepath,
            start_line = start_line,
            end_line = end_line,
            params = params
          )
          self$macro_definitions[[macro_name]] <- c(
            self$macro_definitions[[macro_name]] %||% list(),
            list(macro_def)
          )
          self$log(sprintf("Macro: %s in %s:%d-%d",
                           macro_name, basename(filepath),
                           start_line, end_line))
        }
        i <- i + 1L
      }
    },

    find_best_macro = function(macro_name, current_file, current_line) {
      definitions <- self$macro_definitions[[macro_name]]
      if (is.null(definitions) || length(definitions) == 0L) return(NULL)

      same_file_real <- NULL
      any_real <- NULL
      same_file_stub <- NULL

      for (macro_def in definitions) {
        has_body <- private$macro_has_body(macro_def)
        same_file_before <- (macro_def$file_path == current_file &&
                               macro_def$start_line < current_line)
        if (has_body) {
          if (same_file_before) same_file_real <- macro_def
          if (is.null(any_real)) any_real <- macro_def
        } else {
          if (same_file_before && is.null(same_file_stub)) {
            same_file_stub <- macro_def
          }
        }
      }

      if (!is.null(same_file_real)) return(same_file_real)
      if (!is.null(any_real)) return(any_real)
      if (!is.null(same_file_stub)) return(same_file_stub)
      definitions[[1]]
    },

    macro_has_body = function(macro_def) {
      if (macro_def$end_line <= macro_def$start_line) return(FALSE)
      lines <- private$read_file(macro_def$file_path)
      if (length(lines) == 0L) return(FALSE)
      body_start <- macro_def$start_line + 1L
      body_end <- macro_def$end_line - 1L
      if (body_start > body_end || body_start > length(lines)) return(FALSE)
      body_end <- min(body_end, length(lines))
      any(nzchar(trimws(lines[body_start:body_end])))
    },

    is_nested_macro = function(macro_def, same_file_defs) {
      for (other in same_file_defs) {
        if (identical(other, macro_def)) next
        if (other$start_line < macro_def$start_line &&
            macro_def$start_line <= other$end_line) {
          return(TRUE)
        }
      }
      FALSE
    },

    # --- Code walking --------------------------------------------------------

    walk_file = function(filepath, start_line, callstack, depth,
                          in_macro = FALSE, macro_end_line = NULL) {
      if (depth > 100L) {
        self$log(sprintf("Max depth reached at %s:%d", filepath, start_line))
        return(invisible(NULL))
      }

      lines <- private$read_file(filepath)
      if (length(lines) == 0L) return(invisible(NULL))

      file_basename <- basename(filepath)
      i <- start_line  # 1-indexed

      while (i <= length(lines)) {
        line <- lines[[i]]
        line_num <- i

        # If in macro and hit end line, return
        if (in_macro && !is.null(macro_end_line) && line_num >= macro_end_line) {
          return(invisible(NULL))
        }

        # Check for infinite loop
        location <- call_site_key(filepath, line_num)
        if (location %in% self$visited_locations) {
          i <- i + 1L
          next
        }
        self$visited_locations <- c(self$visited_locations, location)

        # 1. Check if this line matches operation(s) in manifest
        private$check_and_add_operation(filepath, line_num, line)

        # 2. %if/%else chain detection
        chain <- private$detect_if_chain(lines, i)
        if (!is.null(chain)) {
          private$walk_if_chain(
            filepath, lines, chain$branches, chain$has_else,
            callstack, depth, in_macro, macro_end_line
          )
          i <- chain$end_idx
          next
        }

        # 3. Macro definition — skip to end
        if (private$is_macro_definition_start(line)) {
          i <- private$skip_to_macro_end(lines, i)
          next
        }

        # 4. %include statement
        include_file <- private$parse_include(line)
        if (!is.null(include_file)) {
          self$log(sprintf("Include: %s -> %s", trimws(line), basename(include_file)))
          callstack <- c(callstack, list(new_call_stack_frame(
            file_path = filepath,
            line = line_num + 1L,
            frame_type = "include"
          )))
          private$walk_file(include_file, 1L, callstack, depth + 1L,
                             in_macro = FALSE)
          if (length(callstack) > 0L) {
            callstack <- callstack[-length(callstack)]
          }
          i <- i + 1L
          next
        }

        # 5. Macro call
        macro_name <- private$parse_macro_call(line)
        if (!is.null(macro_name) &&
            !is.null(self$macro_definitions[[macro_name]])) {
          macro_def <- private$find_best_macro(macro_name, filepath, line_num)
          if (is.null(macro_def)) {
            i <- i + 1L
            next
          }
          self$log(sprintf("Macro call: %%%s -> %s:%d",
                           macro_name, basename(macro_def$file_path),
                           macro_def$start_line))
          self$entered_macros <- unique(c(self$entered_macros, macro_name))
          callstack <- c(callstack, list(new_call_stack_frame(
            file_path = filepath,
            line = line_num + 1L,
            frame_type = "macro",
            macro_name = macro_name
          )))
          private$walk_file(
            macro_def$file_path,
            macro_def$start_line + 1L,
            callstack,
            depth + 1L,
            in_macro = TRUE,
            macro_end_line = macro_def$end_line
          )
          if (length(callstack) > 0L) {
            callstack <- callstack[-length(callstack)]
          }
          i <- i + 1L
          next
        }

        i <- i + 1L
      }
    },

    check_and_add_operation = function(filepath, line_num, line) {
      file_basename <- basename(filepath)
      cs_key <- call_site_key(file_basename, line_num)

      matching_ops <- if (exists(cs_key, envir = self$operation_lookup,
                                 inherits = FALSE)) {
        get(cs_key, envir = self$operation_lookup)
      } else {
        list()
      }
      if (length(matching_ops) == 0L) return(FALSE)

      pre_state <- self$last_modified
      new_writers <- list()

      for (op in matching_ops) {
        end_line <- op$end_line
        if (!is.null(op$macro_end_line)) {
          end_line <- op$macro_end_line
        }

        op_node <- new_operation_node(
          dataset = tolower(op$dataset),
          file_path = filepath,
          line_number = line_num,
          operation_type = op$operation_type,
          input_datasets = op$input_datasets %||% character(0),
          depth = op$depth %||% 0L,
          resolved_path = op$resolved_path,
          end_line = end_line
        )

        self$log(sprintf("Operation: %s (%s) at %s:%d",
                         op_node$dataset, op_node$operation_type,
                         file_basename, line_num))
        self$graph_nodes <- c(self$graph_nodes, list(op_node))

        for (input_ds in op$input_datasets %||% character(0)) {
          source_ops <- pre_state[[tolower(input_ds)]]
          if (!is.null(source_ops)) {
            for (source_op in source_ops) {
              if (node_identity(source_op) != node_identity(op_node)) {
                self$graph_edges <- c(self$graph_edges,
                                      list(list(source_op, op_node)))
                self$log(sprintf("  Edge: %s@%d -> %s@%d",
                                 source_op$dataset, source_op$line_number,
                                 op_node$dataset, line_num))
              }
            }
          }
        }

        ds <- op_node$dataset
        new_writers[[ds]] <- c(new_writers[[ds]] %||% list(), list(op_node))
      }

      # Publish writer updates atomically
      for (dataset in names(new_writers)) {
        self$last_modified[[dataset]] <- new_writers[[dataset]]
      }
      TRUE
    },

    walk_uncalled_top_level_macros = function() {
      sas_dir_resolved <- normalizePath(self$sas_dir, mustWork = FALSE)

      # Build per-file view
      defs_by_file <- list()
      for (defs in self$macro_definitions) {
        for (macro_def in defs) {
          fp <- macro_def$file_path
          defs_by_file[[fp]] <- c(defs_by_file[[fp]] %||% list(),
                                  list(macro_def))
        }
      }

      for (file_path in names(defs_by_file)) {
        file_defs <- defs_by_file[[file_path]]

        # Restrict to definitions inside the analyzable tree
        resolved_fp <- normalizePath(file_path, mustWork = FALSE)
        if (!startsWith(resolved_fp, sas_dir_resolved)) next

        top_level_defs <- Filter(
          function(d) !private$is_nested_macro(d, file_defs),
          file_defs
        )
        if (length(top_level_defs) != 1L) next

        macro_def <- top_level_defs[[1]]
        name <- macro_def$name
        if (name %in% self$entered_macros) next
        if (!private$macro_has_body(macro_def)) next

        self$log(sprintf("Walking uncalled top-level macro: %%%s -> %s:%d",
                         name, basename(macro_def$file_path),
                         macro_def$start_line))
        self$entered_macros <- unique(c(self$entered_macros, name))
        callstack <- list(new_call_stack_frame(
          file_path = macro_def$file_path,
          line = macro_def$start_line,
          frame_type = "macro",
          macro_name = name
        ))
        private$walk_file(
          macro_def$file_path,
          macro_def$start_line + 1L,
          callstack,
          depth = 1L,
          in_macro = TRUE,
          macro_end_line = macro_def$end_line
        )
      }
    },

    # --- Conditional flow ----------------------------------------------------

    is_macro_definition_start = function(line) {
      grepl(.OG_MACRO_DEF_RE, line, perl = TRUE)
    },

    skip_to_macro_end = function(lines, start_idx) {
      line <- lines[[start_idx]]
      # Inline empty macro
      if (grepl(.OG_INLINE_EMPTY_RE, line, perl = TRUE, ignore.case = TRUE)) {
        return(start_idx + 1L)
      }
      i <- start_idx + 1L
      nest_level <- 1L
      while (i <= length(lines) && nest_level > 0L) {
        macro_count <- .count_matches(.OG_MACRO_OPEN_RE, lines[[i]])
        mend_count <- .count_matches(.OG_MEND_RE, lines[[i]])
        nest_level <- nest_level + macro_count - mend_count
        i <- i + 1L
      }
      i  # Index after matching %mend
    },

    detect_if_chain = function(lines, start_idx) {
      if (!grepl(.OG_IF_DO_RE, lines[[start_idx]], perl = TRUE)) return(NULL)

      branches <- list()
      has_else <- FALSE
      head <- start_idx

      repeat {
        body_end <- private$find_matching_end(lines, head)
        if (body_end < 0L) return(NULL)
        branches <- c(branches, list(c(head, body_end)))

        next_idx <- private$next_chain_line(lines, body_end + 1L)
        if (next_idx > length(lines)) break
        next_line <- lines[[next_idx]]
        if (grepl(.OG_ELSE_IF_DO_RE, next_line, perl = TRUE)) {
          head <- next_idx
          next
        }
        if (grepl(.OG_ELSE_DO_RE, next_line, perl = TRUE)) {
          head <- next_idx
          has_else <- TRUE
          next
        }
        break
      }

      list(branches = branches, has_else = has_else,
           end_idx = body_end + 1L)
    },

    find_matching_end = function(lines, start_idx) {
      depth <- 1L
      i <- start_idx + 1L
      while (i <= length(lines)) {
        depth <- depth + .count_matches(.OG_DO_TOKEN_RE, lines[[i]])
        depth <- depth - .count_matches(.OG_END_TOKEN_RE, lines[[i]])
        if (depth <= 0L) return(i)
        i <- i + 1L
      }
      -1L
    },

    next_chain_line = function(lines, idx) {
      while (idx <= length(lines) &&
             grepl(.OG_CHAIN_SKIP_RE, lines[[idx]], perl = TRUE)) {
        idx <- idx + 1L
      }
      idx
    },

    walk_if_chain = function(filepath, lines, branches, has_else,
                              callstack, depth, in_macro, macro_end_line) {
      # Snapshot last_modified
      snapshot <- lapply(self$last_modified, function(writers) {
        lapply(writers, identity)
      })
      branch_states <- list()

      for (branch in branches) {
        head_idx <- branch[1]
        body_end <- branch[2]
        # Restore snapshot for each branch
        self$last_modified <- lapply(snapshot, function(writers) {
          lapply(writers, identity)
        })
        stop_line <- body_end + 1L
        if (in_macro && !is.null(macro_end_line)) {
          stop_line <- min(stop_line, macro_end_line)
        }
        private$walk_file(
          filepath, head_idx, callstack, depth + 1L,
          in_macro = TRUE, macro_end_line = stop_line
        )
        branch_states <- c(branch_states, list(self$last_modified))
      }

      if (!has_else) {
        branch_states <- c(branch_states, list(snapshot))
      }

      # Merge: union of writers per dataset across branches
      merged <- list()
      for (state in branch_states) {
        for (ds in names(state)) {
          writers <- state[[ds]]
          if (length(writers) > 0L) {
            existing <- merged[[ds]] %||% list()
            # Union by node_identity
            existing_ids <- vapply(existing, node_identity, character(1))
            for (w in writers) {
              if (!(node_identity(w) %in% existing_ids)) {
                existing <- c(existing, list(w))
                existing_ids <- c(existing_ids, node_identity(w))
              }
            }
            merged[[ds]] <- existing
          }
        }
      }
      self$last_modified <- merged
    },

    # --- Include / macro call parsing ----------------------------------------

    parse_include = function(line) {
      if (!grepl("(?i)^\\s*%include\\s+", line, perl = TRUE)) return(NULL)

      m <- regmatches(line, regexec(.OG_INCLUDE_FILEREF_RE, line,
                                    perl = TRUE))[[1]]
      if (length(m) >= 2L) {
        fileref <- .og_normalize_fileref(m[2])
        if (fileref %in% c("if", "do", "else", "end", "let", "put")) {
          return(NULL)
        }
        if (!is.null(self$filename_aliases[[fileref]])) {
          return(self$filename_aliases[[fileref]])
        }
        # Glob fallback: try to find by substring
        for (sas_file in private$discover_sas_files()) {
          if (grepl(fileref, tolower(basename(sas_file)), fixed = TRUE)) {
            return(sas_file)
          }
        }
        return(NULL)
      }

      m <- regmatches(line, regexec(.OG_INCLUDE_QUOTED_RE, line,
                                    perl = TRUE))[[1]]
      if (length(m) >= 2L) {
        return(private$resolve_filename_path(m[2]))
      }

      NULL
    },

    parse_macro_call = function(line) {
      # Skip macro definition
      if (grepl("(?i)^\\s*%macro\\s+", line, perl = TRUE)) return(NULL)
      if (grepl("(?i)^\\s*%mend", line, perl = TRUE)) return(NULL)

      m <- regmatches(line, regexec(.OG_MACRO_CALL_RE, line,
                                    perl = TRUE))[[1]]
      if (length(m) >= 2L) {
        macro_name <- tolower(m[2])
        if (macro_name %in% .OG_SKIP_MACROS) return(NULL)
        return(macro_name)
      }
      NULL
    },

    # --- Output helpers ------------------------------------------------------

    display_file = function(op_node) {
      rel <- tryCatch({
        # Make path relative to sas_dir
        sas_dir_norm <- normalizePath(self$sas_dir, mustWork = FALSE)
        fp_norm <- normalizePath(op_node$file_path, mustWork = FALSE)
        if (startsWith(fp_norm, paste0(sas_dir_norm, "/"))) {
          substring(fp_norm, nchar(sas_dir_norm) + 2L)
        } else {
          op_node$file_path
        }
      }, error = function(e) op_node$file_path)
      rel
    },

    iter_unique_nodes = function() {
      seen <- character(0)
      result <- list()
      for (op_node in self$graph_nodes) {
        nid <- make_node_id(op_node, private$display_file(op_node))
        if (nid %in% seen) next
        seen <- c(seen, nid)
        result <- c(result, list(list(node = op_node, node_id = nid)))
      }
      result
    },

    iter_unique_edges = function() {
      seen <- character(0)
      result <- list()
      for (edge in self$graph_edges) {
        src_id <- make_node_id(edge[[1]], private$display_file(edge[[1]]))
        tgt_id <- make_node_id(edge[[2]], private$display_file(edge[[2]]))
        key <- paste0(src_id, "->", tgt_id)
        if (key %in% seen) next
        seen <- c(seen, key)
        result <- c(result, list(c(src_id, tgt_id)))
      }
      result
    },

    unique_infile_mappings = function() {
      seen <- character(0)
      result <- list()
      for (op_node in self$graph_nodes) {
        if (op_node$operation_type != "INFILE" ||
            is.null(op_node$resolved_path)) next
        dataset <- op_node$dataset
        if (grepl("(?i)^infile:", dataset)) {
          dataset <- substring(dataset, 8L)
        }
        display_filepath <- gsub("&(\\w+)\\.?", "<\\1>", op_node$resolved_path,
                                 perl = TRUE)
        key <- paste0(dataset, "|", display_filepath)
        if (key %in% seen) next
        seen <- c(seen, key)
        result <- c(result, list(list(dataset = dataset,
                                      filepath = display_filepath)))
      }
      result
    },

    render_infile_mapping_table = function() {
      mappings <- private$unique_infile_mappings()
      if (length(mappings) == 0L) {
        return("*No external input files detected.*")
      }
      rows <- c("| Dataset | Input File |", "|---------|------------|")
      for (m in mappings) {
        rows <- c(rows, sprintf("| %s | %s |", m$dataset, m$filepath))
      }
      rows
    },

    bucket_layout = function() {
      n_outputs <- length(self$target_datasets)
      buckets <- list()

      unique_nodes <- private$iter_unique_nodes()
      for (item in unique_nodes) {
        feeds <- paste(sort(item$node$feeds), collapse = ",")
        if (is.null(buckets[[feeds]])) {
          buckets[[feeds]] <- list(
            feeds = sort(item$node$feeds),
            nodes = list()
          )
        }
        buckets[[feeds]]$nodes <- c(buckets[[feeds]]$nodes, list(item))
      }

      # Sort buckets: shared-first, then by size
      scope_rank <- function(feeds) {
        n <- length(feeds)
        if (n == n_outputs && n_outputs > 0L) return(0L)
        if (n > 1L) return(1L)
        2L
      }

      bucket_list <- unname(buckets)
      if (length(bucket_list) == 0L) return(list())

      order_keys <- vapply(bucket_list, function(b) {
        sprintf("%d|%05d|%05d|%s",
                scope_rank(b$feeds),
                10000L - length(b$feeds),
                10000L - length(b$nodes),
                paste(b$feeds, collapse = ","))
      }, character(1))

      bucket_list[order(order_keys)]
    },

    build_code_extracts = function() {
      lines <- c(
        sprintf("# Code extracts for datasets: %s",
                paste(sort(self$target_datasets), collapse = ", ")),
        ""
      )
      ranges <- list()

      unique_nodes <- private$iter_unique_nodes()
      for (item in unique_nodes) {
        op_node <- item$node
        nid <- item$node_id
        start <- length(lines) + 1L

        lines <- c(lines, sprintf("### %s", nid))
        lines <- c(lines, "")
        lines <- c(lines, sprintf("- **type:** %s", op_node$operation_type))
        line_info <- if (!is.null(op_node$end_line)) {
          sprintf("%d-%d", op_node$line_number, op_node$end_line)
        } else {
          as.character(op_node$line_number)
        }
        lines <- c(lines, sprintf("- **file:** %s:%s",
                                  private$display_file(op_node), line_info))
        if (!is.null(op_node$resolved_path)) {
          lines <- c(lines, sprintf("- **resolved path:** %s",
                                    op_node$resolved_path))
        }
        lines <- c(lines, "- **code:**")
        lines <- c(lines, "```sas")
        code <- private$extract_code(op_node)
        lines <- c(lines, strsplit(code, "\n", fixed = TRUE)[[1]])
        lines <- c(lines, "```")

        ranges[[nid]] <- c(start, length(lines))
        lines <- c(lines, "")  # blank separator
      }

      list(lines = lines, ranges = ranges)
    },

    extract_code = function(op_node) {
      file_lines <- private$read_file(op_node$file_path)
      if (length(file_lines) == 0L) {
        return(sprintf("/* Could not read file: %s */",
                       private$display_file(op_node)))
      }
      start <- op_node$line_number
      end <- if (!is.null(op_node$end_line)) op_node$end_line else op_node$line_number
      start <- max(1L, start)
      end <- min(length(file_lines), end)
      if (start > length(file_lines)) {
        return(sprintf("/* Line %d out of range */", op_node$line_number))
      }
      paste(file_lines[start:end], collapse = "\n")
    }
  )
)
