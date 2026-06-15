# analyzer.R — SASLineageAnalyzer R6 class (ported from trace_lineage.py)

.MACRO_CALL_WITH_PARENS_RE <- "^\\s*%(\\w+)\\s*\\(([^)]*)\\)\\s*;?"
.MACRO_CALL_BARE_RE <- "^\\s*%(\\w+)\\s*;"
.STATIC_LET_RE <- "^\\s*%let\\s+([A-Za-z_]\\w*)\\s*=\\s*([^;]*);"
.SUBSTR_RE <- "%substr\\s*\\(\\s*([^,)]+?)\\s*,\\s*(\\d+)\\s*(?:,\\s*(\\d+)\\s*)?\\)"
.ODS_TRIGGER_RE <- "ods\\s+(?:tagsets\\.csv|xml|csv)\\s+file\\s*="

.MACRO_CTRL_FLOW <- c(
 "if", "do", "else", "end", "let", "put", "include",
 "global", "local", "sysfunc", "eval", "str", "nrstr",
 "then", "goto", "label", "return", "abort", "run",
 "mend", "macro"
)

.MAX_EXPANSION_DEPTH <- 12L

.has_body <- function(macro_def) {
  if (is.null(macro_def)) return(FALSE)
  body <- macro_def$body
  if (is.null(body) || length(body) == 0L) return(FALSE)
  any(nzchar(trimws(body)))
}

#' SAS Lineage Analyzer R6 Class
#' @export
SASLineageAnalyzer <- R6::R6Class("SASLineageAnalyzer",
  public = list(
    sas_dir = NULL,
    dataset_operations = NULL,
    dataset_to_ops = NULL,
    macro_definitions = NULL,
    macro_definition_versions = NULL,
    file_macro_definitions = NULL,
    file_includes = NULL,
    file_fileref_definitions = NULL,
    .file_macro_exports_cache = NULL,
    macro_calls = NULL,
    filename_refs = NULL,
    infile_usage = NULL,
    macro_variables = NULL,
    .include_search_roots = NULL,
    .last_sql_output = NULL,

    initialize = function(sas_dir, include_search_roots = NULL) {
      self$sas_dir <- sas_dir
      self$dataset_operations <- list()
      self$dataset_to_ops <- list()
      self$macro_definitions <- list()
      self$macro_definition_versions <- list()
      self$file_macro_definitions <- list()
      self$file_includes <- list()
      self$file_fileref_definitions <- list()
      self$.file_macro_exports_cache <- list()
      self$macro_calls <- list()
      self$filename_refs <- list()
      self$infile_usage <- list()
      self$macro_variables <- list()
      if (is.null(include_search_roots)) {
        include_search_roots <- private$default_include_search_roots()
      }
      self$.include_search_roots <- as.character(include_search_roots)
      self$.last_sql_output <- list()
    },

    parse_all_sas_files = function() {
      initial_files <- sort(list.files(self$sas_dir, pattern = "\\.sas$",
                                        recursive = TRUE, full.names = TRUE))
      message(sprintf("Found %d SAS files", length(initial_files)))

      private$collect_static_let_values(initial_files)

      message("Pass 1: Collecting FILENAME statements and macro definitions...")
      all_files <- private$pass1_walk_include_closure(initial_files)

      message(sprintf("Found %d filename references", length(self$filename_refs)))
      message(sprintf("Found %d macro definitions", length(self$macro_definitions)))
      if (length(all_files) > length(initial_files)) {
        extra <- length(all_files) - length(initial_files)
        message(sprintf("Pulled in %d additional SAS file(s) via cross-procedure %%include resolution", extra))
      }

      message("Pass 2: Parsing dataset operations...")
      for (sas_file in all_files) {
        self$parse_sas_file(sas_file)
      }

      private$expand_uncalled_top_level_macros()
    },

    parse_sas_file = function(filepath) {
      lines <- readLines(filepath, encoding = "latin1", warn = FALSE)
      i <- 1L
      while (i <= length(lines)) {
        # Block comments
        result <- handle_block_comments(lines, i)
        if (result$should_continue) {
          if (!is.null(result$replacement)) lines[[result$new_i]] <- result$replacement
          i <- result$new_i
          next
        }

        # %macro/%mend blocks
        result <- private$handle_macro_block(lines, i)
        if (result$should_continue) {
          i <- result$new_i
          next
        }

        # DATA steps
        result <- private$handle_data_step(lines, i, filepath)
        if (result$should_continue) {
          i <- result$new_i
          next
        }

        # PROC SQL
        result <- private$handle_proc_sql(lines, i, filepath)
        if (result$should_continue) {
          i <- result$new_i
          next
        }

        # PROC SORT
        if (private$handle_proc_sort(lines, i, filepath)) {
          i <- i + 1L
          next
        }

        # Generic PROC
        result <- private$handle_proc_generic(lines, i, filepath)
        if (result$handled) {
          i <- if (result$new_i > i) result$new_i else i + 1L
          next
        }

        # PROC EXPORT
        result <- private$handle_proc_export(lines, i, filepath)
        if (result$should_continue) {
          i <- result$new_i
          next
        }

        # PROC FORMAT
        result <- private$handle_proc_format(lines, i, filepath)
        if (result$should_continue) {
          i <- result$new_i
          next
        }

        # ODS CSV
        result <- private$handle_ods_tagsets_csv(lines, i, filepath)
        if (result$should_continue) {
          i <- result$new_i
          next
        }

        # %let
        if (private$handle_let(lines[[i]], i, filepath)) {
          i <- i + 1L
          next
        }

        # Macro calls
        if (private$handle_macro_call(lines, i, filepath)) {
          i <- i + 1L
          next
        }

        i <- i + 1L
      }
    },

    trace_dependencies = function(target_dataset, max_depth = 20L) {
      target_dataset <- private$resolve_fileref_alias(tolower(target_dataset))
      visited <- character(0)
      result <- list()

      trace_inner <- function(dataset, depth = 0L) {
        if (depth > max_depth) return()
        if (dataset %in% visited) return()
        visited <<- c(visited, dataset)

        if (startsWith(dataset, "infile:")) {
          fileref <- sub("^infile:", "", dataset)
          if (!is.null(self$filename_refs[[fileref]])) {
            file_info <- self$filename_refs[[fileref]]
            result[[length(result) + 1L]] <<- list(
              dataset = dataset,
              operation = "INFILE",
              file = basename(file_info[[1]]),
              line = file_info[[2]],
              code_snippet = paste0("filename ", fileref, " ", file_info[[3]]),
              inputs = character(0),
              depth = depth,
              resolved_path = file_info[[3]]
            )
          }
          return()
        }

        ops <- self$dataset_to_ops[[dataset]]
        if (is.null(ops)) ops <- list()

        for (op in ops) {
          display_file <- if (!is.null(op$macro_source_file)) {
            basename(op$macro_source_file)
          } else {
            basename(op$file)
          }
          display_line <- if (!is.null(op$macro_source_line)) {
            op$macro_source_line
          } else {
            op$line_number
          }
          display_end_line <- if (!is.null(op$macro_end_line)) {
            op$macro_end_line
          } else {
            op$end_line
          }

          entry <- list(
            dataset   = op$dataset,
            operation = op$operation_type,
            file      = display_file,
            line      = display_line,
            end_line  = display_end_line,
            code_snippet = substr(op$code_snippet, 1, 10000),
            inputs    = op$input_datasets,
            depth     = depth
          )
          if (!is.null(op$macro_source_file)) {
            entry$macro_name <- op$macro_name
            entry$macro_source_file <- basename(op$macro_source_file)
            entry$macro_source_line <- op$macro_source_line
            entry$macro_end_line <- op$macro_end_line
            entry$macro_call_file <- basename(op$file)
            entry$macro_call_line <- op$line_number
          }
          result[[length(result) + 1L]] <<- entry

          for (input_ds in op$input_datasets) {
            trace_inner(tolower(input_ds), depth + 1L)
          }
        }
      }

      trace_inner(target_dataset)
      result
    },

    generate_json_manifest = function(target_dataset, output_file = "lineage-manifest.json",
                                       filter_orphan_nodes = FALSE) {
      canonical_target <- private$resolve_fileref_alias(tolower(target_dataset))
      lineage <- self$trace_dependencies(canonical_target)

      if (filter_orphan_nodes) {
        lineage <- Filter(function(item) {
          length(item$inputs) > 0L || item$operation %in% c("INFILE", "PROC FORMAT")
        }, lineage)
      }

      manifest_ops <- lapply(lineage, function(item) {
        op_data <- list(
          dataset         = item$dataset,
          operation_type  = item$operation,
          file            = item$file,
          line_number     = item$line,
          end_line        = item$end_line,
          input_datasets  = if (length(item$inputs) == 0L) list() else as.list(item$inputs),
          depth           = item$depth
        )
        if (item$operation == "INFILE") {
          op_data$resolved_path <- item$resolved_path %||% "unknown"
        }
        if (!is.null(item$macro_source_file)) {
          op_data$macro_name <- item$macro_name
          op_data$macro_source_file <- item$macro_source_file
          op_data$macro_source_line <- item$macro_source_line
          op_data$macro_end_line <- item$macro_end_line
          op_data$macro_call_file <- item$macro_call_file
          op_data$macro_call_line <- item$macro_call_line
        }
        op_data
      })

      manifest <- list(
        target_dataset = canonical_target,
        total_datasets = length(lineage),
        operations     = if (length(manifest_ops) == 0L) list() else manifest_ops
      )

      jsonlite::write_json(manifest, output_file, pretty = TRUE, auto_unbox = TRUE)
      message(sprintf("JSON manifest generated: %s", output_file))
      manifest
    },

    generate_report = function(target_dataset, output_file = "lineage-report.md",
                                filter_orphan_nodes = FALSE) {
      lineage <- self$trace_dependencies(target_dataset)

      if (filter_orphan_nodes) {
        lineage <- Filter(function(item) {
          length(item$inputs) > 0L || item$operation %in% c("INFILE", "PROC FORMAT")
        }, lineage)
      }

      lines_out <- character(0)
      lines_out <- c(lines_out, sprintf("# Data Lineage Report for: %s\n", target_dataset))
      lines_out <- c(lines_out, sprintf("Total datasets involved: %d\n", length(lineage)))

      infiles <- Filter(function(item) item$operation == "INFILE", lineage)
      if (length(infiles) > 0L) {
        lines_out <- c(lines_out, sprintf("Input files identified: %d\n", length(infiles)))
        lines_out <- c(lines_out, "## Input Files (Leaf Nodes)\n")
        for (item in infiles) {
          fileref <- sub("^[^:]+:", "", item$dataset)
          lines_out <- c(lines_out,
            sprintf("- **%s**: `%s`", fileref, item$resolved_path %||% "unknown"),
            sprintf("  - Defined in: `%s:%s`", item$file, item$line)
          )
        }
        lines_out <- c(lines_out, "")
      }

      by_depth <- list()
      for (item in lineage) {
        key <- as.character(item$depth)
        by_depth[[key]] <- c(by_depth[[key]], list(item))
      }

      lines_out <- c(lines_out, "## Dependency Tree (by depth)\n")
      for (depth_key in sort(as.integer(names(by_depth)), decreasing = TRUE)) {
        lines_out <- c(lines_out, sprintf("\n### Depth %d\n", depth_key))
        for (item in by_depth[[as.character(depth_key)]]) {
          lines_out <- c(lines_out,
            sprintf("**%s**", item$dataset),
            sprintf("- Operation: %s", item$operation),
            sprintf("- Location: `%s:%s`", item$file, item$line)
          )
          if (length(item$inputs) > 0L) {
            lines_out <- c(lines_out, sprintf("- Inputs: %s", paste(item$inputs, collapse = ", ")))
          }
          if (item$operation == "INFILE") {
            lines_out <- c(lines_out, sprintf("- Resolved Path: `%s`", item$resolved_path %||% "unknown"))
          }
          lines_out <- c(lines_out, sprintf("- Code:\n```sas\n%s\n```\n", item$code_snippet))
        }
      }

      writeLines(lines_out, output_file)
      message(sprintf("Report generated: %s", output_file))
    },

    deduplicate_operations = function() {
      seen <- character(0)
      unique_ops <- list()

      for (op in self$dataset_operations) {
        key <- paste(tolower(op$dataset), op$file, op$line_number,
                     op$macro_source_line %||% "NA", sep = "|")
        if (!(key %in% seen)) {
          seen <- c(seen, key)
          unique_ops <- c(unique_ops, list(op))
        }
      }

      self$dataset_operations <- unique_ops
      self$dataset_to_ops <- list()
      for (op in unique_ops) {
        ds <- tolower(op$dataset)
        if (is.null(self$dataset_to_ops[[ds]])) {
          self$dataset_to_ops[[ds]] <- list()
        }
        self$dataset_to_ops[[ds]] <- c(self$dataset_to_ops[[ds]], list(op))
      }
    }
  ),

  private = list(
    default_include_search_roots = function() {
      tryCatch({
        project_root <- dirname(dirname(self$sas_dir))
        procedures_dir <- file.path(project_root, "procedures")
        if (!dir.exists(procedures_dir)) return(character(0))
        dirs <- list.dirs(procedures_dir, recursive = FALSE, full.names = TRUE)
        sas_dirs <- file.path(dirs[grepl("migration-", basename(dirs))], "sas")
        sort(sas_dirs[dir.exists(sas_dirs)])
      }, error = function(e) character(0))
    },

    collect_static_let_values = function(sas_files) {
      for (sas_file in sas_files) {
        tryCatch({
          file_lines <- readLines(sas_file, encoding = "latin1", warn = FALSE)
          for (line in file_lines) {
            m <- regmatches(line, regexec(.STATIC_LET_RE, line,
                                          perl = TRUE, ignore.case = TRUE))[[1]]
            if (length(m) < 3L) next
            name <- tolower(m[2])
            raw_value <- trimws(m[3])
            if (is.null(self$macro_variables[[name]])) {
              self$macro_variables[[name]] <- raw_value
            }
          }
        }, error = function(e) NULL)
      }

      # Fixed-point substitution
      .MACRO_REF_RE <- "&([A-Za-z_]\\w*)\\.?"
      current <- self$macro_variables
      previous <- NULL
      for (iter in seq_len(10L)) {
        if (identical(current, previous)) break
        previous <- current
        for (name in names(current)) {
          value <- current[[name]]
          locs <- gregexpr(.MACRO_REF_RE, value, perl = TRUE)
          if (locs[[1]][1] != -1L) {
            matched <- regmatches(value, locs)[[1]]
            replacements <- vapply(matched, function(tok) {
              ref <- regmatches(tok, regexec(.MACRO_REF_RE, tok, perl = TRUE))[[1]]
              refname <- tolower(ref[2])
              if (!is.null(current[[refname]])) current[[refname]] else tok
            }, character(1))
            regmatches(value, locs) <- list(replacements)
          }
          new_value <- private$evaluate_static_macro_funcs(value)
          current[[name]] <- new_value
        }
      }
      self$macro_variables <- Filter(function(v) !grepl("%", v, fixed = TRUE), current)
    },

    evaluate_static_macro_funcs = function(value) {
      previous <- NULL
      current <- value
      for (iter in seq_len(5L)) {
        if (identical(current, previous)) break
        previous <- current
        locs <- gregexpr(.SUBSTR_RE, current, perl = TRUE, ignore.case = TRUE)
        if (locs[[1]][1] == -1L) break
        matched <- regmatches(current, locs)[[1]]
        replacements <- vapply(matched, function(m_str) {
          m <- regmatches(m_str, regexec(.SUBSTR_RE, m_str,
                                          perl = TRUE, ignore.case = TRUE))[[1]]
          arg <- trimws(m[2])
          if (grepl("&", arg, fixed = TRUE)) return(m_str)
          start <- tryCatch(as.integer(m[3]), warning = function(e) NA_integer_)
          if (is.na(start)) return(m_str)
          start_idx <- max(1L, start)
          if (nzchar(m[4])) {
            len <- tryCatch(as.integer(m[4]), warning = function(e) NA_integer_)
            if (is.na(len)) return(m_str)
            return(substr(arg, start_idx, start_idx + len - 1L))
          }
          substr(arg, start_idx, nchar(arg))
        }, character(1))
        regmatches(current, locs) <- list(replacements)
      }
      current
    },

    pass1_walk_include_closure = function(initial_files) {
      parsed <- character(0)
      all_files <- initial_files
      worklist <- initial_files

      repeat {
        new_files <- character(0)
        for (f in worklist) {
          key <- normalizePath(f, mustWork = FALSE)
          if (!(key %in% parsed)) new_files <- c(new_files, f)
        }
        if (length(new_files) == 0L) break

        # Sub-pass a: filename statements
        for (sas_file in new_files) {
          private$parse_filename_stmts(sas_file)
        }
        # Sub-pass b: includes
        for (sas_file in new_files) {
          private$parse_include_stmts(sas_file)
        }
        # Sub-pass c: macro definitions
        for (sas_file in new_files) {
          private$parse_macro_defs(sas_file)
          parsed <- c(parsed, normalizePath(sas_file, mustWork = FALSE))
        }

        # Collect new include targets
        next_worklist <- character(0)
        seen_targets <- character(0)
        for (include_list in self$file_includes) {
          for (inc in include_list) {
            target <- inc$target
            if (is.null(target)) next
            target_key <- normalizePath(target, mustWork = FALSE)
            if (target_key %in% parsed || target_key %in% seen_targets) next
            if (!file.exists(target)) next
            seen_targets <- c(seen_targets, target_key)
            all_files <- c(all_files, target)
            next_worklist <- c(next_worklist, target)
          }
        }

        if (length(next_worklist) == 0L) break
        private$collect_static_let_values(next_worklist)
        worklist <- next_worklist
      }
      all_files
    },

    parse_filename_stmts = function(filepath) {
      result <- parse_filename_statements(filepath)
      for (nm in names(result$filename_refs)) {
        self$filename_refs[[nm]] <- result$filename_refs[[nm]]
      }
      filepath_key <- normalizePath(filepath, mustWork = FALSE)
      if (is.null(self$file_fileref_definitions[[filepath_key]])) {
        self$file_fileref_definitions[[filepath_key]] <- list()
      }
      self$file_fileref_definitions[[filepath_key]] <- c(
        self$file_fileref_definitions[[filepath_key]],
        result$file_fileref_definitions
      )
    },

    parse_macro_defs = function(filepath) {
      filepath_key <- normalizePath(filepath, mustWork = FALSE)
      definitions <- parse_macro_definitions(filepath)
      for (macro_def in definitions) {
        name <- macro_def$name
        existing <- self$macro_definitions[[name]]
        if (is.null(existing) || !.has_body(existing) || .has_body(macro_def)) {
          self$macro_definitions[[name]] <- macro_def
        }
        if (is.null(self$macro_definition_versions[[name]])) {
          self$macro_definition_versions[[name]] <- list()
        }
        self$macro_definition_versions[[name]] <- c(
          self$macro_definition_versions[[name]], list(macro_def)
        )
        if (is.null(self$file_macro_definitions[[filepath_key]])) {
          self$file_macro_definitions[[filepath_key]] <- list()
        }
        self$file_macro_definitions[[filepath_key]] <- c(
          self$file_macro_definitions[[filepath_key]], list(macro_def)
        )
      }
      if (length(definitions) > 0L) {
        self$.file_macro_exports_cache <- list()
      }
    },

    parse_include_stmts = function(filepath) {
      filepath_key <- normalizePath(filepath, mustWork = FALSE)
      includes <- parse_include_statements(
        filepath, self$file_fileref_definitions,
        macro_variables = self$macro_variables,
        search_roots = self$.include_search_roots,
        global_filename_refs = self$filename_refs
      )
      if (is.null(self$file_includes[[filepath_key]])) {
        self$file_includes[[filepath_key]] <- list()
      }
      self$file_includes[[filepath_key]] <- c(
        self$file_includes[[filepath_key]], includes
      )
      if (length(includes) > 0L) {
        self$.file_macro_exports_cache <- list()
      }
    },

    resolve_macro_def = function(macro_name, call_file, call_line) {
      resolve_macro_definition(
        macro_name, call_file, call_line,
        self$file_includes, self$file_macro_definitions,
        self$.file_macro_exports_cache
      )
    },

    expand_macro_call = function(macro_name, args, macro_def = NULL) {
      expand_macro(macro_name, args, self$macro_definitions, macro_def)
    },

    add_operation = function(operation) {
      self$dataset_operations <- c(self$dataset_operations, list(operation))
      ds <- tolower(operation$dataset)
      if (is.null(self$dataset_to_ops[[ds]])) {
        self$dataset_to_ops[[ds]] <- list()
      }
      self$dataset_to_ops[[ds]] <- c(self$dataset_to_ops[[ds]], list(operation))
    },

    handle_data_step = function(lines, i, filepath) {
      m <- regmatches(lines[[i]], regexec("^\\s*data\\s+([^;/(]+)", lines[[i]],
                                           perl = TRUE, ignore.case = TRUE))[[1]]
      if (length(m) < 2L) return(list(should_continue = FALSE, new_i = i))

      result <- parse_data_step(lines, i, filepath)
      if (!is.null(result)) {
        for (op in result$operations) {
          private$add_operation(op)
        }
        self$infile_usage <- c(self$infile_usage, result$infile_records)
        end_line <- result$operations[[1]]$end_line
        next_line <- if (end_line > i) end_line else i + 1L
        return(list(should_continue = TRUE, new_i = next_line))
      }
      list(should_continue = FALSE, new_i = i)
    },

    handle_proc_sql = function(lines, i, filepath) {
      if (!grepl("^\\s*proc\\s+sql\\b", lines[[i]], ignore.case = TRUE, perl = TRUE)) {
        return(list(should_continue = FALSE, new_i = i))
      }

      result <- parse_proc_sql_block(lines, i, filepath)
      for (op in result$operations) {
        private$add_operation(op)
        if (op$operation_type == "PROC SQL" && !startsWith(op$dataset, "mv:")) {
          self$.last_sql_output[[normalizePath(filepath, mustWork = FALSE)]] <- op$dataset
        }
      }
      list(should_continue = TRUE, new_i = result$end_line)
    },

    handle_proc_sort = function(lines, i, filepath) {
      if (!grepl("^\\s*proc\\s+sort\\s+data\\s*=\\s*", lines[[i]],
                  ignore.case = TRUE, perl = TRUE)) {
        return(FALSE)
      }
      result <- parse_proc_sort(lines, i, filepath)
      if (!is.null(result) && !is.null(result$dataset)) {
        private$add_operation(result)
      }
      TRUE
    },

    handle_proc_generic = function(lines, i, filepath) {
      m <- regmatches(lines[[i]], regexec(
        "^\\s*proc\\s+(transpose|append|means|summary|univariate|freq)\\b",
        lines[[i]], perl = TRUE, ignore.case = TRUE
      ))[[1]]
      if (length(m) < 2L) return(list(handled = FALSE, new_i = i))

      result <- parse_proc_generic(lines, i, filepath, m[2])
      if (!is.null(result) && !is.null(result$dataset)) {
        private$add_operation(result)
        next_idx <- max(result$end_line, i + 1L)
        return(list(handled = TRUE, new_i = next_idx))
      }
      list(handled = TRUE, new_i = i + 1L)
    },

    handle_proc_export = function(lines, i, filepath) {
      if (!grepl("^\\s*proc\\s+export\\b", lines[[i]], ignore.case = TRUE, perl = TRUE)) {
        return(list(should_continue = FALSE, new_i = i))
      }
      result <- parse_proc_export(lines, i, filepath)
      if (is.null(result)) return(list(should_continue = FALSE, new_i = i + 1L))
      private$add_operation(result$operation)
      list(should_continue = TRUE, new_i = result$end_idx + 1L)
    },

    handle_proc_format = function(lines, i, filepath) {
      if (!grepl("^\\s*proc\\s+format\\b", lines[[i]], ignore.case = TRUE, perl = TRUE)) {
        return(list(should_continue = FALSE, new_i = i))
      }
      result <- parse_proc_format(lines, i, filepath)
      for (op in result$operations) {
        private$add_operation(op)
      }
      list(should_continue = TRUE, new_i = result$end_idx + 1L)
    },

    handle_ods_tagsets_csv = function(lines, i, filepath) {
      if (!grepl(.ODS_TRIGGER_RE, lines[[i]], ignore.case = TRUE, perl = TRUE)) {
        return(list(should_continue = FALSE, new_i = i))
      }
      result <- parse_ods_tagsets_csv(
        lines, i, filepath, self$macro_definitions, self$filename_refs
      )
      if (is.null(result)) return(list(should_continue = FALSE, new_i = i))
      private$add_operation(result$operation)
      list(should_continue = TRUE, new_i = result$end_idx + 1L)
    },

    handle_let = function(line, line_num, filepath) {
      if (!grepl("^\\s*%let\\s+", line, ignore.case = TRUE, perl = TRUE)) return(FALSE)
      op <- parse_let_statement(line, filepath, line_num)
      if (is.null(op)) return(FALSE)
      op <- private$rewrite_sqlobs_input(op, filepath)
      private$add_operation(op)
      TRUE
    },

    rewrite_sqlobs_input = function(op, filepath) {
      if (!("mv:sqlobs" %in% op$input_datasets)) return(op)
      substitute <- self$.last_sql_output[[normalizePath(filepath, mustWork = FALSE)]]
      if (is.null(substitute)) return(op)
      new_inputs <- ifelse(op$input_datasets == "mv:sqlobs", substitute, op$input_datasets)
      replace_operation(op, input_datasets = new_inputs)
    },

    handle_macro_block = function(lines, i) {
      if (!grepl("^\\s*%macro\\s+", lines[[i]], ignore.case = TRUE, perl = TRUE)) {
        return(list(should_continue = FALSE, new_i = i))
      }
      end_idx <- find_macro_end(lines, i)
      if (end_idx < 0L) {
        return(list(should_continue = TRUE, new_i = length(lines) + 1L))
      }
      list(should_continue = TRUE, new_i = end_idx + 1L)
    },

    handle_macro_call = function(lines, i, filepath) {
      line <- lines[[i]]
      line_num <- i

      macro_call <- regmatches(line, regexec(
        "^\\s*%(\\w+)\\s*\\(([^)]*)\\)",
        line, perl = TRUE, ignore.case = TRUE
      ))[[1]]
      if (length(macro_call) < 2L) {
        macro_call <- regmatches(line, regexec(
          "^\\s*%(\\w+)\\s*;",
          line, perl = TRUE, ignore.case = TRUE
        ))[[1]]
      }
      if (length(macro_call) < 2L) return(FALSE)

      stripped <- trimws(tolower(line))
      if (startsWith(stripped, "%macro") || startsWith(stripped, "%mend")) return(FALSE)

      macro_name <- tolower(macro_call[2])
      if (macro_name %in% c("if", "do", "else", "end", "let", "put", "include")) return(FALSE)

      args <- character(0)
      if (length(macro_call) >= 3L && nzchar(macro_call[3])) {
        args <- trimws(strsplit(macro_call[3], ",")[[1]])
        args <- args[nzchar(args)]
      }

      self$macro_calls <- c(self$macro_calls, list(list(macro_name, filepath, line_num, trimws(line))))

      resolved_macro_def <- private$resolve_macro_def(macro_name, filepath, line_num)
      expanded <- private$expand_macro_call(macro_name, args, macro_def = resolved_macro_def)

      if (length(expanded$lines) > 0L && !is.null(resolved_macro_def)) {
        private$parse_expanded_macro_lines(
          expanded$lines, expanded$source_lines,
          line_num, filepath,
          macro_name = macro_name,
          macro_def_file = resolved_macro_def$file
        )
        private$collect_proc_exports_from_macro(
          resolved_macro_def,
          call_line = line_num, call_file = filepath,
          macro_name = macro_name
        )
      }
      TRUE
    },

    parse_expanded_macro_lines = function(expanded_lines, expanded_source_lines,
                                           call_line, call_file,
                                           macro_name, macro_def_file = NULL,
                                           depth = 0L, seen_macros = character(0)) {
      to_source_line <- function(one_indexed) {
        if (is.null(expanded_source_lines) || length(expanded_source_lines) == 0L) return(NULL)
        idx <- min(max(one_indexed, 1L), length(expanded_source_lines))
        expanded_source_lines[idx]
      }

      fix_op <- function(operation) {
        replace_operation(
          operation,
          line_number = call_line,
          end_line = call_line,
          macro_name = macro_name,
          macro_source_file = macro_def_file,
          macro_source_line = to_source_line(operation$line_number),
          macro_end_line = to_source_line(operation$end_line)
        )
      }

      exp_idx <- 1L
      while (exp_idx <= length(expanded_lines)) {
        exp_line <- expanded_lines[exp_idx]

        # Skip nested macro DEFINITIONS
        if (grepl("^\\s*%macro\\s+\\w+", exp_line, ignore.case = TRUE, perl = TRUE)) {
          nested_end <- find_macro_end(expanded_lines, exp_idx)
          if (nested_end < 0L) break
          exp_idx <- nested_end + 1L
          next
        }

        # DATA step
        if (grepl("^\\s*data\\s+[^;/(]+", exp_line, ignore.case = TRUE, perl = TRUE)) {
          result <- parse_data_step(expanded_lines, exp_idx, call_file)
          if (!is.null(result)) {
            for (op in result$operations) {
              private$add_operation(fix_op(op))
            }
            self$infile_usage <- c(self$infile_usage, result$infile_records)
            exp_idx <- result$operations[[length(result$operations)]]$end_line + 1L
            next
          }
        }

        # PROC SQL
        if (grepl("^\\s*proc\\s+sql\\b", exp_line, ignore.case = TRUE, perl = TRUE)) {
          result <- parse_proc_sql_block(expanded_lines, exp_idx, call_file)
          for (op in result$operations) {
            private$add_operation(fix_op(op))
          }
          exp_idx <- result$end_line + 1L
          next
        }

        # PROC SORT
        if (grepl("^\\s*proc\\s+sort\\s+data\\s*=\\s*", exp_line,
                   ignore.case = TRUE, perl = TRUE)) {
          sort_op <- parse_proc_sort(expanded_lines, exp_idx, call_file)
          if (!is.null(sort_op) && !is.null(sort_op$dataset)) {
            private$add_operation(fix_op(sort_op))
          }
        }

        # PROC FORMAT
        if (grepl("^\\s*proc\\s+format\\b", exp_line, ignore.case = TRUE, perl = TRUE)) {
          result <- parse_proc_format(expanded_lines, exp_idx, call_file)
          for (op in result$operations) {
            private$add_operation(fix_op(op))
          }
          exp_idx <- result$end_idx + 1L
          next
        }

        # Generic PROC
        gm <- regmatches(exp_line, regexec(
          "^\\s*proc\\s+(transpose|append|means|summary|univariate|freq)\\b",
          exp_line, perl = TRUE, ignore.case = TRUE
        ))[[1]]
        if (length(gm) >= 2L) {
          generic_op <- parse_proc_generic(expanded_lines, exp_idx, call_file, gm[2])
          if (!is.null(generic_op) && !is.null(generic_op$dataset)) {
            private$add_operation(fix_op(generic_op))
          }
        }

        # ODS CSV
        if (grepl(.ODS_TRIGGER_RE, exp_line, ignore.case = TRUE, perl = TRUE)) {
          result <- parse_ods_tagsets_csv(
            expanded_lines, exp_idx, call_file,
            self$macro_definitions, self$filename_refs
          )
          if (!is.null(result)) {
            private$add_operation(fix_op(result$operation))
            exp_idx <- result$end_idx + 1L
            next
          }
        }

        # %let
        let_op <- parse_let_statement(exp_line, call_file, exp_idx)
        if (!is.null(let_op)) {
          private$add_operation(fix_op(let_op))
          exp_idx <- exp_idx + 1L
          next
        }

        # Nested macro CALL
        if (depth < .MAX_EXPANSION_DEPTH) {
          inner_match <- regmatches(exp_line, regexec(
            .MACRO_CALL_WITH_PARENS_RE, exp_line, perl = TRUE, ignore.case = TRUE
          ))[[1]]
          if (length(inner_match) < 2L) {
            inner_match <- regmatches(exp_line, regexec(
              .MACRO_CALL_BARE_RE, exp_line, perl = TRUE, ignore.case = TRUE
            ))[[1]]
          }
          if (length(inner_match) >= 2L) {
            inner_name <- tolower(inner_match[2])
            if (!(inner_name %in% .MACRO_CTRL_FLOW) &&
                !(inner_name %in% seen_macros) &&
                !is.null(self$macro_definitions[[inner_name]])) {
              inner_args <- character(0)
              if (length(inner_match) >= 3L && nzchar(inner_match[3])) {
                inner_args <- trimws(strsplit(inner_match[3], ",")[[1]])
                inner_args <- inner_args[nzchar(inner_args)]
              }
              inner_def <- private$resolve_macro_def(
                inner_name,
                macro_def_file %||% call_file,
                to_source_line(exp_idx) %||% 0L
              )
              if (is.null(inner_def)) inner_def <- self$macro_definitions[[inner_name]]

              inner_expanded <- expand_macro(
                inner_name, inner_args, self$macro_definitions,
                macro_def = inner_def
              )
              if (length(inner_expanded$lines) > 0L) {
                private$parse_expanded_macro_lines(
                  inner_expanded$lines, inner_expanded$source_lines,
                  to_source_line(exp_idx),
                  call_file,
                  macro_name = inner_name,
                  macro_def_file = inner_def$file,
                  depth = depth + 1L,
                  seen_macros = c(seen_macros, inner_name)
                )
                private$collect_proc_exports_from_macro(
                  inner_def,
                  call_line = to_source_line(exp_idx),
                  call_file = call_file,
                  macro_name = inner_name
                )
              }
            }
          }
        }
        exp_idx <- exp_idx + 1L
      }
    },

    collect_proc_exports_from_macro = function(macro_def, call_line, call_file, macro_name) {
      body <- macro_def$body %||% character(0)
      body_source_lines <- macro_def$body_source_lines %||% integer(0)
      macro_file <- macro_def$file

      i <- 1L
      while (i <= length(body)) {
        if (!grepl("^\\s*proc\\s+export\\b", body[i], ignore.case = TRUE, perl = TRUE)) {
          i <- i + 1L
          next
        }
        result <- parse_proc_export(body, i, call_file)
        if (is.null(result)) {
          i <- i + 1L
          next
        }
        source_line <- if (i <= length(body_source_lines)) body_source_lines[i] else NULL
        end_source_line <- if (result$end_idx <= length(body_source_lines)) {
          body_source_lines[result$end_idx]
        } else {
          source_line
        }
        private$add_operation(replace_operation(
          result$operation,
          line_number = call_line,
          end_line = call_line,
          macro_name = macro_name,
          macro_source_file = macro_file,
          macro_source_line = source_line,
          macro_end_line = end_source_line
        ))
        i <- result$end_idx + 1L
      }
    },

    expand_uncalled_top_level_macros = function() {
      called_macros <- character(0)
      for (call in self$macro_calls) {
        called_macros <- c(called_macros, tolower(call[[1]]))
      }
      called_macros <- unique(called_macros)

      nested <- private$build_nested_macro_line_index()

      for (filepath_key in names(self$file_macro_definitions)) {
        definitions <- self$file_macro_definitions[[filepath_key]]
        top_level_defs <- Filter(function(d) {
          key <- paste(filepath_key, d$line, sep = "|")
          !(key %in% nested)
        }, definitions)

        if (length(top_level_defs) != 1L) next

        macro_def <- top_level_defs[[1]]
        name <- tolower(macro_def$name)
        if (name %in% called_macros) next
        if (!.has_body(macro_def)) next

        args <- rep("", length(macro_def$params %||% character(0)))
        expanded <- private$expand_macro_call(name, args, macro_def = macro_def)
        if (length(expanded$lines) == 0L) next

        private$parse_expanded_macro_lines(
          expanded$lines, expanded$source_lines,
          macro_def$line, filepath_key,
          macro_name = name,
          macro_def_file = macro_def$file
        )
        private$collect_proc_exports_from_macro(
          macro_def,
          call_line = macro_def$line,
          call_file = filepath_key,
          macro_name = name
        )
      }
    },

    build_nested_macro_line_index = function() {
      nested <- character(0)
      for (filepath_key in names(self$file_macro_definitions)) {
        definitions <- self$file_macro_definitions[[filepath_key]]
        for (outer in definitions) {
          outer_line <- outer$line
          outer_end <- private$macro_end_line(filepath_key, outer_line)
          if (is.null(outer_end)) next
          for (inner in definitions) {
            if (identical(inner, outer)) next
            if (outer_line < inner$line && inner$line <= outer_end) {
              nested <- c(nested, paste(filepath_key, inner$line, sep = "|"))
            }
          }
        }
      }
      unique(nested)
    },

    macro_end_line = function(filepath, macro_start_line) {
      tryCatch({
        file_lines <- readLines(filepath, encoding = "latin1", warn = FALSE)
        if (macro_start_line < 1L || macro_start_line > length(file_lines)) return(NULL)
        end_idx <- find_macro_end(file_lines, macro_start_line)
        if (end_idx < 0L) return(NULL)
        end_idx
      }, error = function(e) NULL)
    },

    resolve_fileref_alias = function(target_dataset) {
      if (!is.null(self$dataset_to_ops[[target_dataset]])) {
        return(target_dataset)
      }
      ref_info <- self$filename_refs[[normalize_fileref(target_dataset)]]
      if (is.null(ref_info)) return(target_dataset)
      canonical <- path_to_dataset_name(ref_info[[3]])
      if (!is.null(canonical) && !is.null(self$dataset_to_ops[[canonical]])) {
        return(canonical)
      }
      target_dataset
    }
  )
)
