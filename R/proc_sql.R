# proc_sql.R — PROC SQL tokenizer + parser (ported from parsers/proc_sql.py)

.FROM_TERMINATOR_KEYWORDS <- c(
  "where", "group", "order", "having", "union", "except", "intersect",
  "qualify", "window", "select"
)
.JOIN_KEYWORDS <- c(
  "inner", "left", "right", "full", "outer", "natural", "cross", "join", "on"
)
.TABLE_LIST_END_KEYWORDS <- c(.FROM_TERMINATOR_KEYWORDS, .JOIN_KEYWORDS)

.IDENT_RE_SQL <- "[A-Za-z_]\\w*"

# Tokenizer — returns a data.frame with columns: kind, value, start, end, depth
.scan_tokens <- function(buffer) {
  tokens <- list()
  i <- 1L
  n <- nchar(buffer)
  depth <- 0L

  while (i <= n) {
    c <- substr(buffer, i, i)

    # Skip string literals
    if (c == "'" || c == "\"") {
      quote <- c
      i <- i + 1L
      while (i <= n && substr(buffer, i, i) != quote) {
        i <- i + 1L
      }
      i <- i + 1L
      next
    }

    if (c == "(") {
      depth <- depth + 1L
      tokens <- c(tokens, list(list(kind = "open", value = "(", start = i, end = i + 1L, depth = depth)))
      i <- i + 1L
      next
    }
    if (c == ")") {
      depth <- depth - 1L
      tokens <- c(tokens, list(list(kind = "close", value = ")", start = i, end = i + 1L, depth = depth)))
      i <- i + 1L
      next
    }
    if (c == ",") {
      tokens <- c(tokens, list(list(kind = "comma", value = ",", start = i, end = i + 1L, depth = depth)))
      i <- i + 1L
      next
    }
    if (c == ";") {
      tokens <- c(tokens, list(list(kind = "semi", value = ";", start = i, end = i + 1L, depth = depth)))
      i <- i + 1L
      next
    }

    # Identifier
    m <- regexpr(paste0("^", .IDENT_RE_SQL), substr(buffer, i, n), perl = TRUE)
    if (m > 0L) {
      len <- attr(m, "match.length")
      value <- tolower(substr(buffer, i, i + len - 1L))
      tokens <- c(tokens, list(list(kind = "ident", value = value, start = i, end = i + len, depth = depth)))
      i <- i + len
      next
    }

    i <- i + 1L
  }
  tokens
}

.extract_table_token <- function(buffer, start, end) {
  chunk <- trimws(substr(buffer, start, end - 1L))
  if (!nzchar(chunk)) return(NULL)
  if (startsWith(chunk, "(")) return(NULL)
  first_token <- strsplit(chunk, "\\s+", perl = TRUE)[[1]][1]
  clean_dataset_name(first_token)
}

#' Extract table names from FROM and JOIN clauses
#' @export
extract_sql_tables <- function(buffer) {
  datasets <- character(0)
  tokens <- .scan_tokens(buffer)

  open_contexts <- list()

  close_top <- function(end_idx) {
    ctx <- open_contexts[[length(open_contexts)]]
    open_contexts[[length(open_contexts)]] <<- NULL
    if (is.null(ctx$list_start)) return()
    bounds <- c(ctx$list_start, ctx$commas, end_idx)
    for (idx in seq_len(length(bounds) - 1L)) {
      lo <- bounds[idx]
      if (idx > 1L) lo <- lo + 1L
      hi <- bounds[idx + 1L]
      ds <- .extract_table_token(buffer, lo, hi)
      if (!is.null(ds)) {
        datasets <<- c(datasets, ds)
      }
    }
  }

  expect_table_for <- NULL

  for (tok in tokens) {
    kind <- tok$kind
    value <- tok$value
    start <- tok$start
    end <- tok$end
    depth_after <- tok$depth

    if (kind == "ident") {
      if (value == "from") {
        expect_table_for <- list(
          kind = "from", depth = depth_after,
          list_start = NULL, commas = integer(0)
        )
        open_contexts <- c(open_contexts, list(expect_table_for))
        next
      }
      if (value == "join") {
        expect_table_for <- list(
          kind = "join", depth = depth_after,
          list_start = NULL, commas = integer(0)
        )
        open_contexts <- c(open_contexts, list(expect_table_for))
        next
      }
      if (value %in% .TABLE_LIST_END_KEYWORDS && length(open_contexts) > 0L) {
        top <- open_contexts[[length(open_contexts)]]
        if (top$depth == depth_after) {
          close_top(start)
          expect_table_for <- NULL
        }
        next
      }
      if (length(open_contexts) > 0L) {
        top <- open_contexts[[length(open_contexts)]]
        if (top$depth == depth_after && is.null(top$list_start)) {
          open_contexts[[length(open_contexts)]]$list_start <- start
        }
        if (top$kind == "join" && top$depth == depth_after) {
          close_top(end)
          expect_table_for <- NULL
        }
      }
      next
    }

    if (kind == "open") {
      if (length(open_contexts) > 0L) {
        top <- open_contexts[[length(open_contexts)]]
        if (top$depth == depth_after - 1L && is.null(top$list_start)) {
          open_contexts[[length(open_contexts)]]$list_start <- start
        }
      }
      next
    }

    if (kind == "close") {
      while (length(open_contexts) > 0L && open_contexts[[length(open_contexts)]]$depth > depth_after) {
        close_top(start)
      }
      expect_table_for <- NULL
      next
    }

    if (kind == "comma") {
      if (length(open_contexts) > 0L) {
        top <- open_contexts[[length(open_contexts)]]
        if (top$kind == "from" && top$depth == depth_after) {
          open_contexts[[length(open_contexts)]]$commas <- c(top$commas, start)
        }
      }
      next
    }

    if (kind == "semi") {
      while (length(open_contexts) > 0L) {
        close_top(start)
      }
      expect_table_for <- NULL
      next
    }
  }

  # EOF
  while (length(open_contexts) > 0L) {
    close_top(nchar(buffer) + 1L)
  }

  datasets
}

#' Collect input tables from a single SQL statement
#' @export
collect_sql_statement_inputs <- function(lines, start_idx) {
  code_lines <- character(0)
  i <- start_idx

  while (i <= length(lines)) {
    current_line <- lines[[i]]
    code_lines <- c(code_lines, current_line)

    line_no_macro <- gsub("%do\\s*;", "", current_line, ignore.case = TRUE, perl = TRUE)
    line_no_macro <- gsub("%end\\s*;", "", line_no_macro, ignore.case = TRUE, perl = TRUE)
    line_no_macro <- gsub("%else\\s+%do\\s*;", "", line_no_macro, ignore.case = TRUE, perl = TRUE)
    if (i == start_idx) {
      line_no_macro <- sub("^\\s*proc\\s+sql\\b[^;]*;\\s*", "", line_no_macro,
                            ignore.case = TRUE, perl = TRUE)
    }
    if (grepl(";", line_no_macro, fixed = TRUE)) break

    i <- i + 1L
    if (i - start_idx > 50L) break
  }

  buffer <- paste0(code_lines, collapse = " ")
  buffer <- sub("^\\s*proc\\s+sql\\b[^;]*;\\s*", "", buffer, ignore.case = TRUE, perl = TRUE)
  input_datasets <- extract_sql_tables(buffer)

  for (mv_name in scan_macro_var_refs(code_lines)) {
    input_datasets <- c(input_datasets, paste0("mv:", mv_name))
  }
  for (fmt_name in scan_format_refs(code_lines)) {
    input_datasets <- c(input_datasets, paste0("fmt:", fmt_name))
  }

  input_datasets <- deduplicate_list(input_datasets)

  list(input_datasets = input_datasets, code_lines = code_lines, stmt_end_idx = i)
}

#' Parse a single PROC SQL CREATE TABLE statement
#' @export
parse_proc_sql <- function(lines, start_idx, filepath) {
  line <- lines[[start_idx]]

  sql_match <- regmatches(line, regexec(
    "^\\s*create\\s+table\\s+([^\\s(]+)",
    line, perl = TRUE, ignore.case = TRUE
  ))[[1]]
  if (length(sql_match) < 2L) return(NULL)

  output_ds <- clean_dataset_name(sql_match[2])

  result <- collect_sql_statement_inputs(lines, start_idx)
  code_snippet <- paste0(result$code_lines[seq_len(min(500L, length(result$code_lines)))],
                          collapse = "")

  new_operation(
    dataset        = output_ds,
    operation_type = "PROC SQL",
    file           = as.character(filepath),
    line_number    = start_idx,
    code_snippet   = code_snippet,
    input_datasets = result$input_datasets,
    end_line       = result$stmt_end_idx
  )
}

.collect_select_into_targets <- function(lines, start_idx) {
  targets <- character(0)
  i <- start_idx
  buffer_parts <- character(0)

  while (i <= length(lines)) {
    current_line <- lines[[i]]
    buffer_parts <- c(buffer_parts, current_line)

    line_no_macro <- gsub("%(?:do|end|else\\s+%do)\\s*;", "", current_line,
                          ignore.case = TRUE, perl = TRUE)
    if (i == start_idx) {
      line_no_macro <- sub("^\\s*proc\\s+sql\\b[^;]*;\\s*", "", line_no_macro,
                            ignore.case = TRUE, perl = TRUE)
    }
    if (grepl(";", line_no_macro, fixed = TRUE)) break
    i <- i + 1L
    if (i - start_idx > 50L) break
  }

  buffer <- paste0(buffer_parts, collapse = " ")
  buffer <- sub("^\\s*proc\\s+sql\\b[^;]*;\\s*", "", buffer, ignore.case = TRUE, perl = TRUE)

  # Extract :varname targets
  matches <- gregexpr(":([A-Za-z_]\\w*)", buffer, perl = TRUE)
  if (matches[[1]][1] != -1L) {
    all_matches <- regmatches(buffer, matches)[[1]]
    for (full_match in all_matches) {
      m <- regmatches(full_match, regexec(":([A-Za-z_]\\w*)", full_match, perl = TRUE))[[1]]
      if (length(m) >= 2L) {
        name <- tolower(m[2])
        if (!(name %in% targets)) targets <- c(targets, name)
      }
    }
  }

  inputs <- deduplicate_list(extract_sql_tables(buffer))
  list(targets = targets, inputs = inputs, stmt_end_idx = i)
}

#' Parse a complete PROC SQL block
#' @export
parse_proc_sql_block <- function(lines, start_idx, filepath) {
  proc_sql_line <- start_idx
  operations <- list()
  i <- start_idx
  last_created_output <- NULL

  proc_sql_prefix <- "(?:proc\\s+sql\\b[^;]*;\\s*)?"

  while (i <= length(lines)) {
    current_line <- lines[[i]]

    if (grepl("^\\s*quit\\s*;", current_line, ignore.case = TRUE, perl = TRUE)) break

    if (i != proc_sql_line && grepl("^\\s*(data|proc)\\s+", current_line,
                                     ignore.case = TRUE, perl = TRUE)) {
      i <- i - 1L
      break
    }

    # %let var = &sqlobs;
    sqlobs_match <- regmatches(current_line, regexec(
      "^\\s*%let\\s+([A-Za-z_]\\w*)\\s*=\\s*&sqlobs\\b\\s*;",
      current_line, perl = TRUE, ignore.case = TRUE
    ))[[1]]
    if (length(sqlobs_match) >= 2L && !is.null(last_created_output)) {
      target_var <- tolower(sqlobs_match[2])
      operations <- c(operations, list(new_operation(
        dataset        = paste0("mv:", target_var),
        operation_type = "MACRO LET",
        file           = as.character(filepath),
        line_number    = i,
        code_snippet   = trimws(current_line, which = "right"),
        input_datasets = last_created_output,
        end_line       = i
      )))
      i <- i + 1L
      next
    }

    # CREATE TABLE
    create_match <- regmatches(current_line, regexec(
      paste0("^\\s*", proc_sql_prefix, "create\\s+table\\s+([^\\s(]+)"),
      current_line, perl = TRUE, ignore.case = TRUE
    ))[[1]]
    if (length(create_match) >= 2L) {
      output_ds <- clean_dataset_name(create_match[2])
      if (!is.null(output_ds)) {
        result <- collect_sql_statement_inputs(lines, i)
        operations <- c(operations, list(new_operation(
          dataset        = output_ds,
          operation_type = "PROC SQL",
          file           = as.character(filepath),
          line_number    = i,
          code_snippet   = "",
          input_datasets = result$input_datasets,
          end_line       = result$stmt_end_idx
        )))
        last_created_output <- output_ds
        i <- result$stmt_end_idx + 1L
        next
      }
    }

    # SELECT ... INTO :var
    if (grepl(paste0("^\\s*", proc_sql_prefix, "select\\b"), current_line,
              ignore.case = TRUE, perl = TRUE)) {
      lookahead <- paste0(lines[i:min(i + 5L, length(lines))], collapse = " ")
      if (grepl("\\binto\\s*:", lookahead, ignore.case = TRUE, perl = TRUE)) {
        stmt_line <- i
        result <- .collect_select_into_targets(lines, i)
        for (var in result$targets) {
          operations <- c(operations, list(new_operation(
            dataset        = paste0("mv:", var),
            operation_type = "PROC SQL INTO",
            file           = as.character(filepath),
            line_number    = stmt_line,
            code_snippet   = "",
            input_datasets = result$inputs,
            end_line       = result$stmt_end_idx
          )))
        }
        i <- result$stmt_end_idx + 1L
        next
      }
    }

    i <- i + 1L
  }

  if (i > length(lines)) {
    end_line <- length(lines)
  } else {
    end_line <- i
  }

  block_lines <- lines[proc_sql_line:end_line]
  code_snippet <- paste0(block_lines[seq_len(min(500L, length(block_lines)))], collapse = "")

  operations <- lapply(operations, function(op) {
    replace_operation(op, code_snippet = code_snippet)
  })

  list(operations = operations, end_line = end_line)
}
