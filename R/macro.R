# macro.R — Macro definition, expansion, and resolution (ported from parsers/macro.py)

.LET_STMT_RE <- "^\\s*%let\\s+([A-Za-z_]\\w*)\\s*=\\s*([^;]*);"
.LET_VALUE_REF_RE <- "&{1,2}([A-Za-z_]\\w*)"

.MACRO_OPEN_TOKEN_RE <- "%macro\\s+\\w+"
.MEND_TOKEN_RE <- "%mend\\b"
.INLINE_EMPTY_MACRO_RE <- "%macro\\s+\\w+[^;]*;\\s*%mend"
.MACRO_HEAD_RE <- "^\\s*%macro\\s+\\w+"

#' Find matching %mend for %macro at start_idx (1-based)
#' @return 1-based line of %mend, or -1
#' @export
find_macro_end <- function(lines, start_idx) {
  head <- lines[[start_idx]]
  if (grepl(.INLINE_EMPTY_MACRO_RE, head, ignore.case = TRUE, perl = TRUE)) {
    return(start_idx)
  }

  depth <- 1L
  i <- start_idx + 1L
  while (i <= length(lines)) {
    result <- handle_block_comments(lines, i)
    if (result$should_continue) {
      if (!is.null(result$replacement)) lines[[result$new_i]] <- result$replacement
      i <- result$new_i
      next
    }
    line <- lines[[i]]
    opens <- length(gregexpr(.MACRO_OPEN_TOKEN_RE, line, ignore.case = TRUE, perl = TRUE)[[1]])
    if (gregexpr(.MACRO_OPEN_TOKEN_RE, line, ignore.case = TRUE, perl = TRUE)[[1]][1] == -1L) opens <- 0L
    closes <- length(gregexpr(.MEND_TOKEN_RE, line, ignore.case = TRUE, perl = TRUE)[[1]])
    if (gregexpr(.MEND_TOKEN_RE, line, ignore.case = TRUE, perl = TRUE)[[1]][1] == -1L) closes <- 0L
    depth <- depth + opens - closes
    if (depth <= 0L) return(i)
    i <- i + 1L
  }
  -1L
}

#' Parse a %let var = value; statement
#' @export
parse_let_statement <- function(line, filepath, line_num) {
  m <- regmatches(line, regexec(.LET_STMT_RE, line, perl = TRUE, ignore.case = TRUE))[[1]]
  if (length(m) < 3L) return(NULL)

  var <- tolower(m[2])
  value <- m[3]

  inputs <- character(0)
  seen <- character(0)
  refs <- gregexpr(.LET_VALUE_REF_RE, value, perl = TRUE)
  if (refs[[1]][1] != -1L) {
    all_matches <- regmatches(value, refs)[[1]]
    for (full_match in all_matches) {
      ref_m <- regmatches(full_match, regexec(.LET_VALUE_REF_RE, full_match, perl = TRUE))[[1]]
      if (length(ref_m) >= 2L) {
        name <- tolower(ref_m[2])
        if (name %in% seen) next
        seen <- c(seen, name)
        inputs <- c(inputs, paste0("mv:", name))
      }
    }
  }

  new_operation(
    dataset        = paste0("mv:", var),
    operation_type = "MACRO LET",
    file           = as.character(filepath),
    line_number    = line_num,
    code_snippet   = sub("\n$", "", line),
    input_datasets = inputs,
    end_line       = line_num
  )
}

.MACRO_DEF_WITH_PARAMS_RE <- "^\\s*%macro\\s+(\\w+)\\s*\\(([^)]*)\\)"
.MACRO_DEF_BARE_RE <- "^\\s*%macro\\s+(\\w+)\\s*;"

#' Parse macro definitions from a SAS file
#' @export
parse_macro_definitions <- function(filepath) {
  filepath <- normalizePath(filepath, mustWork = FALSE)
  lines <- readLines(filepath, encoding = "latin1", warn = FALSE)

  definitions <- list()
  i <- 1L
  while (i <= length(lines)) {
    result <- handle_block_comments(lines, i)
    if (result$should_continue) {
      if (!is.null(result$replacement)) lines[[result$new_i]] <- result$replacement
      i <- result$new_i
      next
    }

    macro_def <- .try_parse_macro_definition(lines, i, filepath)
    if (!is.null(macro_def)) {
      definitions <- c(definitions, list(macro_def))
    }
    i <- i + 1L
  }
  definitions
}

.try_parse_macro_definition <- function(lines, start_idx, filepath) {
  line <- lines[[start_idx]]

  m <- regmatches(line, regexec(.MACRO_DEF_WITH_PARAMS_RE, line,
                                 perl = TRUE, ignore.case = TRUE))[[1]]
  has_params <- length(m) >= 3L
  if (!has_params) {
    m <- regmatches(line, regexec(.MACRO_DEF_BARE_RE, line,
                                   perl = TRUE, ignore.case = TRUE))[[1]]
  }
  if (length(m) < 2L) return(NULL)

  macro_name <- tolower(m[2])
  params <- character(0)
  if (has_params && length(m) >= 3L && nzchar(m[3])) {
    params <- trimws(strsplit(m[3], ",")[[1]])
    params <- params[nzchar(params)]
  }

  end_idx <- find_macro_end(lines, start_idx)
  body_lines <- character(0)
  body_source_lines <- integer(0)
  if (end_idx > start_idx) {
    j <- start_idx + 1L
    while (j < end_idx) {
      result <- handle_block_comments(lines, j)
      if (result$should_continue) {
        if (!is.null(result$replacement)) lines[[result$new_i]] <- result$replacement
        j <- result$new_i
        next
      }
      body_lines <- c(body_lines, lines[[j]])
      body_source_lines <- c(body_source_lines, j)
      j <- j + 1L
    }
  }

  list(
    name              = macro_name,
    params            = params,
    body              = body_lines,
    body_source_lines = as.integer(body_source_lines),
    file              = filepath,
    line              = as.integer(start_idx)
  )
}

#' Expand a macro call with given arguments
#' @export
expand_macro <- function(macro_name, args, macro_definitions, macro_def = NULL) {
  if (is.null(macro_def)) {
    key <- tolower(macro_name)
    if (is.null(macro_definitions[[key]])) return(list(lines = character(0), source_lines = integer(0)))
    macro_def <- macro_definitions[[key]]
  }

  params <- macro_def$params %||% character(0)
  body <- macro_def$body %||% character(0)
  body_source_lines <- macro_def$body_source_lines
  if (is.null(body_source_lines) || length(body_source_lines) == 0L) {
    base_line <- macro_def$line %||% 1L
    body_source_lines <- as.integer(base_line + seq_along(body))
  }

  # Build substitutions map
  param_lower_to_canonical <- setNames(params, tolower(params))
  substitutions <- setNames(rep("", length(params)), params)
  bound_by_name <- character(0)
  positional_index <- 1L

  for (raw_arg in args) {
    eq_idx <- regexpr("=", raw_arg, fixed = TRUE)
    if (eq_idx > 1L) {
      name_candidate <- trimws(substr(raw_arg, 1, eq_idx - 1L))
      if (nzchar(name_candidate) &&
          grepl("^[A-Za-z_]\\w*$", name_candidate, perl = TRUE) &&
          tolower(name_candidate) %in% names(param_lower_to_canonical)) {
        canonical <- param_lower_to_canonical[[tolower(name_candidate)]]
        substitutions[[canonical]] <- trimws(substr(raw_arg, eq_idx + 1L, nchar(raw_arg)))
        bound_by_name <- c(bound_by_name, canonical)
        next
      }
    }
    # Positional binding
    while (positional_index <= length(params) && params[positional_index] %in% bound_by_name) {
      positional_index <- positional_index + 1L
    }
    if (positional_index <= length(params)) {
      substitutions[[params[positional_index]]] <- raw_arg
      positional_index <- positional_index + 1L
    }
  }

  # Default values for common params
  if (!("an" %in% names(substitutions))) substitutions[["an"]] <- ""
  if (!("champ" %in% names(substitutions))) substitutions[["champ"]] <- "MCO"

  expanded_lines <- character(length(body))
  for (idx in seq_along(body)) {
    expanded_line <- body[idx]
    for (param in names(substitutions)) {
      value <- substitutions[[param]]
      expanded_line <- gsub(
        paste0("&", param, "\\."),
        value, expanded_line, ignore.case = TRUE, perl = TRUE
      )
      expanded_line <- gsub(
        paste0("&", param, "\\b"),
        value, expanded_line, ignore.case = TRUE, perl = TRUE
      )
    }
    expanded_lines[idx] <- expanded_line
  }

  unroll_do_loops(expanded_lines, body_source_lines)
}

.DO_LOOP_OPEN_RE <- "%do\\s+(\\w+)\\s*=\\s*(\\d+)\\s*%to\\s*(\\d+)\\s*;"
.DO_RE <- "%do\\b"
.END_RE <- "%end\\b"

#' Unroll simple %do var=A %to B loops with integer bounds
#' @export
unroll_do_loops <- function(lines, source_lines, max_iterations = 200L) {
  out_lines <- character(0)
  out_sources <- integer(0)
  i <- 1L

  while (i <= length(lines)) {
    line <- lines[i]
    m <- regmatches(line, regexec(.DO_LOOP_OPEN_RE, line, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(m) < 4L) {
      out_lines <- c(out_lines, line)
      out_sources <- c(out_sources, source_lines[i])
      i <- i + 1L
      next
    }

    var <- m[2]
    lo <- as.integer(m[3])
    hi <- as.integer(m[4])

    depth <- 1L
    j <- i + 1L
    while (j <= length(lines)) {
      opens <- length(gregexpr(.DO_RE, lines[j], ignore.case = TRUE, perl = TRUE)[[1]])
      if (gregexpr(.DO_RE, lines[j], ignore.case = TRUE, perl = TRUE)[[1]][1] == -1L) opens <- 0L
      closes <- length(gregexpr(.END_RE, lines[j], ignore.case = TRUE, perl = TRUE)[[1]])
      if (gregexpr(.END_RE, lines[j], ignore.case = TRUE, perl = TRUE)[[1]][1] == -1L) closes <- 0L
      depth <- depth + opens - closes
      if (depth == 0L) break
      j <- j + 1L
    }

    if (j > length(lines) || hi - lo + 1L > max_iterations) {
      out_lines <- c(out_lines, line)
      out_sources <- c(out_sources, source_lines[i])
      i <- i + 1L
      next
    }

    body <- lines[(i + 1L):(j - 1L)]
    body_sources <- source_lines[(i + 1L):(j - 1L)]
    for (k in seq.int(lo, hi)) {
      substituted_body <- character(length(body))
      for (b_idx in seq_along(body)) {
        substituted <- gsub(
          paste0("&", var, "\\."),
          as.character(k), body[b_idx], ignore.case = TRUE, perl = TRUE
        )
        substituted <- gsub(
          paste0("&", var, "\\b"),
          as.character(k), substituted, ignore.case = TRUE, perl = TRUE
        )
        substituted_body[b_idx] <- substituted
      }
      inner <- unroll_do_loops(substituted_body, body_sources, max_iterations)
      out_lines <- c(out_lines, inner$lines)
      out_sources <- c(out_sources, inner$source_lines)
    }
    i <- j + 1L
  }

  list(lines = out_lines, source_lines = out_sources)
}

#' Resolve macro definition visible at call site using event order
#' @export
resolve_macro_definition <- function(macro_name, call_file, call_line,
                                      file_includes, file_macro_definitions,
                                      file_macro_exports_cache) {
  macro_name <- tolower(macro_name)
  call_file <- normalizePath(call_file, mustWork = FALSE)
  current_def <- NULL

  events <- get_file_events(call_file, file_includes, file_macro_definitions,
                             max_line = call_line)
  for (event in events) {
    if (event$kind == "macro_def") {
      if (event$macro_name == macro_name) {
        current_def <- event$macro_def
      }
    } else if (event$kind == "include") {
      included_exports <- get_exported_macros_for_file(
        event$target, file_macro_exports_cache,
        file_includes, file_macro_definitions
      )
      if (!is.null(included_exports[[macro_name]])) {
        current_def <- included_exports[[macro_name]]
      }
    }
  }
  current_def
}
