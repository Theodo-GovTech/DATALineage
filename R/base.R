# base.R — Operation factory, utility functions (ported from parsers/base.py)

# Input (source) libraries — tables from these are external data, not produced outputs
INPUT_LIBS <- c(
  "all", "all1", "nom_gen", "hadoqn", "hadoqn1",
  "nom_pmsi", "datain"
)
.INPUT_LIB_RE <- "^(?:mco|had|ssr|psy)\\d{2}bd$"

#' Check if a library name is an external source library
#' @param lib Character library name
#' @return Logical
#' @export
is_input_lib <- function(lib) {
  lib <- tolower(lib)
  lib %in% INPUT_LIBS || grepl(.INPUT_LIB_RE, lib, ignore.case = TRUE)
}

# --- Operation as named list ---

#' Create a new operation (named list)
#' @export
new_operation <- function(dataset, operation_type, file, line_number,
                          code_snippet, input_datasets, end_line,
                          macro_name = NULL, macro_source_file = NULL,
                          macro_source_line = NULL, macro_end_line = NULL) {
  list(
    dataset          = dataset,
    operation_type   = operation_type,
    file             = file,
    line_number      = as.integer(line_number),
    code_snippet     = code_snippet,
    input_datasets   = input_datasets,
    end_line         = as.integer(end_line),
    macro_name       = macro_name,
    macro_source_file = macro_source_file,
    macro_source_line = if (!is.null(macro_source_line)) as.integer(macro_source_line) else NULL,
    macro_end_line   = if (!is.null(macro_end_line)) as.integer(macro_end_line) else NULL
  )
}

#' Create a copy of an operation with some fields replaced
#' @export
replace_operation <- function(op, ...) {
  replacements <- list(...)
  for (nm in names(replacements)) {
    op[[nm]] <- replacements[[nm]]
  }
  # Ensure integer types for line number fields
  for (field in c("line_number", "end_line", "macro_source_line", "macro_end_line")) {
    if (!is.null(op[[field]])) {
      op[[field]] <- as.integer(op[[field]])
    }
  }
  op
}

# --- Utility: defaultdict(list) replacement ---

#' Append a value to a key in an environment (hash-map), creating the key if absent
#' @export
append_to <- function(env, key, value) {
  if (exists(key, envir = env, inherits = FALSE)) {
    current <- get(key, envir = env, inherits = FALSE)
    assign(key, c(current, list(value)), envir = env)
  } else {
    assign(key, list(value), envir = env)
  }
}

# Retrieve a key from an env-hash, returning default if absent
env_get <- function(env, key, default = list()) {
  if (exists(key, envir = env, inherits = FALSE)) {
    get(key, envir = env, inherits = FALSE)
  } else {
    default
  }
}

# --- Regexes for dataset name parsing ---
.RE_WORD <- "\\w+$"
.RE_MACRO_DOT <- "&\\w+\\."
.RE_KEEP_MACRO <- "[^\\w:&.]"
.RE_KEEP_PLAIN <- "[^\\w:]"
.RE_NUMBERED_RANGE <- "^(.+?)(\\d+)-(.+?)(\\d+)$"

# Strip paren-balanced dataset options from a dataset token
.strip_dataset_options <- function(name) {
  out <- character(0)
  i <- 1L
  n <- nchar(name)
  while (i <= n) {
    ch <- substr(name, i, i)
    if (ch == "(") {
      depth <- 1L
      i <- i + 1L
      while (i <= n && depth > 0L) {
        ch2 <- substr(name, i, i)
        if (ch2 == "(") depth <- depth + 1L
        else if (ch2 == ")") depth <- depth - 1L
        i <- i + 1L
      }
      next
    }
    out <- c(out, ch)
    i <- i + 1L
  }
  paste0(out, collapse = "")
}

#' Clean a SAS dataset name
#' @export
clean_dataset_name <- function(name) {
  name <- trimws(.strip_dataset_options(trimws(name)))
  if (grepl(".", name, fixed = TRUE)) {
    parts <- strsplit(name, ".", fixed = TRUE)[[1]]
    before <- parts[1]
    after <- paste(parts[-1], collapse = ".")
    if (!grepl("&", before, fixed = TRUE) && grepl("^\\w+$", before, perl = TRUE)) {
      name <- after
    }
  }
  if (grepl(.RE_MACRO_DOT, name, perl = TRUE)) {
    name <- gsub(.RE_KEEP_MACRO, "", name, perl = TRUE)
  } else {
    name <- gsub(.RE_KEEP_PLAIN, "", name, perl = TRUE)
  }
  result <- tolower(name)
  if (nzchar(result)) result else NULL
}

#' Remove duplicates from a character vector while preserving order
#' @export
deduplicate_list <- function(items) {
  seen <- character(0)
  deduped <- character(0)
  for (item in items) {
    if (!(item %in% seen)) {
      seen <- c(seen, item)
      deduped <- c(deduped, item)
    }
  }
  deduped
}

#' Expand SAS numbered dataset ranges like s1-s4
#' @export
expand_numbered_range <- function(name) {
  m <- regmatches(name, regexec(.RE_NUMBERED_RANGE, name, perl = TRUE))[[1]]
  if (length(m) == 0L) return(name)
  prefix1 <- m[2]
  n1_str  <- m[3]
  prefix2 <- m[4]
  n2_str  <- m[5]
  if (tolower(prefix1) != tolower(prefix2)) return(name)
  n1 <- as.integer(n1_str)
  n2 <- as.integer(n2_str)
  if (n1 > n2) return(name)
  width <- nchar(n1_str)
  if (width > 1L && startsWith(n1_str, "0")) {
    return(sprintf(paste0("%s%0", width, "d"), prefix1, seq.int(n1, n2)))
  }
  paste0(prefix1, seq.int(n1, n2))
}

#' Parse dataset names from a string, handling nested parentheses for options
#' @export
parse_dataset_names_with_parens <- function(datasets_str, exclude_keywords = character(0)) {
  dataset_names <- character(0)
  i <- 1L
  n <- nchar(datasets_str)

  while (i <= n) {
    # Skip whitespace
    while (i <= n && grepl("\\s", substr(datasets_str, i, i), perl = TRUE)) {
      i <- i + 1L
    }
    if (i > n) break

    # Read dataset name token (up to whitespace, open-paren)
    start <- i
    while (i <= n && !grepl("[\\s(]", substr(datasets_str, i, i), perl = TRUE)) {
      i <- i + 1L
    }
    dataset_name <- trimws(substr(datasets_str, start, i - 1L))

    # Skip parenthesized options
    if (i <= n && substr(datasets_str, i, i) == "(") {
      paren_count <- 1L
      i <- i + 1L
      while (i <= n && paren_count > 0L) {
        ch <- substr(datasets_str, i, i)
        if (ch == "(") paren_count <- paren_count + 1L
        else if (ch == ")") paren_count <- paren_count - 1L
        i <- i + 1L
      }
    }

    if (nzchar(dataset_name) && !(tolower(dataset_name) %in% exclude_keywords)) {
      for (expanded in expand_numbered_range(dataset_name)) {
        cleaned <- clean_dataset_name(expanded)
        if (!is.null(cleaned) && nchar(cleaned) > 1L) {
          dataset_names <- c(dataset_names, cleaned)
        }
      }
    }
  }
  dataset_names
}

#' Remove SAS block comments from a string
#' @export
strip_sas_block_comments <- function(s) {
  out <- character(0)
  i <- 1L
  n <- nchar(s)
  while (i <= n) {
    if (i < n && substr(s, i, i + 1L) == "/*") {
      i <- i + 2L
      while (i < n && substr(s, i, i + 1L) != "*/") {
        i <- i + 1L
      }
      i <- i + 2L
      out <- c(out, " ")
      next
    }
    out <- c(out, substr(s, i, i))
    i <- i + 1L
  }
  paste0(out, collapse = "")
}

# Macro control-flow regex
.MACRO_CTRL_KEYWORDS_RE <- paste0(
  "(?:",
  "%\\s*if\\b[^;%]*?\\s*%\\s*then\\b",   # %if <cond> %then
  "|%\\s*else\\b",                          # %else
  "|%\\s*do\\b[^;]*;",                      # %do [args];
  "|%\\s*end\\b\\s*;?",                     # %end [;]
  ")"
)

#' Replace macro control-flow tokens with spaces
#' @export
strip_macro_control_flow <- function(s) {
  m <- gregexpr(.MACRO_CTRL_KEYWORDS_RE, s, perl = TRUE, ignore.case = TRUE)
  if (m[[1]][1] == -1L) return(s)
  matches <- regmatches(s, m)[[1]]
  replacements <- vapply(matches, function(x) strrep(" ", nchar(x)), character(1))
  regmatches(s, m) <- list(replacements)
  s
}

#' Find first semicolon not inside parentheses or quotes
#' @return 1-based index or -1
#' @export
find_statement_end_semicolon <- function(s) {
  paren <- 0L
  in_double_quote <- FALSE
  in_single_quote <- FALSE
  n <- nchar(s)
  i <- 1L
  while (i <= n) {
    c <- substr(s, i, i)
    if (in_double_quote) {
      if (c == "\"") in_double_quote <- FALSE
      i <- i + 1L
      next
    }
    if (in_single_quote) {
      if (c == "'") in_single_quote <- FALSE
      i <- i + 1L
      next
    }
    if (c == "\"" && !in_single_quote) {
      in_double_quote <- TRUE
      i <- i + 1L
      next
    }
    if (c == "'" && !in_double_quote) {
      in_single_quote <- TRUE
      i <- i + 1L
      next
    }
    if (c == "(") {
      paren <- paren + 1L
      i <- i + 1L
      next
    }
    if (c == ")") {
      paren <- paren - 1L
      i <- i + 1L
      next
    }
    if (c == ";" && paren == 0L) {
      return(i)
    }
    i <- i + 1L
  }
  -1L
}

#' Create a new operation with corrected line numbers
#' @export
fix_operation_line_number <- function(operation, new_line_number) {
  offset <- new_line_number - operation$line_number
  new_end_line <- operation$end_line + offset
  replace_operation(operation, line_number = new_line_number, end_line = new_end_line)
}

.MACRO_VAR_REF_RE <- "&{1,2}([A-Za-z_]\\w*)"

# SAS built-in / read-only automatic macro variables
.SAS_BUILTIN_VARS <- c(
  "sqlobs", "sysobs", "syssqlobs", "syssqlobsmax",
  "syserr", "syserrortext", "syscc", "syslast", "sysrc",
  "sysmsg", "sysscp", "sysscpl", "sysdate", "systime", "sysday",
  "sysver", "sysuserid", "sysjobid", "sysprocessid",
  "sysmacroname", "sysstartid", "sysfunc", "sysevalf"
)

#' Return ordered, de-duplicated macro variable names referenced in code
#' @export
scan_macro_var_refs <- function(code_lines) {
  refs <- character(0)
  seen <- character(0)
  for (line in code_lines) {
    matches <- gregexpr(.MACRO_VAR_REF_RE, line, perl = TRUE)
    if (matches[[1]][1] != -1L) {
      captured <- regmatches(line, matches)[[1]]
      for (token in captured) {
        # Extract the group (the name after & or &&)
        m <- regmatches(token, regexec(.MACRO_VAR_REF_RE, token, perl = TRUE))[[1]]
        name <- tolower(m[2])
        if (name %in% .SAS_BUILTIN_VARS || name %in% seen) next
        seen <- c(seen, name)
        refs <- c(refs, name)
      }
    }
  }
  refs
}
