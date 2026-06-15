# proc_format.R — PROC FORMAT parsing (ported from parsers/proc_format.py)

.VALUE_RE <- "^\\s*(?:invalue|value|picture)\\s+(\\$?[A-Za-z_]\\w*)"
.CNTLIN_RE <- "\\bcntlin\\s*=\\s*([^\\s;()]+)"
.PROC_FORMAT_RE <- "^\\s*proc\\s+format\\b"
.RUN_RE_FORMAT <- "^\\s*(run|quit)\\s*;"

#' Parse a PROC FORMAT block
#'
#' @param lines Character vector of file lines
#' @param start_idx 1-based index
#' @param filepath File path string
#' @return List with `operations` and `end_idx` (1-based)
#' @export
parse_proc_format <- function(lines, start_idx, filepath) {
  if (!grepl(.PROC_FORMAT_RE, lines[[start_idx]], ignore.case = TRUE, perl = TRUE)) {
    return(list(operations = list(), end_idx = start_idx))
  }

  proc_line <- lines[[start_idx]]
  operations <- list()
  seen <- character(0)

  cntlin_match <- regmatches(proc_line, regexec(.CNTLIN_RE, proc_line,
                                                 perl = TRUE, ignore.case = TRUE))[[1]]
  has_cntlin <- length(cntlin_match) >= 2L && nzchar(cntlin_match[2])

  i <- start_idx + 1L
  while (i <= length(lines)) {
    current <- lines[[i]]
    if (grepl(.RUN_RE_FORMAT, current, ignore.case = TRUE, perl = TRUE)) break
    # Bail on another proc/data before run;
    if (grepl("^\\s*(data|proc)\\s+", current, ignore.case = TRUE, perl = TRUE)) {
      i <- i - 1L
      break
    }

    # Check for cntlin= inline
    if (!has_cntlin) {
      cntlin_inline <- regmatches(current, regexec(.CNTLIN_RE, current,
                                                    perl = TRUE, ignore.case = TRUE))[[1]]
      if (length(cntlin_inline) >= 2L && nzchar(cntlin_inline[2])) {
        cntlin_match <- cntlin_inline
        has_cntlin <- TRUE
      }
    }

    value_match <- regmatches(current, regexec(.VALUE_RE, current,
                                                perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(value_match) >= 2L) {
      raw_name <- value_match[2]
      name <- tolower(sub("^\\$", "", raw_name))
      if (!(name %in% seen)) {
        seen <- c(seen, name)
        operations <- c(operations, list(new_operation(
          dataset        = paste0("fmt:", name),
          operation_type = "PROC FORMAT",
          file           = as.character(filepath),
          line_number    = i,
          code_snippet   = trimws(current, which = "right"),
          input_datasets = character(0),
          end_line       = i
        )))
      }
    }
    i <- i + 1L
  }

  end_idx <- i

  if (has_cntlin && length(cntlin_match) >= 2L) {
    source_ds <- clean_dataset_name(cntlin_match[2])
    if (!is.null(source_ds)) {
      operations <- c(operations, list(new_operation(
        dataset        = paste0("fmt:cntlin:", source_ds),
        operation_type = "PROC FORMAT",
        file           = as.character(filepath),
        line_number    = start_idx,
        code_snippet   = trimws(proc_line, which = "right"),
        input_datasets = source_ds,
        end_line       = end_idx
      )))
    }
  }

  list(operations = operations, end_idx = end_idx)
}

.FORMAT_REF_PUT_RE <- "\\bput\\s*\\(\\s*[^,()]+,\\s*(\\$?[A-Za-z_][\\w]*)(?:\\d+)?(?:\\.\\d+)?\\s*\\.\\s*\\)"

#' Return ordered, de-duplicated user-defined format names referenced via put()
#' @export
scan_format_refs <- function(code_lines) {
  refs <- character(0)
  seen <- character(0)
  for (line in code_lines) {
    matches <- gregexpr(.FORMAT_REF_PUT_RE, line, perl = TRUE, ignore.case = TRUE)
    if (matches[[1]][1] != -1L) {
      # Get full matches, then extract capture group
      all_matches <- regmatches(line, matches)[[1]]
      for (full_match in all_matches) {
        m <- regmatches(full_match, regexec(.FORMAT_REF_PUT_RE, full_match,
                                             perl = TRUE, ignore.case = TRUE))[[1]]
        if (length(m) >= 2L) {
          raw_name <- m[2]
          name <- tolower(sub("^\\$", "", raw_name))
          if (!nzchar(name) || grepl("^\\d", name) || name %in% seen) next
          seen <- c(seen, name)
          refs <- c(refs, name)
        }
      }
    }
  }
  refs
}
