# ods.R — ODS tagsets.csv / xml / csv parsing (ported from parsers/ods.py)

.ODS_OPEN_RE <- "ods\\s+(tagsets\\.csv|xml|csv)\\s+file\\s*=\\s*(\\w+)"
.ODS_CLOSE_RE <- "ods\\s+(?:tagsets\\.csv|xml|csv)\\s+close"
.PROC_DATA_RE <- "\\bproc\\s+(?:print|report|tabulate|freq|means|summary)\\b[^;]*\\bdata\\s*=\\s*([^\\s;()]+)"
.MACRO_CALL_ODS_RE <- "%\\s*([A-Za-z_]\\w*)\\s*(?:\\([^)]*\\))?\\s*;"

.find_block_end <- function(lines, start_idx, open_end_col) {
  line <- lines[[start_idx]]
  rest <- substr(line, open_end_col + 1L, nchar(line))
  close_pos <- regexpr(.ODS_CLOSE_RE, rest, ignore.case = TRUE, perl = TRUE)
  if (close_pos > 0L) {
    body <- substr(rest, 1, close_pos - 1L)
    return(list(body = body, end_idx = start_idx))
  }

  parts <- rest
  i <- start_idx + 1L
  while (i <= length(lines) && i - start_idx < 200L) {
    close_match <- regexpr(.ODS_CLOSE_RE, lines[[i]], ignore.case = TRUE, perl = TRUE)
    if (close_match > 0L) {
      parts <- c(parts, substr(lines[[i]], 1, close_match - 1L))
      return(list(body = paste0(parts, collapse = ""), end_idx = i))
    }
    parts <- c(parts, lines[[i]])
    i <- i + 1L
  }
  list(body = NULL, end_idx = NULL)
}

.scan_proc_data_refs <- function(text, inputs, seen) {
  matches <- gregexpr(.PROC_DATA_RE, text, perl = TRUE, ignore.case = TRUE)
  if (matches[[1]][1] != -1L) {
    all_matches <- regmatches(text, matches)[[1]]
    for (full_match in all_matches) {
      m <- regmatches(full_match, regexec(.PROC_DATA_RE, full_match,
                                           perl = TRUE, ignore.case = TRUE))[[1]]
      if (length(m) >= 2L) {
        ds <- clean_dataset_name(m[2])
        if (!is.null(ds) && !(ds %in% seen)) {
          seen <- c(seen, ds)
          inputs <- c(inputs, ds)
        }
      }
    }
  }
  list(inputs = inputs, seen = seen)
}

#' Parse an ODS tagsets.csv / xml / csv block
#' @export
parse_ods_tagsets_csv <- function(lines, start_idx, filepath,
                                   macro_definitions = NULL, filename_refs = NULL) {
  line <- lines[[start_idx]]
  open_match <- regexpr(.ODS_OPEN_RE, line, ignore.case = TRUE, perl = TRUE)
  if (open_match < 1L) return(NULL)

  # Extract fileref (group 2)
  m <- regmatches(line, regexec(.ODS_OPEN_RE, line, perl = TRUE, ignore.case = TRUE))[[1]]
  if (length(m) < 3L) return(NULL)
  fileref <- m[3]

  output <- NULL
  if (!is.null(filename_refs)) {
    resolved <- filename_refs[[normalize_fileref(fileref)]]
    if (!is.null(resolved)) {
      output <- path_to_dataset_name(resolved[[3]])
    }
  }
  if (is.null(output)) output <- fileref

  open_end_col <- open_match + attr(open_match, "match.length") - 1L
  result <- .find_block_end(lines, start_idx, open_end_col)
  if (is.null(result$body)) return(NULL)

  inputs <- character(0)
  seen <- character(0)
  ref_result <- .scan_proc_data_refs(result$body, inputs, seen)
  inputs <- ref_result$inputs
  seen <- ref_result$seen

  # Look through macro calls in body
  if (!is.null(macro_definitions) && length(macro_definitions) > 0L) {
    mc_matches <- gregexpr(.MACRO_CALL_ODS_RE, result$body, perl = TRUE, ignore.case = TRUE)
    if (mc_matches[[1]][1] != -1L) {
      all_mc <- regmatches(result$body, mc_matches)[[1]]
      skip_names <- c("if", "do", "else", "end", "let", "put",
                       "include", "global", "local", "mend", "macro")
      for (mc in all_mc) {
        mc_m <- regmatches(mc, regexec(.MACRO_CALL_ODS_RE, mc,
                                        perl = TRUE, ignore.case = TRUE))[[1]]
        if (length(mc_m) >= 2L) {
          macro_name <- tolower(mc_m[2])
          if (macro_name %in% skip_names) next
          macro_def <- macro_definitions[[macro_name]]
          if (is.null(macro_def)) next
          macro_body <- paste0(macro_def$body %||% character(0), collapse = "")
          ref_result2 <- .scan_proc_data_refs(macro_body, inputs, seen)
          inputs <- ref_result2$inputs
          seen <- ref_result2$seen
        }
      }
    }
  }

  code_snippet <- substr(paste0(lines[start_idx:result$end_idx], collapse = ""), 1, 10000)

  operation <- new_operation(
    dataset        = tolower(output),
    operation_type = "ODS CSV",
    file           = as.character(filepath),
    line_number    = start_idx,
    code_snippet   = code_snippet,
    input_datasets = inputs,
    end_line       = result$end_idx
  )

  list(operation = operation, end_idx = result$end_idx)
}
