# proc_export.R — PROC EXPORT parsing (ported from parsers/proc_export.py)

.RUN_RE_EXPORT <- "\\brun\\s*;"
.DATA_RE_EXPORT <- "\\bdata\\s*=\\s*([^\\s;()]+)"
.OUTFILE_QUOTED_RE <- "\\boutfile\\s*=\\s*(['\"])(.*?)\\1"

#' Parse a proc export ... run; block
#'
#' @param lines Character vector of file lines
#' @param start_idx 1-based index
#' @param filepath File path string
#' @return List with `operation` and `end_idx` (1-based), or NULL
#' @export
parse_proc_export <- function(lines, start_idx, filepath) {
  proc_line <- lines[[start_idx]]
  if (!grepl("^\\s*proc\\s+export\\b", proc_line, ignore.case = TRUE, perl = TRUE)) {
    return(NULL)
  }

  end_idx <- .find_run_export(lines, start_idx)
  body <- paste0(lines[start_idx:end_idx], collapse = "\n")

  output_ds <- .outfile_to_dataset(body)
  if (is.null(output_ds)) return(NULL)

  input_datasets <- character(0)
  data_match <- regmatches(body, regexec(.DATA_RE_EXPORT, body,
                                          perl = TRUE, ignore.case = TRUE))[[1]]
  if (length(data_match) >= 2L) {
    cleaned <- clean_dataset_name(data_match[2])
    if (!is.null(cleaned)) {
      input_datasets <- cleaned
    }
  }

  operation <- new_operation(
    dataset        = output_ds,
    operation_type = "PROC EXPORT",
    file           = as.character(filepath),
    line_number    = start_idx,
    code_snippet   = substr(body, 1, 10000),
    input_datasets = input_datasets,
    end_line       = end_idx
  )

  list(operation = operation, end_idx = end_idx)
}

.find_run_export <- function(lines, start_idx) {
  i <- start_idx
  while (i <= length(lines)) {
    if (grepl(.RUN_RE_EXPORT, lines[[i]], ignore.case = TRUE, perl = TRUE)) {
      return(i)
    }
    i <- i + 1L
  }
  length(lines)
}

.outfile_to_dataset <- function(text) {
  m <- regmatches(text, regexec(.OUTFILE_QUOTED_RE, text,
                                 perl = TRUE, ignore.case = TRUE))[[1]]
  if (length(m) < 3L) return(NULL)
  path_to_dataset_name(m[3])
}
