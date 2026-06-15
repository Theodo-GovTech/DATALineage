# proc_generic.R — Generic PROC parsing (ported from parsers/proc_generic.py)

.PROC_TYPES_WITH_INLINE_OUT <- c("transpose", "append")
.PROC_TYPES_WITH_OUTPUT_STMT <- c("means", "summary", "univariate")
.PROC_TYPES_WITH_TABLE_OUT <- c("freq")

.RUN_RE_GENERIC <- "\\brun\\s*;"
.NEXT_BLOCK_RE <- "^\\s*(?:data|proc|quit)\\b"
.TABLE_STMT_RE <- "\\btables?\\b"

#' Parse a generic PROC step (TRANSPOSE, APPEND, MEANS, SUMMARY, FREQ, UNIVARIATE)
#' @param lines Character vector of file lines
#' @param start_idx Integer 1-based index where the PROC step starts
#' @param filepath Character path to the source file
#' @param proc_type Character PROC type name (e.g. "transpose", "freq")
#' @return Named list operation, or `NULL` when no output dataset is found
#' @export
parse_proc_generic <- function(lines, start_idx, filepath, proc_type) {
  proc_type_lower <- tolower(proc_type)
  proc_line <- lines[[start_idx]]

  input_ds <- .extract_token_after(proc_line, "data")
  input_datasets <- character(0)
  if (!is.null(input_ds)) {
    cleaned_input <- clean_dataset_name(input_ds)
    if (!is.null(cleaned_input)) {
      input_datasets <- cleaned_input
    }
  }

  result <- .find_output_token(lines, start_idx, proc_type_lower, proc_line)
  output_token <- result$token
  end_idx <- result$end_idx

  if (is.null(output_token)) return(NULL)

  output_ds <- clean_dataset_name(output_token)
  if (is.null(output_ds)) return(NULL)

  code_snippet <- if (end_idx > start_idx) {
    paste0(lines[start_idx:end_idx], collapse = "")
  } else {
    proc_line
  }

  new_operation(
    dataset        = output_ds,
    operation_type = paste0("PROC ", toupper(proc_type)),
    file           = as.character(filepath),
    line_number    = start_idx,
    code_snippet   = code_snippet,
    input_datasets = input_datasets,
    end_line       = end_idx
  )
}

.find_output_token <- function(lines, start_idx, proc_type_lower, proc_line) {
  if (proc_type_lower %in% .PROC_TYPES_WITH_INLINE_OUT) {
    return(list(token = .extract_token_after(proc_line, "out"), end_idx = start_idx))
  }

  if (proc_type_lower %in% .PROC_TYPES_WITH_OUTPUT_STMT) {
    i <- start_idx
    while (i <= length(lines)) {
      line <- lines[[i]]
      if (i > start_idx && grepl(.NEXT_BLOCK_RE, line, ignore.case = TRUE, perl = TRUE)) {
        break
      }
      output_match <- regexpr("\\boutput\\b", line, ignore.case = TRUE, perl = TRUE)
      if (output_match > 0L) {
        rest <- substr(line, output_match + attr(output_match, "match.length"), nchar(line))
        token <- .extract_token_after(rest, "out")
        if (!is.null(token)) {
          return(list(token = token, end_idx = i))
        }
      }
      if (grepl(.RUN_RE_GENERIC, line, ignore.case = TRUE, perl = TRUE)) {
        return(list(token = NULL, end_idx = i))
      }
      i <- i + 1L
    }
    return(list(token = NULL, end_idx = min(i, length(lines))))
  }

  if (proc_type_lower %in% .PROC_TYPES_WITH_TABLE_OUT) {
    i <- start_idx
    while (i <= length(lines)) {
      line <- lines[[i]]
      if (i > start_idx && grepl(.NEXT_BLOCK_RE, line, ignore.case = TRUE, perl = TRUE)) {
        break
      }
      slash_pos <- regexpr("/", line, fixed = TRUE)
      if (slash_pos > 0L) {
        token <- .extract_token_after(substr(line, slash_pos + 1L, nchar(line)), "out")
        if (!is.null(token)) {
          return(list(token = token, end_idx = i))
        }
      }
      if (grepl(.RUN_RE_GENERIC, line, ignore.case = TRUE, perl = TRUE)) {
        return(list(token = NULL, end_idx = i))
      }
      i <- i + 1L
    }
    return(list(token = NULL, end_idx = min(i, length(lines))))
  }

  # Fallback: inline out
  list(token = .extract_token_after(proc_line, "out"), end_idx = start_idx)
}

.extract_token_after <- function(text, keyword) {
  pattern <- paste0("\\b", keyword, "\\s*=\\s*")
  m <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)
  if (m < 1L) return(NULL)

  start <- m + attr(m, "match.length")
  n <- nchar(text)
  if (start > n) return(NULL)

  i <- start
  paren_depth <- 0L
  while (i <= n) {
    c <- substr(text, i, i)
    if (paren_depth == 0L && (grepl("\\s", c) || c == ";")) break
    if (c == "(") {
      paren_depth <- paren_depth + 1L
    } else if (c == ")") {
      if (paren_depth > 0L) {
        paren_depth <- paren_depth - 1L
      } else {
        break
      }
    }
    i <- i + 1L
  }
  token <- trimws(substr(text, start, i - 1L))
  if (nzchar(token)) token else NULL
}
