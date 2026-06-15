# comments.R — Block comment handling (ported from parsers/comments.py)

#' Handle full-line SAS block comments starting at current index
#'
#' Uses 1-based indexing. When a comment is followed by code on the same
#' line, `replacement` is non-NULL and the caller must update
#' `lines[[result$new_i]] <- result$replacement`.
#'
#' @param lines Character vector of file lines
#' @param i 1-based index of current line
#' @return List with `should_continue` (logical), `new_i` (integer),
#'   and optionally `replacement` (character or NULL)
#' @export
handle_block_comments <- function(lines, i) {

  line <- lines[[i]]
  if (!grepl("^\\s*/\\*", line)) {
    return(list(should_continue = FALSE, new_i = i, replacement = NULL))
  }

  # One-line block comment or comment start+end on same line
  end_idx <- regexpr("*/", line, fixed = TRUE)
  if (end_idx > 0L) {
    remainder <- substr(line, end_idx + 2L, nchar(line))
    if (nzchar(trimws(remainder))) {
      return(list(should_continue = TRUE, new_i = i, replacement = remainder))
    }
    return(list(should_continue = TRUE, new_i = i + 1L, replacement = NULL))
  }

  # Multi-line block comment: skip until end marker
  j <- i + 1L
  while (j <= length(lines)) {
    end_idx <- regexpr("*/", lines[[j]], fixed = TRUE)
    if (end_idx > 0L) {
      remainder <- substr(lines[[j]], end_idx + 2L, nchar(lines[[j]]))
      if (nzchar(trimws(remainder))) {
        return(list(should_continue = TRUE, new_i = j, replacement = remainder))
      }
      return(list(should_continue = TRUE, new_i = j + 1L, replacement = NULL))
    }
    j <- j + 1L
  }

  # Unterminated comment: nothing left to parse
  list(should_continue = TRUE, new_i = length(lines) + 1L, replacement = NULL)
}
