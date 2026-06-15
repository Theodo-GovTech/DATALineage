# proc_sort.R — PROC SORT parsing (ported from parsers/proc_sort.py)

#' Parse PROC SORT
#'
#' @param lines Character vector of file lines
#' @param start_idx 1-based index
#' @param filepath File path string
#' @return An operation (named list) or NULL
#' @export
parse_proc_sort <- function(lines, start_idx, filepath) {
  line <- lines[[start_idx]]

  sort_match <- regmatches(line, regexec(
    "\\s*proc\\s+sort\\s+data\\s*=\\s*([^\\s;]+)",
    line, perl = TRUE, ignore.case = TRUE
  ))[[1]]
  if (length(sort_match) == 0L) return(NULL)

  dataset <- clean_dataset_name(sort_match[2])

  out_match <- regmatches(line, regexec(
    "out\\s*=\\s*([^\\s;]+)",
    line, perl = TRUE, ignore.case = TRUE
  ))[[1]]
  if (length(out_match) >= 2L && nzchar(out_match[2])) {
    output_ds <- clean_dataset_name(out_match[2])
    input_datasets <- dataset
  } else {
    output_ds <- dataset
    input_datasets <- dataset
  }

  new_operation(
    dataset        = output_ds,
    operation_type = "PROC SORT",
    file           = as.character(filepath),
    line_number    = start_idx,
    code_snippet   = line,
    input_datasets = input_datasets,
    end_line       = start_idx
  )
}
