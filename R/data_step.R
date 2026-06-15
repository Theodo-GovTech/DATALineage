# data_step.R — DATA step parsing (ported from parsers/data_step.py)

#' Extract the full DATA statement (may span multiple lines)
#' @export
extract_data_statement <- function(lines, start_idx) {
  parts <- lines[[start_idx]]
  temp_idx <- start_idx

  while (find_statement_end_semicolon(parts) < 0L && temp_idx + 1L <= length(lines)) {
    temp_idx <- temp_idx + 1L
    parts <- paste0(parts, " ", lines[[temp_idx]])
  }
  parts
}

#' Parse output dataset names from DATA statement
#' @export
parse_data_output_datasets <- function(data_statement) {
  m <- regmatches(data_statement, regexec(
    "^\\s*data\\s+([^;]+);",
    data_statement, perl = TRUE, ignore.case = TRUE
  ))[[1]]
  if (length(m) < 2L) return(NULL)

  output_str <- trimws(m[2])
  if (!nzchar(output_str)) return(NULL)

  output_datasets <- parse_dataset_names_with_parens(output_str)
  if (length(output_datasets) == 0L) NULL else output_datasets
}

#' Collect input datasets and INFILE references from DATA step body
#' @export
collect_data_step_inputs <- function(lines, start_idx, filepath, output_ds) {
  input_datasets <- character(0)
  input_files <- character(0)
  code_lines <- character(0)
  infile_records <- list()
  i <- start_idx
  set_merge_buffer <- NULL

  exclude_keywords <- c("by", "end", "rename", "where", "if", "then")

  while (i <= length(lines)) {
    current_line <- lines[[i]]
    code_lines <- c(code_lines, current_line)

    # INFILE statement
    infile_match <- regmatches(current_line, regexec(
      "\\binfile\\s+(\\w+)", current_line, perl = TRUE, ignore.case = TRUE
    ))[[1]]
    if (length(infile_match) >= 2L) {
      fileref <- tolower(infile_match[2])
      input_files <- c(input_files, fileref)
      infile_records <- c(infile_records, list(list(
        dataset = output_ds, fileref = fileref,
        filepath = as.character(filepath), line_num = i
      )))
    }

    # SET/MERGE/UPDATE handling
    if (!is.null(set_merge_buffer)) {
      set_merge_buffer <- paste0(set_merge_buffer, " ", trimws(current_line))
      stripped_buffer <- strip_macro_control_flow(set_merge_buffer)
      idx <- find_statement_end_semicolon(stripped_buffer)
      if (idx > 0L) {
        datasets_str <- strip_sas_block_comments(trimws(substr(stripped_buffer, 1, idx - 1L)))
        parsed_datasets <- parse_dataset_names_with_parens(datasets_str, exclude_keywords)
        input_datasets <- c(input_datasets, parsed_datasets)
        set_merge_buffer <- NULL
      }
    } else {
      set_match <- regmatches(current_line, regexec(
        "\\b(set|merge|update)\\s+(.+)",
        current_line, perl = TRUE, ignore.case = TRUE
      ))[[1]]
      if (length(set_match) >= 3L) {
        rest <- trimws(set_match[3])
        stripped_rest <- strip_macro_control_flow(rest)
        idx <- find_statement_end_semicolon(stripped_rest)
        if (idx > 0L) {
          datasets_str <- strip_sas_block_comments(trimws(substr(stripped_rest, 1, idx - 1L)))
          parsed_datasets <- parse_dataset_names_with_parens(datasets_str, exclude_keywords)
          input_datasets <- c(input_datasets, parsed_datasets)
        } else {
          set_merge_buffer <- rest
        }
      }
    }

    # End of DATA step: run;
    if (grepl("\\brun\\s*;", current_line, ignore.case = TRUE, perl = TRUE)) {
      if (!is.null(set_merge_buffer)) {
        raw <- strip_sas_block_comments(trimws(strip_macro_control_flow(set_merge_buffer)))
        parsed <- parse_dataset_names_with_parens(raw, exclude_keywords)
        input_datasets <- c(input_datasets, parsed)
      }
      break
    }
    # Next data/proc block
    if (i > start_idx && grepl("^\\s*(data|proc)\\s+", current_line,
                                ignore.case = TRUE, perl = TRUE)) {
      if (!is.null(set_merge_buffer)) {
        raw <- strip_sas_block_comments(trimws(strip_macro_control_flow(set_merge_buffer)))
        parsed <- parse_dataset_names_with_parens(raw, exclude_keywords)
        input_datasets <- c(input_datasets, parsed)
        set_merge_buffer <- NULL
      }
      break
    }

    i <- i + 1L
  }

  # Flush remaining buffer
  if (!is.null(set_merge_buffer)) {
    raw <- strip_sas_block_comments(trimws(strip_macro_control_flow(set_merge_buffer)))
    parsed <- parse_dataset_names_with_parens(raw, exclude_keywords)
    input_datasets <- c(input_datasets, parsed)
  }

  # INFILE refs
  for (fileref in input_files) {
    input_datasets <- c(input_datasets, paste0("INFILE:", fileref))
  }

  # Macro variable refs
  for (mv_name in scan_macro_var_refs(code_lines)) {
    input_datasets <- c(input_datasets, paste0("mv:", mv_name))
  }

  # Format refs
  for (fmt_name in scan_format_refs(code_lines)) {
    input_datasets <- c(input_datasets, paste0("fmt:", fmt_name))
  }

  input_datasets <- deduplicate_list(input_datasets)

  list(
    input_datasets = input_datasets,
    code_lines     = code_lines,
    end_line       = i,
    infile_records = infile_records
  )
}

#' Parse a DATA step to extract output and input datasets
#' @export
parse_data_step <- function(lines, start_idx, filepath) {
  data_statement <- extract_data_statement(lines, start_idx)
  output_datasets <- parse_data_output_datasets(data_statement)
  if (is.null(output_datasets)) return(NULL)

  output_ds <- output_datasets[1]
  result <- collect_data_step_inputs(lines, start_idx, filepath, output_ds)

  code_snippet <- paste0(result$code_lines[seq_len(min(500L, length(result$code_lines)))],
                          collapse = "")

  operations <- lapply(output_datasets, function(output_ds_name) {
    new_operation(
      dataset        = output_ds_name,
      operation_type = "DATA",
      file           = as.character(filepath),
      line_number    = start_idx,
      code_snippet   = code_snippet,
      input_datasets = result$input_datasets,
      end_line       = result$end_line
    )
  })

  list(operations = operations, infile_records = result$infile_records)
}
