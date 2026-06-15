# filename.R — FILENAME statement parsing (ported from parsers/filename.py)

.FILEREF_TOKEN <- "[A-Za-z_][\\w&.]*"

.FILENAME_QUOTED_RE <- paste0(
  "^\\s*filename\\s+(", .FILEREF_TOKEN, ")\\s+[\"']([^\"']+)[\"']"
)
.FILENAME_UNQUOTED_RE <- paste0(
  "^\\s*filename\\s+(", .FILEREF_TOKEN, ")\\s+([^;]+);"
)

.MACRO_REF_RE <- "&\\w+\\.?"
.TRAILING_EXT_RE <- "\\.[A-Za-z0-9]+$"
.NON_WORD_EDGE_RE <- "^[^\\w]+|[^\\w]+$"

#' Derive a dataset-style name from a (possibly macro-laden) path literal
#' @export
path_to_dataset_name <- function(path) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  # basename: split on / and \\
  basename_str <- sub(".*[\\\\/]", "", path)
  no_macros <- gsub(.MACRO_REF_RE, "", basename_str, perl = TRUE)
  stem <- sub(.TRAILING_EXT_RE, "", no_macros, perl = TRUE)
  stem <- gsub(.NON_WORD_EDGE_RE, "", stem, perl = TRUE)
  if (grepl(".", stem, fixed = TRUE)) {
    parts <- strsplit(stem, ".", fixed = TRUE)[[1]]
    stem <- parts[length(parts)]
  }
  result <- tolower(stem)
  if (nzchar(result)) result else NULL
}

#' Normalize a fileref — lowercase and strip embedded macro references
#' @export
normalize_fileref <- function(fileref) {
  if (is.null(fileref)) return(NULL)
  tolower(gsub("&\\w+\\.?", "", fileref, perl = TRUE))
}

#' Parse FILENAME statements to map filerefs to actual paths
#' @export
parse_filename_statements <- function(filepath) {
  filepath <- normalizePath(filepath, mustWork = FALSE)
  lines <- readLines(filepath, encoding = "latin1", warn = FALSE)

  filename_refs <- list()
  file_fileref_definitions <- list()

  for (i in seq_along(lines)) {
    line <- lines[[i]]

    m <- regmatches(line, regexec(.FILENAME_QUOTED_RE, line,
                                   perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(m) < 3L) {
      m <- regmatches(line, regexec(.FILENAME_UNQUOTED_RE, line,
                                     perl = TRUE, ignore.case = TRUE))[[1]]
    }
    if (length(m) < 3L) next

    fileref <- normalize_fileref(m[2])
    path <- trimws(sub(";$", "", trimws(m[3])))
    filename_refs[[fileref]] <- list(filepath, i, path)
    file_fileref_definitions <- c(file_fileref_definitions, list(
      list(fileref = fileref, line = i, path = path)
    ))
  }

  list(filename_refs = filename_refs, file_fileref_definitions = file_fileref_definitions)
}
