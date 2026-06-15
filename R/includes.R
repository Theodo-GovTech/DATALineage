# includes.R — %include resolution (ported from parsers/includes.py)

.INCLUDE_FILEREF_RE <- "^\\s*%include\\s+([A-Za-z_][\\w&.]*)\\s*;"
.INCLUDE_QUOTED_RE <- "^\\s*%include\\s+[\"']([^\"']+)[\"']\\s*;"
.MACRO_VAR_REF_INC_RE <- "&([A-Za-z_]\\w*)\\.?"

#' Substitute known macro-var references in a path
#' @export
substitute_macro_vars <- function(path, macro_variables) {
  if (is.null(macro_variables) || length(macro_variables) == 0L) return(path)

  previous <- NULL
  current <- path
  for (iter in seq_len(5L)) {
    if (identical(current, previous)) break
    previous <- current
    locs <- gregexpr(.MACRO_VAR_REF_INC_RE, current, perl = TRUE)
    if (locs[[1]][1] == -1L) break
    matched <- regmatches(current, locs)[[1]]
    replacements <- vapply(matched, function(tok) {
      m <- regmatches(tok, regexec(.MACRO_VAR_REF_INC_RE, tok, perl = TRUE))[[1]]
      name <- tolower(m[2])
      if (name %in% names(macro_variables)) {
        as.character(macro_variables[[name]])
      } else {
        tok
      }
    }, character(1))
    regmatches(current, locs) <- list(replacements)
  }
  current
}

.segment_to_glob <- function(segment) {
  gsub("&\\w+\\.?", "*", segment, perl = TRUE)
}

.search_include_in_roots <- function(remaining_path, search_roots) {
  if (is.null(search_roots) || length(search_roots) == 0L) return(NULL)

  segments <- unlist(strsplit(remaining_path, "[\\\\/]"))
  segments <- segments[nzchar(segments)]
  if (length(segments) == 0L) return(NULL)

  # Drop leading pure-macro segments
  while (length(segments) > 0L && grepl("^&\\w+\\.?$", segments[1], perl = TRUE)) {
    segments <- segments[-1]
  }
  if (length(segments) == 0L) return(NULL)

  for (start in seq_along(segments)) {
    attempt <- segments[start:length(segments)]
    glob_parts <- vapply(attempt, .segment_to_glob, character(1))
    glob_suffix <- paste0("**/", paste(glob_parts, collapse = "/"))

    for (root in search_roots) {
      root_path <- root
      if (!dir.exists(root_path)) next
      matches <- tryCatch(
        Sys.glob(file.path(root_path, glob_suffix)),
        error = function(e) character(0)
      )
      matches <- matches[file.exists(matches) & !dir.exists(matches)]
      if (length(matches) == 0L) next
      return(normalizePath(sort(matches)[1], mustWork = FALSE))
    }
  }
  NULL
}

#' Resolve %include path against including file location
#' @export
resolve_include_target <- function(include_path, including_file,
                                    macro_variables = NULL, search_roots = NULL) {
  substituted <- substitute_macro_vars(trimws(include_path), macro_variables)

  include_candidate <- substituted
  if (!grepl("^[/~]", substituted) && !grepl("^[A-Za-z]:", substituted)) {
    if (!grepl("&", substituted, fixed = TRUE)) {
      include_candidate <- file.path(dirname(including_file), substituted)
    }
  }

  if (!grepl("&", substituted, fixed = TRUE)) {
    resolved <- tryCatch(normalizePath(include_candidate, mustWork = FALSE), error = function(e) NULL)
    if (!is.null(resolved) && file.exists(resolved)) {
      return(resolved)
    }
  }

  fallback <- .search_include_in_roots(substituted, search_roots)
  if (!is.null(fallback)) return(fallback)

  if (!grepl("&", substituted, fixed = TRUE)) {
    resolved <- tryCatch(normalizePath(include_candidate, mustWork = FALSE), error = function(e) NULL)
    if (!is.null(resolved)) return(resolved)
  }
  NULL
}

#' Resolve %include fileref using the latest visible filename statement
#' @export
resolve_fileref_include_target <- function(including_file, include_line, fileref,
                                            file_fileref_definitions,
                                            macro_variables = NULL, search_roots = NULL,
                                            global_filename_refs = NULL) {
  including_file <- normalizePath(including_file, mustWork = FALSE)
  fileref <- normalize_fileref(fileref)
  latest <- NULL

  defs <- if (is.environment(file_fileref_definitions)) {
    env_get(file_fileref_definitions, including_file, list())
  } else if (is.list(file_fileref_definitions)) {
    file_fileref_definitions[[including_file]]
  } else {
    list()
  }

  for (fileref_def in defs) {
    if (fileref_def$line >= include_line) next
    if (fileref_def$fileref != fileref) next
    latest <- fileref_def
  }

  path <- NULL
  if (!is.null(latest)) {
    path <- latest$path
  } else if (!is.null(global_filename_refs) && !is.null(global_filename_refs[[fileref]])) {
    entry <- global_filename_refs[[fileref]]
    if (is.list(entry) && length(entry) >= 3L) {
      defining_file <- entry[[1]]
      defining_line <- entry[[2]]
      candidate_path <- entry[[3]]
      same_file <- normalizePath(defining_file, mustWork = FALSE) ==
                   normalizePath(including_file, mustWork = FALSE)
      if (same_file && defining_line >= include_line) {
        path <- NULL
      } else {
        path <- candidate_path
      }
    } else if (is.character(entry) && length(entry) == 1L) {
      path <- entry
    }
  }

  if (is.null(path)) return(NULL)

  resolve_include_target(
    path, including_file,
    macro_variables = macro_variables, search_roots = search_roots
  )
}

#' Parse %include statements from a SAS file
#' @export
parse_include_statements <- function(filepath, file_fileref_definitions,
                                      macro_variables = NULL, search_roots = NULL,
                                      global_filename_refs = NULL) {
  filepath <- normalizePath(filepath, mustWork = FALSE)
  lines <- readLines(filepath, encoding = "latin1", warn = FALSE)

  includes <- list()
  i <- 1L
  while (i <= length(lines)) {
    result <- handle_block_comments(lines, i)
    if (result$should_continue) {
      if (!is.null(result$replacement)) lines[[result$new_i]] <- result$replacement
      i <- result$new_i
      next
    }

    line <- lines[[i]]
    include_line <- i
    target <- NULL

    quoted_match <- regmatches(line, regexec(.INCLUDE_QUOTED_RE, line,
                                              perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(quoted_match) >= 2L) {
      target <- resolve_include_target(
        quoted_match[2], filepath,
        macro_variables = macro_variables, search_roots = search_roots
      )
    } else {
      fileref_match <- regmatches(line, regexec(.INCLUDE_FILEREF_RE, line,
                                                 perl = TRUE, ignore.case = TRUE))[[1]]
      if (length(fileref_match) >= 2L) {
        target <- resolve_fileref_include_target(
          filepath, include_line, fileref_match[2],
          file_fileref_definitions,
          macro_variables = macro_variables, search_roots = search_roots,
          global_filename_refs = global_filename_refs
        )
      }
    }

    if (!is.null(target)) {
      includes <- c(includes, list(list(line = include_line, target = target)))
    }
    i <- i + 1L
  }
  includes
}

#' Get macro name from a macro definition
#' @export
extract_macro_name_from_def <- function(macro_def) {
  macro_def$name
}

#' Build ordered local events (include + macro def) for one file
#' @export
get_file_events <- function(filepath, file_includes, file_macro_definitions,
                             max_line = NULL) {
  filepath <- normalizePath(filepath, mustWork = FALSE)
  events <- list()

  includes <- if (is.environment(file_includes)) {
    env_get(file_includes, filepath, list())
  } else {
    file_includes[[filepath]]
  }

  for (include_event in if (is.null(includes)) list() else includes) {
    line <- include_event$line
    if (!is.null(max_line) && line >= max_line) next
    events <- c(events, list(list(
      line = line, kind = "include", target = include_event$target
    )))
  }

  macro_defs <- if (is.environment(file_macro_definitions)) {
    env_get(file_macro_definitions, filepath, list())
  } else {
    file_macro_definitions[[filepath]]
  }

  for (macro_def in if (is.null(macro_defs)) list() else macro_defs) {
    line <- macro_def$line
    if (!is.null(max_line) && line >= max_line) next
    events <- c(events, list(list(
      line = line, kind = "macro_def",
      macro_name = extract_macro_name_from_def(macro_def),
      macro_def = macro_def
    )))
  }

  # Sort by line
  if (length(events) > 0L) {
    lines_vec <- vapply(events, function(e) e$line, integer(1))
    events <- events[order(lines_vec)]
  }
  events
}

#' Return visible macros after executing a file in source order
#' @export
get_exported_macros_for_file <- function(filepath, file_macro_exports_cache,
                                          file_includes, file_macro_definitions,
                                          visiting = NULL) {
  filepath <- normalizePath(filepath, mustWork = FALSE)

  cached <- if (is.environment(file_macro_exports_cache)) {
    env_get(file_macro_exports_cache, filepath, NULL)
  } else {
    file_macro_exports_cache[[filepath]]
  }
  if (!is.null(cached)) return(cached)

  if (is.null(visiting)) visiting <- character(0)
  if (filepath %in% visiting) return(list())

  visiting <- c(visiting, filepath)
  visible_macros <- list()
  events <- get_file_events(filepath, file_includes, file_macro_definitions)

  for (event in events) {
    if (event$kind == "include") {
      included_exports <- get_exported_macros_for_file(
        event$target, file_macro_exports_cache,
        file_includes, file_macro_definitions, visiting
      )
      if (length(included_exports) > 0L) {
        for (nm in names(included_exports)) {
          visible_macros[[nm]] <- included_exports[[nm]]
        }
      }
    } else if (event$kind == "macro_def") {
      macro_name <- event$macro_name
      if (!is.null(macro_name) && nzchar(macro_name)) {
        visible_macros[[macro_name]] <- event$macro_def
      }
    }
  }

  if (is.environment(file_macro_exports_cache)) {
    assign(filepath, visible_macros, envir = file_macro_exports_cache)
  } else {
    file_macro_exports_cache[[filepath]] <- visible_macros
  }
  visible_macros
}
