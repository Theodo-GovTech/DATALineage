# test-filename.R — Tests for filename.R and include fileref resolution

test_that("simple filename", {
  with_temp_sas_dir(
    list("test.sas" = 'filename myfile "/path/to/file.txt";\n'),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_filename_stmts(file.path(dir, "test.sas"))
      expect_true("myfile" %in% names(analyzer$filename_refs))
      expect_equal(analyzer$filename_refs[["myfile"]][[3]], "/path/to/file.txt")
    }
  )
})

test_that("filename with macro vars", {
  with_temp_sas_dir(
    list("test.sas" = "filename data1 &path/&file..txt;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_filename_stmts(file.path(dir, "test.sas"))
      expect_true("data1" %in% names(analyzer$filename_refs))
      expect_true(grepl("&path", analyzer$filename_refs[["data1"]][[3]], fixed = TRUE))
    }
  )
})

test_that("include fileref resolves from filename", {
  with_temp_sas_dir(
    list(
      "inc.sas" = "%macro mk(); %mend;\n",
      "main.sas" = ""  # placeholder, overwritten below
    ),
    function(dir) {
      inc_path <- file.path(dir, "inc.sas")
      main_path <- file.path(dir, "main.sas")
      writeLines(
        c(paste0('filename incl "', inc_path, '";'), "%include incl;"),
        main_path
      )
      analyzer <- SASLineageAnalyzer$new(dir)
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_filename_stmts(main_path)
      private_env$parse_include_stmts(main_path)
      key <- normalizePath(main_path, mustWork = FALSE)
      includes <- analyzer$file_includes[[key]]
      expect_length(includes, 1L)
      expect_equal(
        normalizePath(includes[[1]]$target, mustWork = FALSE),
        normalizePath(inc_path, mustWork = FALSE)
      )
    }
  )
})

test_that("include fileref uses latest prior filename redefinition", {
  with_temp_sas_dir(
    list(
      "inc_old.sas" = "%macro mk(); %mend;\n",
      "inc_new.sas" = "%macro mk(); %mend;\n",
      "main.sas" = ""
    ),
    function(dir) {
      old_path <- file.path(dir, "inc_old.sas")
      new_path <- file.path(dir, "inc_new.sas")
      main_path <- file.path(dir, "main.sas")
      writeLines(c(
        paste0('filename incl "', old_path, '";'),
        paste0('filename incl "', new_path, '";'),
        "%include incl;"
      ), main_path)

      analyzer <- SASLineageAnalyzer$new(dir)
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_filename_stmts(main_path)
      private_env$parse_include_stmts(main_path)
      key <- normalizePath(main_path, mustWork = FALSE)
      includes <- analyzer$file_includes[[key]]
      expect_length(includes, 1L)
      expect_equal(
        normalizePath(includes[[1]]$target, mustWork = FALSE),
        normalizePath(new_path, mustWork = FALSE)
      )
    }
  )
})

test_that("include fileref ignores filename defined after include", {
  with_temp_sas_dir(
    list(
      "inc.sas" = "%macro mk(); %mend;\n",
      "main.sas" = ""
    ),
    function(dir) {
      inc_path <- file.path(dir, "inc.sas")
      main_path <- file.path(dir, "main.sas")
      writeLines(c(
        "%include incl;",
        paste0('filename incl "', inc_path, '";')
      ), main_path)

      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$.include_search_roots <- character(0)
      private_env <- analyzer$.__enclos_env__$private
      private_env$parse_filename_stmts(main_path)
      private_env$parse_include_stmts(main_path)
      key <- normalizePath(main_path, mustWork = FALSE)
      includes <- analyzer$file_includes[[key]]
      expect_true(is.null(includes) || length(includes) == 0L)
    }
  )
})

test_that("path_to_dataset_name keeps last dotted part when stem has a dot", {
  expect_equal(path_to_dataset_name("lib.tbl.txt"), "tbl")
})

test_that("path_to_dataset_name returns NULL when stem reduces to empty", {
  expect_null(path_to_dataset_name("&mac."))
})

test_that("path_to_dataset_name returns NULL on empty or NULL input", {
  expect_null(path_to_dataset_name(""))
  expect_null(path_to_dataset_name(NULL))
})

test_that("normalize_fileref returns NULL on NULL input", {
  expect_null(normalize_fileref(NULL))
})

test_that("normalize_fileref lower-cases and strips macro references", {
  expect_equal(normalize_fileref("MyRef&suffix."), "myref")
})

test_that("include resolves cross-procedure macro path via suffix search", {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  peer_root <- file.path(dir, "peer_proc_root")
  peer_sas <- file.path(peer_root, "peer_proc", "2024")
  dir.create(peer_sas, recursive = TRUE)
  peer_include <- file.path(peer_sas, "util.sas")
  writeLines("/* cross-proc util */", peer_include)

  main_path <- file.path(dir, "main.sas")
  writeLines('%include "&prog/peer_proc/2024/util.sas";', main_path)

  analyzer <- SASLineageAnalyzer$new(dir)
  analyzer$.include_search_roots <- peer_root
  private_env <- analyzer$.__enclos_env__$private
  private_env$parse_filename_stmts(main_path)
  private_env$parse_include_stmts(main_path)
  key <- normalizePath(main_path, mustWork = FALSE)
  includes <- analyzer$file_includes[[key]]
  expect_length(includes, 1L)
  expect_equal(
    normalizePath(includes[[1]]$target, mustWork = FALSE),
    normalizePath(peer_include, mustWork = FALSE)
  )
})
