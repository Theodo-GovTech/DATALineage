# test-includes.R — Tests for includes.R (%include resolution)

test_that("substitute_macro_vars replaces known vars", {
  result <- substitute_macro_vars("&root/data/&file..sas",
    list(root = "/opt", file = "main"))
  expect_equal(result, "/opt/data/main.sas")
})

test_that("substitute_macro_vars leaves unknown vars", {
  result <- substitute_macro_vars("&root/data", list())
  expect_true(grepl("&root", result, fixed = TRUE))
})

test_that("resolve_include_target relative path", {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  inc <- file.path(dir, "sub.sas")
  writeLines("/* sub */", inc)
  main <- file.path(dir, "main.sas")

  target <- resolve_include_target("sub.sas", main)
  expect_false(is.null(target))
  expect_equal(normalizePath(target, mustWork = FALSE),
    normalizePath(inc, mustWork = FALSE))
})

test_that("get_file_events returns sorted events", {
  file_includes <- list()
  file_macro_definitions <- list()
  fp <- "/fake/test.sas"
  file_includes[[fp]] <- list(
    list(line = 5L, target = "/fake/inc.sas"),
    list(line = 1L, target = "/fake/inc2.sas")
  )
  file_macro_definitions[[fp]] <- list(
    list(name = "mk", line = 3L, file = fp, params = character(0), body = character(0))
  )
  events <- get_file_events(fp, file_includes, file_macro_definitions)
  expect_length(events, 3L)
  lines_vec <- vapply(events, `[[`, integer(1), "line")
  expect_equal(lines_vec, c(1L, 3L, 5L))
})

test_that("get_exported_macros_for_file detects cycle", {
  fp <- "/fake/cycle.sas"
  file_includes <- list()
  file_includes[[fp]] <- list(list(line = 1L, target = fp))
  file_macro_definitions <- list()
  cache <- list()
  result <- get_exported_macros_for_file(fp, cache, file_includes,
    file_macro_definitions)
  expect_true(is.list(result))
})

test_that("include resolves cross-procedure with let-substituted year", {
  dir <- tempfile("sas_test_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  peer_root <- file.path(dir, "peer_proc_root")
  peer_sas <- file.path(peer_root, "peer_proc", "2024")
  dir.create(peer_sas, recursive = TRUE)
  peer_include <- file.path(peer_sas, "report_24.sas")
  writeLines("/* cross-proc report */", peer_include)

  main_path <- file.path(dir, "main.sas")
  writeLines(c(
    "%let an_cours=2024;",
    "%let dir_an=%substr(&an_cours,3);",
    '%include "&prog/peer_proc/&an_cours/report_&dir_an..sas";'
  ), main_path)

  analyzer <- SASLineageAnalyzer$new(dir)
  analyzer$.include_search_roots <- peer_root
  private_env <- analyzer$.__enclos_env__$private
  private_env$collect_static_let_values(main_path)
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
