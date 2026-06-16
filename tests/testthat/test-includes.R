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

# ---------------------------------------------------------------------------
# Added coverage tests
# ---------------------------------------------------------------------------

test_that("resolve_include_target returns a normalized path for a missing non-macro file", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  target <- resolve_include_target("nonexistent_abc.sas", main)
  expect_false(is.null(target))
  expect_true(grepl("nonexistent_abc.sas", target, fixed = TRUE))
})

test_that("resolve_include_target finds a macro-path target by globbing search_roots", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  root <- file.path(dir, "searchroot")
  deep <- file.path(root, "proc", "2024")
  dir.create(deep, recursive = TRUE)
  tgt <- file.path(deep, "report_24.sas")
  writeLines("/* report */", tgt)

  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  target <- resolve_include_target(
    "&prog/proc/2024/report_24.sas", main,
    search_roots = root
  )
  expect_false(is.null(target))
  expect_equal(
    normalizePath(target, mustWork = FALSE),
    normalizePath(tgt, mustWork = FALSE)
  )
})

test_that("resolve_include_target returns NULL for an all-macro path with no glob match", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  root <- file.path(dir, "root")
  dir.create(root, recursive = TRUE)
  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  # Every segment is a pure macro reference; after dropping them no segment
  # remains, so the roots search short-circuits to NULL.
  expect_null(resolve_include_target("&a/&b.", main, search_roots = root))
})

test_that("resolve_include_target returns NULL when the search root does not exist", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  expect_null(resolve_include_target(
    "&a/x.sas", main,
    search_roots = file.path(dir, "does_not_exist")
  ))
})

test_that(".search_include_in_roots returns NULL when the path has no usable segment", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_null(DATALineage:::.search_include_in_roots("///", dir))
})

test_that("resolve_include_target returns NULL when a macro path matches nothing", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  root <- file.path(dir, "searchroot")
  dir.create(root, recursive = TRUE)
  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  target <- resolve_include_target(
    "&prog/nonexistent_xyz.sas", main,
    search_roots = root
  )
  expect_null(target)
})

test_that("resolve_fileref_include_target uses the latest visible local fileref def", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  tgt <- file.path(dir, "f.sas")
  writeLines("/* f */", tgt)
  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  defs <- list()
  defs[[normalizePath(main, mustWork = FALSE)]] <- list(
    list(line = 1L, fileref = "myref", path = tgt),
    list(line = 2L, fileref = "myref", path = tgt)
  )
  target <- resolve_fileref_include_target(
    including_file = main, include_line = 5L, fileref = "myref",
    file_fileref_definitions = defs
  )
  expect_equal(
    normalizePath(target, mustWork = FALSE),
    normalizePath(tgt, mustWork = FALSE)
  )
})

test_that("resolve_fileref_include_target reads defs from an environment and skips non-matching defs", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  tgt <- file.path(dir, "f.sas")
  writeLines("/* f */", tgt)
  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  env <- new.env()
  assign(normalizePath(main, mustWork = FALSE), list(
    list(line = 10L, fileref = "myref", path = tgt), # at/after include line: skipped
    list(line = 1L, fileref = "other", path = tgt),  # wrong fileref: skipped
    list(line = 2L, fileref = "myref", path = tgt)   # latest valid def
  ), envir = env)

  target <- resolve_fileref_include_target(
    including_file = main, include_line = 5L, fileref = "myref",
    file_fileref_definitions = env
  )
  expect_equal(
    normalizePath(target, mustWork = FALSE),
    normalizePath(tgt, mustWork = FALSE)
  )
})

test_that("resolve_fileref_include_target treats a non-list non-environment defs arg as empty", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  expect_null(resolve_fileref_include_target(
    including_file = main, include_line = 5L, fileref = "myref",
    file_fileref_definitions = 42L
  ))
})

test_that("resolve_fileref_include_target falls back to a global filename ref entry", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  tgt <- file.path(dir, "f.sas")
  writeLines("/* f */", tgt)
  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  gfr <- list(gref = list("/some/other.sas", 3L, tgt))
  target <- resolve_fileref_include_target(
    including_file = main, include_line = 5L, fileref = "gref",
    file_fileref_definitions = list(),
    global_filename_refs = gfr
  )
  expect_equal(
    normalizePath(target, mustWork = FALSE),
    normalizePath(tgt, mustWork = FALSE)
  )
})

test_that("resolve_fileref_include_target ignores a global ref defined later in the same file", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  tgt <- file.path(dir, "f.sas")
  writeLines("/* f */", tgt)
  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  gfr <- list(gref = list(main, 9L, tgt))
  target <- resolve_fileref_include_target(
    including_file = main, include_line = 5L, fileref = "gref",
    file_fileref_definitions = list(),
    global_filename_refs = gfr
  )
  expect_null(target)
})

test_that("resolve_fileref_include_target accepts a scalar character global ref", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  tgt <- file.path(dir, "f.sas")
  writeLines("/* f */", tgt)
  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  gfr <- list(gref = tgt)
  target <- resolve_fileref_include_target(
    including_file = main, include_line = 5L, fileref = "gref",
    file_fileref_definitions = list(),
    global_filename_refs = gfr
  )
  expect_equal(
    normalizePath(target, mustWork = FALSE),
    normalizePath(tgt, mustWork = FALSE)
  )
})

test_that("resolve_fileref_include_target returns NULL with neither def nor global ref", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)

  target <- resolve_fileref_include_target(
    including_file = main, include_line = 5L, fileref = "none",
    file_fileref_definitions = list()
  )
  expect_null(target)
})

test_that("parse_include_statements skips over multi-line block comments", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  inc <- file.path(dir, "inc.sas")
  writeLines("/* inc */", inc)
  src <- file.path(dir, "src.sas")
  writeLines(c(
    "/* a block comment",
    "spanning lines */",
    '%include "inc.sas";'
  ), src)

  includes <- parse_include_statements(src, list())
  expect_length(includes, 1L)
  expect_equal(basename(includes[[1]]$target), "inc.sas")
})

test_that("parse_include_statements re-parses code that follows an inline block comment", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  inc <- file.path(dir, "inc.sas")
  writeLines("/* inc */", inc)
  src <- file.path(dir, "src.sas")
  # The %include shares a line with a leading block comment, so the comment
  # handler hands back the remainder for re-parsing.
  writeLines('/* lead comment */ %include "inc.sas";', src)

  includes <- parse_include_statements(src, list())
  expect_length(includes, 1L)
  expect_equal(basename(includes[[1]]$target), "inc.sas")
})

test_that("parse_include_statements resolves both quoted and fileref includes", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  incdir <- file.path(dir, "incs")
  dir.create(incdir, recursive = TRUE)
  qtarget <- file.path(incdir, "q.sas")
  writeLines("/* q */", qtarget)
  ftarget <- file.path(incdir, "f.sas")
  writeLines("/* f */", ftarget)

  src <- file.path(dir, "src.sas")
  writeLines(c(
    '%include "incs/q.sas";',
    "%include myref;"
  ), src)

  defs <- list()
  defs[[normalizePath(src, mustWork = FALSE)]] <- list(
    list(line = 0L, fileref = "myref", path = ftarget)
  )
  includes <- parse_include_statements(src, defs)
  expect_length(includes, 2L)
  bases <- vapply(includes, function(x) {
    basename(x$target)
  }, character(1))
  expect_true("q.sas" %in% bases)
  expect_true("f.sas" %in% bases)
})

test_that("get_file_events reads includes and macro defs from environments", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  main <- file.path(dir, "main.sas")
  writeLines("/* main */", main)
  fp <- normalizePath(main, mustWork = FALSE)

  file_includes <- new.env()
  file_macro_definitions <- new.env()
  assign(fp, list(list(line = 2L, target = "/x/a.sas")), envir = file_includes)
  assign(fp, list(list(name = "m1", line = 1L)), envir = file_macro_definitions)

  events <- get_file_events(main, file_includes, file_macro_definitions)
  expect_length(events, 2L)
  lines_vec <- vapply(events, `[[`, integer(1), "line")
  expect_equal(lines_vec, c(1L, 2L))
})

test_that("get_file_events drops events at or beyond max_line", {
  fp <- "/fake/test.sas"
  file_includes <- list()
  file_includes[[normalizePath(fp, mustWork = FALSE)]] <- list(
    list(line = 1L, target = "/fake/inc.sas"),
    list(line = 9L, target = "/fake/late.sas")
  )
  file_macro_definitions <- list()
  file_macro_definitions[[normalizePath(fp, mustWork = FALSE)]] <- list(
    list(name = "early", line = 2L),
    list(name = "late", line = 8L)
  )
  events <- get_file_events(fp, file_includes, file_macro_definitions, max_line = 5L)
  kinds_lines <- vapply(events, `[[`, integer(1), "line")
  expect_equal(sort(kinds_lines), c(1L, 2L))
})

test_that("get_exported_macros_for_file merges included exports and local defs", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  parent_file <- file.path(dir, "parent.sas")
  child_file <- file.path(dir, "child.sas")
  writeLines("/* parent */", parent_file)
  writeLines("/* child */", child_file)
  parent <- normalizePath(parent_file, mustWork = FALSE)
  child <- normalizePath(child_file, mustWork = FALSE)

  file_includes <- list()
  file_includes[[parent]] <- list(list(line = 1L, target = child))
  file_macro_definitions <- list()
  file_macro_definitions[[child]] <- list(
    list(name = "childmac", line = 1L, body = character(0))
  )
  file_macro_definitions[[parent]] <- list(
    list(name = "parentmac", line = 2L, body = character(0))
  )

  cache <- list()
  exported <- get_exported_macros_for_file(
    parent, cache, file_includes, file_macro_definitions
  )
  expect_true("childmac" %in% names(exported))
  expect_true("parentmac" %in% names(exported))
})

test_that("get_exported_macros_for_file uses an environment cache and returns it on re-entry", {
  dir <- tempfile("inc_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  pf <- file.path(dir, "p.sas")
  writeLines("/* p */", pf)
  parent <- normalizePath(pf, mustWork = FALSE)

  file_includes <- list()
  file_macro_definitions <- list()
  file_macro_definitions[[parent]] <- list(
    list(name = "mac", line = 1L, body = character(0))
  )

  cache <- new.env()
  first <- get_exported_macros_for_file(
    parent, cache, file_includes, file_macro_definitions
  )
  expect_true("mac" %in% names(first))
  # Second call must short-circuit to the cached value stored in the env.
  expect_true(exists(parent, envir = cache, inherits = FALSE))
  second <- get_exported_macros_for_file(
    parent, cache, file_includes, file_macro_definitions
  )
  expect_identical(second, first)
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
