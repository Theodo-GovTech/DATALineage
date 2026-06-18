# test-base.R — Tests for base.R utility functions

# --- TestCleanDatasetName ---

test_that("clean_dataset_name: simple name", {
  expect_equal(clean_dataset_name("dataset1"), "dataset1")
})

test_that("clean_dataset_name: with library prefix", {
  expect_equal(clean_dataset_name("work.dataset1"), "dataset1")
})

test_that("clean_dataset_name: with options", {
  expect_equal(clean_dataset_name("dataset1(keep=a b c)"), "dataset1")
})

test_that("clean_dataset_name: with nested options", {
  expect_equal(clean_dataset_name("dataset1(in=a where=(x=1))"), "dataset1")
})

test_that("clean_dataset_name: nested where then drop (regression)", {
  result <- clean_dataset_name(
    'tabdial5(where=(finess ne "" and type ne "") drop=_type_ _freq_)'
  )
  expect_equal(result, "tabdial5")
})

test_that("clean_dataset_name: library prefix + nested options", {
  result <- clean_dataset_name("work.tabdial5(where=(a>1) drop=_type_)")
  expect_equal(result, "tabdial5")
})

test_that("clean_dataset_name: special chars stripped", {
  expect_equal(clean_dataset_name("dataset-name!"), "datasetname")
})

test_that("clean_dataset_name: uppercase to lowercase", {
  expect_equal(clean_dataset_name("DATASET1"), "dataset1")
})

test_that("clean_dataset_name: empty string returns NULL", {
  expect_null(clean_dataset_name(""))
})

test_that("clean_dataset_name: infile prefix preserved", {
  expect_equal(clean_dataset_name("INFILE:fileref"), "infile:fileref")
})

# --- TestExpandNumberedRange ---

test_that("expand_numbered_range: simple range", {
  expect_equal(expand_numbered_range("s1-s4"), c("s1", "s2", "s3", "s4"))
})

test_that("expand_numbered_range: single element range", {
  expect_equal(expand_numbered_range("s2-s2"), "s2")
})

test_that("expand_numbered_range: zero-padded range", {
  expect_equal(expand_numbered_range("s01-s04"), c("s01", "s02", "s03", "s04"))
})

test_that("expand_numbered_range: not a range", {
  expect_equal(expand_numbered_range("s1"), "s1")
  expect_equal(expand_numbered_range("dataset"), "dataset")
})

test_that("expand_numbered_range: mismatched prefix not expanded", {
  expect_equal(expand_numbered_range("foo1-bar4"), "foo1-bar4")
})

test_that("expand_numbered_range: descending range not expanded", {
  expect_equal(expand_numbered_range("s4-s1"), "s4-s1")
})

test_that("expand_numbered_range: library-qualified range", {
  expect_equal(
    expand_numbered_range("work.s1-work.s3"),
    c("work.s1", "work.s2", "work.s3")
  )
})

# --- TestParseDatasetNamesWithParens ---

test_that("parse_dataset_names_with_parens: set with numbered range", {
  expect_equal(
    parse_dataset_names_with_parens("s1-s4"),
    c("s1", "s2", "s3", "s4")
  )
})

test_that("parse_dataset_names_with_parens: multiple with range", {
  expect_equal(
    parse_dataset_names_with_parens("foo s1-s3 bar"),
    c("foo", "s1", "s2", "s3", "bar")
  )
})

test_that("parse_dataset_names_with_parens: range with options", {
  expect_equal(
    parse_dataset_names_with_parens("s1-s3(keep=x)"),
    c("s1", "s2", "s3")
  )
})

# --- TestDeduplicateList ---

test_that("deduplicate_list: no duplicates", {
  expect_equal(deduplicate_list(c("a", "b", "c")), c("a", "b", "c"))
})

test_that("deduplicate_list: with duplicates", {
  expect_equal(deduplicate_list(c("a", "b", "a", "c", "b")), c("a", "b", "c"))
})

test_that("deduplicate_list: preserves order", {
  expect_equal(deduplicate_list(c("c", "a", "b", "a")), c("c", "a", "b"))
})

test_that("deduplicate_list: empty vector", {
  expect_equal(deduplicate_list(character(0)), character(0))
})

# --- fix_operation_line_number ---

test_that("fix_operation_line_number adjusts both line_number and end_line", {
  original <- new_operation(
    dataset = "test",
    operation_type = "DATA",
    file = "test.sas",
    line_number = 5L,
    code_snippet = "data test; run;",
    input_datasets = character(0),
    end_line = 10L
  )
  fixed <- fix_operation_line_number(original, 50L)
  expect_equal(fixed$line_number, 50L)
  expect_equal(fixed$end_line, 55L)
  expect_true(fixed$end_line >= fixed$line_number)
})

# --- is_input_lib ---

test_that("is_input_lib recognises a name from the source-lib allowlist", {
  expect_true(is_input_lib("all"))
  expect_true(is_input_lib("nom_pmsi"))
})

test_that("is_input_lib is case-insensitive", {
  expect_true(is_input_lib("DATAIN"))
})

test_that("is_input_lib matches the period-suffixed source-lib pattern", {
  expect_true(is_input_lib("mco23bd"))
  expect_true(is_input_lib("had07bd"))
})

test_that("is_input_lib rejects an ordinary work library", {
  expect_false(is_input_lib("work"))
  expect_false(is_input_lib("mco23"))
})

# --- append_to / env_get (defaultdict(list) replacement) ---

test_that("append_to creates a one-element list for a fresh key", {
  e <- new.env()
  append_to(e, key = "k", value = "first")
  expect_equal(get("k", envir = e), list("first"))
})

test_that("append_to appends to an existing key preserving order", {
  e <- new.env()
  append_to(e, key = "k", value = "first")
  append_to(e, key = "k", value = "second")
  expect_equal(get("k", envir = e), list("first", "second"))
})

test_that("env_get returns the stored value when the key exists", {
  e <- new.env()
  assign("k", list("v"), envir = e)
  expect_equal(DATALineage:::env_get(e, key = "k"), list("v"))
})

test_that("env_get returns the default when the key is absent", {
  e <- new.env()
  expect_equal(DATALineage:::env_get(e, key = "missing"), list())
  expect_null(DATALineage:::env_get(e, key = "missing", default = NULL))
})

# --- clean_dataset_name: macro-dot branch (line 124) ---

test_that("clean_dataset_name keeps a macro-variable dot reference intact", {
  expect_equal(clean_dataset_name("&lib.dataset"), "&lib.dataset")
})

test_that("clean_dataset_name strips disallowed chars around a macro ref", {
  expect_equal(clean_dataset_name("&lib.data-set!"), "&lib.dataset")
})

# --- parse_dataset_names_with_parens: trailing-whitespace break (line 178) ---

test_that("parse_dataset_names_with_parens handles trailing whitespace", {
  expect_equal(
    parse_dataset_names_with_parens("foo bar   "),
    c("foo", "bar")
  )
})

test_that("parse_dataset_names_with_parens skips an excluded keyword", {
  expect_equal(
    parse_dataset_names_with_parens("foo end bar", exclude_keywords = "end"),
    c("foo", "bar")
  )
})

# --- strip_sas_block_comments ---

test_that("strip_sas_block_comments removes a single block comment", {
  expect_equal(
    strip_sas_block_comments("data a; /* note */ set b;"),
    "data a;   set b;"
  )
})

test_that("strip_sas_block_comments leaves comment-free text unchanged", {
  expect_equal(strip_sas_block_comments("data a; set b;"), "data a; set b;")
})

test_that("strip_sas_block_comments handles an unterminated comment", {
  out <- strip_sas_block_comments("data a; /* unterminated")
  expect_true(startsWith(out, "data a; "))
  expect_false(grepl("unterminated", out))
})

# --- find_statement_end_semicolon ---

test_that("find_statement_end_semicolon finds a top-level semicolon", {
  expect_equal(find_statement_end_semicolon("data a; set b;"), 7L)
})

test_that("find_statement_end_semicolon ignores a semicolon inside parens", {
  expect_equal(
    find_statement_end_semicolon("set a(where=(x=1; y=2)); run"),
    nchar("set a(where=(x=1; y=2));")
  )
})

test_that("find_statement_end_semicolon ignores a semicolon in double quotes", {
  expect_equal(
    find_statement_end_semicolon('put "a;b"; rest'),
    nchar('put "a;b";')
  )
})

test_that("find_statement_end_semicolon ignores a semicolon in single quotes", {
  expect_equal(
    find_statement_end_semicolon("put 'a;b'; rest"),
    nchar("put 'a;b';")
  )
})

test_that("find_statement_end_semicolon returns -1 when none present", {
  expect_equal(find_statement_end_semicolon("data a set b"), -1L)
})

# --- scan_macro_var_refs: builtin / dedup skip (line 335) ---

test_that("scan_macro_var_refs returns ordered de-duplicated names", {
  expect_equal(
    scan_macro_var_refs(c("data &year.; set &lib..&year.;")),
    c("year", "lib")
  )
})

test_that("scan_macro_var_refs drops SAS built-in automatic vars", {
  expect_equal(
    scan_macro_var_refs(c("%if &syserr. then; data &real.; run;")),
    "real"
  )
})

test_that("scan_macro_var_refs returns empty when no refs present", {
  expect_equal(scan_macro_var_refs(c("data a; run;")), character(0))
})
