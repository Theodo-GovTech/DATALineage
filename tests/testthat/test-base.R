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
