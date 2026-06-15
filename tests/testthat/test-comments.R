# test-comments.R — Tests for comments.R (handle_block_comments)

test_that("single-line block comment skips to next line", {
  lines <- c("/* only a comment */", "data out; set in1; run;")
  result <- handle_block_comments(lines, 1L)
  expect_true(result$should_continue)
  expect_equal(result$new_i, 2L)
  expect_null(result$replacement)
})

test_that("single-line comment with trailing code returns replacement", {
  lines <- c("/* comment */ data out; set in1; run;")
  result <- handle_block_comments(lines, 1L)
  expect_true(result$should_continue)
  expect_equal(result$new_i, 1L)
  expect_false(is.null(result$replacement))
  expect_true(grepl("data out;", result$replacement, ignore.case = TRUE))
})

test_that("multiline comment with trailing code", {
  lines <- c(
    "/* comment start",
    "still comment",
    "*/ data out; set in1; run;"
  )
  result <- handle_block_comments(lines, 1L)
  expect_true(result$should_continue)
  expect_equal(result$new_i, 3L)
  expect_false(is.null(result$replacement))
  expect_true(grepl("data out;", result$replacement, ignore.case = TRUE))
})

test_that("parse_sas_file ignores commented data step", {
  with_temp_sas_dir(
    list("test.sas" = paste0(
      "/* data fake_out;\n",
      "set fake_in;\n",
      "run; */\n",
      "data real_out;\n",
      "    set real_in;\n",
      "run;\n"
    )),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "test.sas"))
      datasets <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_false("fake_out" %in% datasets)
      expect_true("real_out" %in% datasets)
    }
  )
})

test_that("parse_sas_file parses code after multiline comment end", {
  with_temp_sas_dir(
    list("test.sas" = paste0(
      "/* start comment\n",
      "still comment\n",
      "*/ data real_out;\n",
      "set real_in;\n",
      "run;\n"
    )),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "test.sas"))
      datasets <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("real_out" %in% datasets)
    }
  )
})

test_that("parse_sas_file strips consecutive leading block comments", {
  with_temp_sas_dir(
    list("test.sas" = "/* c1 */ /* c2 */ data real_out; set real_in; run;\n"),
    function(dir) {
      analyzer <- SASLineageAnalyzer$new(dir)
      analyzer$parse_sas_file(file.path(dir, "test.sas"))
      datasets <- vapply(analyzer$dataset_operations, `[[`, character(1), "dataset")
      expect_true("real_out" %in% datasets)
    }
  )
})
