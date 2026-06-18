# test-operations_graph_nodes.R — Unit tests for node-id, style and txt-escape
# helpers in operations_graph_nodes.R.

# ===========================================================================
# make_node_id: leading-digit sanitization (line 140)
# ===========================================================================
test_that("make_node_id: sanitized id starting with digit gets op_ prefix", {
  node <- new_operation_node(
    dataset = "9out", file_path = "9file.sas", line_number = 3,
    operation_type = "DATA", input_datasets = character(0), depth = 0
  )
  nid <- make_node_id(node, "9file.sas")
  expect_true(startsWith(nid, "op_"))
  expect_equal(nid, "op_9out_9file_sas_3")
})

# ===========================================================================
# get_operation_style: each colour-selection branch (lines 152-163)
# ===========================================================================
.style_node <- function(dataset, operation_type) {
  new_operation_node(
    dataset = dataset, file_path = "f.sas", line_number = 1,
    operation_type = operation_type, input_datasets = character(0), depth = 0
  )
}

test_that("get_operation_style: target dataset uses the target colour", {
  node <- .style_node(dataset = "term", operation_type = "DATA")
  style <- get_operation_style(node, target_datasets = "term")
  expect_true(grepl("#ffcdd2", style, fixed = TRUE))
  expect_true(grepl("#f44336", style, fixed = TRUE))
})

test_that("get_operation_style: known operation type uses its mapped colour", {
  node <- .style_node(dataset = "x", operation_type = "PROC SQL")
  style <- get_operation_style(node, target_datasets = character(0))
  expect_true(grepl("#e1bee7", style, fixed = TRUE))
  expect_true(grepl("#7b1fa2", style, fixed = TRUE))
})

test_that("get_operation_style: unknown PROC type falls back to default proc colour", {
  node <- .style_node(dataset = "x", operation_type = "PROC MEANS")
  style <- get_operation_style(node, target_datasets = character(0))
  expect_true(grepl("#f5f5f5", style, fixed = TRUE))
  expect_true(grepl("#616161", style, fixed = TRUE))
})

test_that("get_operation_style: non-PROC unknown type falls back to DATA colour", {
  node <- .style_node(dataset = "x", operation_type = "MERGE")
  style <- get_operation_style(node, target_datasets = character(0))
  # The else branch resolves to the DATA entry of .OG_OPERATION_COLORS.
  expect_true(grepl("#bbdefb", style, fixed = TRUE))
  expect_true(grepl("#1976d2", style, fixed = TRUE))
})

# ===========================================================================
# escape_txt_value: quoting branch (line 176)
# ===========================================================================
test_that("escape_txt_value: value with a space is quoted", {
  expect_equal(escape_txt_value("has space"), '"has space"')
})

test_that("escape_txt_value: value with an equals sign is quoted", {
  expect_equal(escape_txt_value("a=b"), '"a=b"')
})

test_that("escape_txt_value: plain value is returned unquoted", {
  expect_equal(escape_txt_value("plain"), "plain")
})
