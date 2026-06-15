# test-operations_graph.R — Unit tests for the operations graph generator
# Ported from: tests/unit/test_generate_operations_graph.py

# ===========================================================================
# Helper: create a generator with minimal manifest in a temp dir
# ===========================================================================
make_test_generator <- function(dir, target = "test", ops = list(),
                                 entrypoint = "test.sas") {
  manifest <- list(target_dataset = target, operations = ops)
  manifest_path <- file.path(dir, "manifest.json")
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), manifest_path)
  OperationsGraphGenerator$new(dir, entrypoint, manifest_path)
}

# ===========================================================================
# TestOperationNode
# ===========================================================================
test_that("OperationNode: identity based on file_path, line_number, dataset", {
  node1 <- new_operation_node(
    dataset = "test", file_path = "file.sas", line_number = 10,
    operation_type = "DATA", input_datasets = character(0), depth = 0
  )
  node2 <- new_operation_node(
    dataset = "test", file_path = "file.sas", line_number = 10,
    operation_type = "DATA", input_datasets = "other", depth = 1
  )
  # Same file/line/dataset should have same identity

  expect_equal(node_identity(node1), node_identity(node2))
})

test_that("OperationNode: different line_number -> different identity", {
  node1 <- new_operation_node(
    dataset = "test", file_path = "file.sas", line_number = 10,
    operation_type = "DATA", input_datasets = character(0), depth = 0
  )
  node2 <- new_operation_node(
    dataset = "test", file_path = "file.sas", line_number = 20,
    operation_type = "DATA", input_datasets = character(0), depth = 0
  )
  expect_false(node_identity(node1) == node_identity(node2))
})

# ===========================================================================
# TestMakeNodeId
# ===========================================================================
test_that("make_node_id: simple node produces expected id", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    nid <- make_node_id(node, "test.sas")
    expect_equal(nid, "output_test_sas_10")
  })
})

test_that("make_node_id: special chars are sanitized", {
  node <- new_operation_node(
    dataset = "infile:data", file_path = "test-file.sas", line_number = 10,
    operation_type = "INFILE", input_datasets = character(0), depth = 0
  )
  nid <- make_node_id(node, "test-file.sas")
  expect_false(grepl(":", nid, fixed = TRUE))
  expect_false(grepl("-", nid, fixed = TRUE))
})

test_that("make_node_id: dataset starting with digit gets op_ prefix", {
  node <- new_operation_node(
    dataset = "123data", file_path = "test.sas", line_number = 10,
    operation_type = "DATA", input_datasets = character(0), depth = 0
  )
  nid <- make_node_id(node, "test.sas")
  expect_true(startsWith(nid, "op_"))
})

# ===========================================================================
# TestFilenameMatches
# ===========================================================================
test_that("filename_matches: exact match", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_true(gen$.__enclos_env__$private$filename_matches("test.sas", "test.sas"))
  })
})

test_that("filename_matches: macro variable match", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_true(gen$.__enclos_env__$private$filename_matches(
      "mco_tt24_macro_util.sas", "mco_tt&dir_an._macro_util.sas"
    ))
  })
})

test_that("filename_matches: multiple macro variables", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_true(gen$.__enclos_env__$private$filename_matches(
      "mco_2024_data_2024.sas", "mco_&year._data_&an_cours..sas"
    ))
  })
})

test_that("filename_matches: no match", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_false(gen$.__enclos_env__$private$filename_matches(
      "other_file.sas", "mco_tt24_macro_util.sas"
    ))
  })
})

# ===========================================================================
# TestIsMacroDefinitionStart
# ===========================================================================
test_that("is_macro_definition_start: simple macro", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_true(gen$.__enclos_env__$private$is_macro_definition_start("%macro test;"))
  })
})

test_that("is_macro_definition_start: macro with params", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_true(gen$.__enclos_env__$private$is_macro_definition_start(
      "%macro test(param1, param2);"
    ))
  })
})

test_that("is_macro_definition_start: leading whitespace", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_true(gen$.__enclos_env__$private$is_macro_definition_start("  %macro test;"))
  })
})

test_that("is_macro_definition_start: macro call is NOT detected", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_false(gen$.__enclos_env__$private$is_macro_definition_start("%test;"))
  })
})

test_that("is_macro_definition_start: %mend is NOT detected", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_false(gen$.__enclos_env__$private$is_macro_definition_start("%mend;"))
  })
})

# ===========================================================================
# TestParseMacroCall
# ===========================================================================
test_that("parse_macro_call: simple call", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_equal(gen$.__enclos_env__$private$parse_macro_call("%mymacro;"), "mymacro")
  })
})

test_that("parse_macro_call: call with params", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_equal(
      gen$.__enclos_env__$private$parse_macro_call("%mymacro(param1, param2);"),
      "mymacro"
    )
  })
})

test_that("parse_macro_call: leading whitespace", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_equal(gen$.__enclos_env__$private$parse_macro_call("  %mymacro;"), "mymacro")
  })
})

test_that("parse_macro_call: skip macro definition", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_null(gen$.__enclos_env__$private$parse_macro_call("%macro test;"))
  })
})

test_that("parse_macro_call: skip %mend", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_null(gen$.__enclos_env__$private$parse_macro_call("%mend;"))
  })
})

test_that("parse_macro_call: skip control flow", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    p <- gen$.__enclos_env__$private
    expect_null(p$parse_macro_call("%if condition;"))
    expect_null(p$parse_macro_call("%do;"))
    expect_null(p$parse_macro_call("%else;"))
    expect_null(p$parse_macro_call("%end;"))
    expect_null(p$parse_macro_call("%let var = 1;"))
  })
})

test_that("parse_macro_call: skip built-in macros", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    p <- gen$.__enclos_env__$private
    expect_null(p$parse_macro_call("%sysfunc(date());"))
    expect_null(p$parse_macro_call("%eval(1+1)"))
    expect_null(p$parse_macro_call("%str(text)"))
  })
})

test_that("parse_macro_call: plain SAS line returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_null(gen$.__enclos_env__$private$parse_macro_call("data test; run;"))
  })
})

test_that("parse_macro_call: empty line returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_null(gen$.__enclos_env__$private$parse_macro_call(""))
  })
})

# ===========================================================================
# TestParseInclude
# ===========================================================================
test_that("parse_include: non-include line returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_null(gen$.__enclos_env__$private$parse_include("data test; run;"))
  })
})

test_that("parse_include: fileref with no alias returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_null(gen$.__enclos_env__$private$parse_include("%include myfile;"))
  })
})

test_that("parse_include: known fileref resolves", {
  with_temp_sas_dir(list(
    "test.sas" = "/* test */",
    "included.sas" = "/* test */"
  ), function(dir) {
    gen <- make_test_generator(dir)
    gen$filename_aliases[["myfile"]] <- file.path(dir, "included.sas")
    result <- gen$.__enclos_env__$private$parse_include("%include myfile;")
    expect_equal(result, file.path(dir, "included.sas"))
  })
})

test_that("parse_include: skip control flow keywords", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_null(gen$.__enclos_env__$private$parse_include("%include if;"))
    expect_null(gen$.__enclos_env__$private$parse_include("%include do;"))
  })
})

test_that("parse_include: fileref resolved by glob", {
  with_temp_sas_dir(list(
    "test.sas" = "/* test */",
    "utils_helper.sas" = "/* utils */"
  ), function(dir) {
    gen <- make_test_generator(dir)
    result <- gen$.__enclos_env__$private$parse_include("%include utils_helper;")
    expect_equal(result, file.path(dir, "utils_helper.sas"))
  })
})

test_that("parse_include: literal path double quote", {
  with_temp_sas_dir(list(
    "test.sas" = "/* test */",
    "target.sas" = "/* target */"
  ), function(dir) {
    gen <- make_test_generator(dir)
    result <- gen$.__enclos_env__$private$parse_include(
      sprintf('%%include "&prog/target.sas";')
    )
    expect_equal(result, file.path(dir, "target.sas"))
  })
})

test_that("parse_include: literal path single quote", {
  with_temp_sas_dir(list(
    "test.sas" = "/* test */",
    "target.sas" = "/* target */"
  ), function(dir) {
    gen <- make_test_generator(dir)
    result <- gen$.__enclos_env__$private$parse_include(
      "%include '&prog/target.sas';"
    )
    expect_equal(result, file.path(dir, "target.sas"))
  })
})

test_that("parse_include: multiple filerefs returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    expect_null(gen$.__enclos_env__$private$parse_include("%include file1 file2;"))
  })
})

# ===========================================================================
# TestParseMacroDefinitions
# ===========================================================================
test_that("parse_macro_defs: simple macro", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c("%macro test;", "  data output; run;", "%mend;")
    gen$.__enclos_env__$private$parse_macro_defs("/test.sas", lines)
    expect_true("test" %in% names(gen$macro_definitions))
    macro <- gen$macro_definitions[["test"]][[1]]
    expect_equal(macro$start_line, 1L)
    expect_equal(macro$end_line, 3L)
  })
})

test_that("parse_macro_defs: macro with params", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c("%macro test(param1, param2);", "  data output; run;", "%mend;")
    gen$.__enclos_env__$private$parse_macro_defs("/test.sas", lines)
    macro <- gen$macro_definitions[["test"]][[1]]
    expect_equal(macro$params, c("param1", "param2"))
  })
})

test_that("parse_macro_defs: nested macros", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c(
      "%macro outer;",
      "  %macro inner;",
      "    data x; run;",
      "  %mend;",
      "  %inner;",
      "%mend;"
    )
    gen$.__enclos_env__$private$parse_macro_defs("/test.sas", lines)
    outer <- gen$macro_definitions[["outer"]][[1]]
    inner <- gen$macro_definitions[["inner"]][[1]]
    expect_equal(outer$start_line, 1L)
    expect_equal(outer$end_line, 6L)
    expect_equal(inner$start_line, 2L)
    expect_equal(inner$end_line, 4L)
  })
})

test_that("parse_macro_defs: inline empty macro", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c("%macro empty; %mend;", "data x; run;")
    gen$.__enclos_env__$private$parse_macro_defs("/test.sas", lines)
    macro <- gen$macro_definitions[["empty"]][[1]]
    expect_equal(macro$start_line, 1L)
    expect_equal(macro$end_line, 1L)
  })
})

# ===========================================================================
# TestSkipToMacroEnd
# ===========================================================================
test_that("skip_to_macro_end: simple skip", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c("%macro test;", "  data x; run;", "%mend;", "data y; run;")
    result <- gen$.__enclos_env__$private$skip_to_macro_end(lines, 1L)
    expect_equal(result, 4L)  # Index after %mend (1-based)
  })
})

test_that("skip_to_macro_end: inline macro", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c("%macro empty; %mend;", "data y; run;")
    result <- gen$.__enclos_env__$private$skip_to_macro_end(lines, 1L)
    expect_equal(result, 2L)  # Next line
  })
})

test_that("skip_to_macro_end: nested macros", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c(
      "%macro outer;",
      "  %macro inner;",
      "    data x; run;",
      "  %mend;",
      "  %inner;",
      "%mend;",
      "data y; run;"
    )
    result <- gen$.__enclos_env__$private$skip_to_macro_end(lines, 1L)
    expect_equal(result, 7L)  # Index after outer %mend
  })
})

# ===========================================================================
# TestGenerateDot
# ===========================================================================
test_that("generate_dot: empty graph", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    dot <- gen$generate_dot()
    expect_true(grepl("digraph operations", dot, fixed = TRUE))
    expect_true(grepl("rankdir=TB", dot, fixed = TRUE))
  })
})

test_that("generate_dot: single node", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node)
    dot <- gen$generate_dot()
    expect_true(grepl("output_test_sas_10", dot, fixed = TRUE))
    expect_true(grepl('label="DATA\\noutput\\ntest.sas:10"', dot, fixed = TRUE))
  })
})

test_that("generate_dot: target node styling (red)", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node)
    dot <- gen$generate_dot()
    expect_true(grepl('#ffcdd2', dot, fixed = TRUE))
  })
})

test_that("generate_dot: INFILE node styling (green)", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "myfile", file_path = "test.sas", line_number = 10,
      operation_type = "INFILE", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node)
    dot <- gen$generate_dot()
    expect_true(grepl('#c8e6c9', dot, fixed = TRUE))
  })
})

test_that("generate_dot: edge generation", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node1 <- new_operation_node(
      dataset = "input", file_path = "test.sas", line_number = 5,
      operation_type = "DATA", input_datasets = character(0), depth = 1
    )
    node2 <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = "input", depth = 0
    )
    gen$graph_nodes <- list(node1, node2)
    gen$graph_edges <- list(list(node1, node2))
    dot <- gen$generate_dot()
    expect_true(grepl("input_test_sas_5 -> output_test_sas_10", dot, fixed = TRUE))
  })
})

test_that("generate_dot: no duplicate nodes", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node, node)
    dot <- gen$generate_dot()
    count <- length(gregexpr("output_test_sas_10 \\[label=", dot)[[1]])
    if (gregexpr("output_test_sas_10 \\[label=", dot)[[1]][1] == -1L) count <- 0L
    expect_equal(count, 1L)
  })
})

test_that("generate_dot: no duplicate edges", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node1 <- new_operation_node(
      dataset = "input", file_path = "test.sas", line_number = 5,
      operation_type = "DATA", input_datasets = character(0), depth = 1
    )
    node2 <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = "input", depth = 0
    )
    gen$graph_nodes <- list(node1, node2)
    gen$graph_edges <- list(list(node1, node2), list(node1, node2))
    dot <- gen$generate_dot()
    matches <- gregexpr("input_test_sas_5 -> output_test_sas_10", dot, fixed = TRUE)[[1]]
    count <- if (matches[1] == -1L) 0L else length(matches)
    expect_equal(count, 1L)
  })
})

# ===========================================================================
# TestEscapeTxtValue
# ===========================================================================
test_that("escape_txt_value: simple value", {
  expect_equal(escape_txt_value("DATA"), "DATA")
})

test_that("escape_txt_value: value with space", {
  expect_equal(escape_txt_value("PROC SQL"), '"PROC SQL"')
})

test_that("escape_txt_value: value with equals", {
  expect_equal(escape_txt_value("key=value"), '"key=value"')
})

test_that("escape_txt_value: value with space and equals", {
  expect_equal(escape_txt_value("key = value"), '"key = value"')
})

# ===========================================================================
# TestGenerateTxt
# ===========================================================================
test_that("generate_txt: empty graph", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    txt <- gen$generate_txt()
    expect_true(grepl("# Nodes", txt, fixed = TRUE))
    expect_true(grepl("# Edges", txt, fixed = TRUE))
  })
})

test_that("generate_txt: single node", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node)
    txt <- gen$generate_txt()
    expect_true(grepl("N output_test_sas_10", txt, fixed = TRUE))
    expect_true(grepl("type=DATA", txt, fixed = TRUE))
    expect_true(grepl("dataset=output", txt, fixed = TRUE))
    expect_true(grepl("file=test.sas", txt, fixed = TRUE))
    expect_true(grepl("start_line=10", txt, fixed = TRUE))
  })
})

test_that("generate_txt: target node gets target=true", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node)
    txt <- gen$generate_txt()
    expect_true(grepl("target=true", txt, fixed = TRUE))
  })
})

test_that("generate_txt: non-target node does not get target=true", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "other", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node)
    txt <- gen$generate_txt()
    expect_false(grepl("target=true", txt, fixed = TRUE))
  })
})

test_that("generate_txt: PROC SQL type is quoted", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "result", file_path = "test.sas", line_number = 10,
      operation_type = "PROC SQL", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node)
    txt <- gen$generate_txt()
    expect_true(grepl('type="PROC SQL"', txt, fixed = TRUE))
  })
})

test_that("generate_txt: resolved_path included for INFILE", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "infile:mydata", file_path = "test.sas", line_number = 10,
      operation_type = "INFILE", input_datasets = character(0), depth = 0,
      resolved_path = "/path/to/file.dat"
    )
    gen$graph_nodes <- list(node)
    txt <- gen$generate_txt()
    expect_true(grepl("resolved_path=/path/to/file.dat", txt, fixed = TRUE))
  })
})

test_that("generate_txt: edge generation", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node1 <- new_operation_node(
      dataset = "input", file_path = "test.sas", line_number = 5,
      operation_type = "DATA", input_datasets = character(0), depth = 1
    )
    node2 <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = "input", depth = 0
    )
    gen$graph_nodes <- list(node1, node2)
    gen$graph_edges <- list(list(node1, node2))
    txt <- gen$generate_txt()
    expect_true(grepl("E input_test_sas_5 -> output_test_sas_10", txt, fixed = TRUE))
  })
})

test_that("generate_txt: no duplicate nodes", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node, node)
    txt <- gen$generate_txt()
    matches <- gregexpr("N output_test_sas_10", txt, fixed = TRUE)[[1]]
    count <- if (matches[1] == -1L) 0L else length(matches)
    expect_equal(count, 1L)
  })
})

test_that("generate_txt: no duplicate edges", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node1 <- new_operation_node(
      dataset = "input", file_path = "test.sas", line_number = 5,
      operation_type = "DATA", input_datasets = character(0), depth = 1
    )
    node2 <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 10,
      operation_type = "DATA", input_datasets = "input", depth = 0
    )
    gen$graph_nodes <- list(node1, node2)
    gen$graph_edges <- list(list(node1, node2), list(node1, node2))
    txt <- gen$generate_txt()
    matches <- gregexpr("E input_test_sas_5 -> output_test_sas_10", txt, fixed = TRUE)[[1]]
    count <- if (matches[1] == -1L) 0L else length(matches)
    expect_equal(count, 1L)
  })
})

# ===========================================================================
# TestFeedsComputation
# ===========================================================================
.run_feeds_test <- function(dir, operations, target_datasets, sas_code) {
  writeLines(sas_code, file.path(dir, "main.sas"))
  manifest_paths <- character(0)
  for (i in seq_along(target_datasets)) {
    manifest <- list(
      target_dataset = target_datasets[i],
      operations = operations[[i]]
    )
    mp <- file.path(dir, sprintf("manifest_%d.json", i))
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)
    manifest_paths <- c(manifest_paths, mp)
  }
  gen <- OperationsGraphGenerator$new(dir, "main.sas", manifest_paths)
  gen$load_manifests()
  gen$walk_code()
  gen
}

test_that("feeds: shared upstream feeds both terminals", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    sas_code <- paste(
      "data shared; set in; run;",
      "data branch_a; set shared; run;",
      "data branch_b; set shared; run;",
      "data term_a; set branch_a; run;",
      "data term_b; set branch_b; run;",
      sep = "\n"
    )
    ops_a <- list(
      list(dataset = "shared", file = "main.sas", line_number = 1,
           operation_type = "DATA", input_datasets = list(), depth = 2),
      list(dataset = "branch_a", file = "main.sas", line_number = 2,
           operation_type = "DATA", input_datasets = list("shared"), depth = 1),
      list(dataset = "term_a", file = "main.sas", line_number = 4,
           operation_type = "DATA", input_datasets = list("branch_a"), depth = 0)
    )
    ops_b <- list(
      list(dataset = "shared", file = "main.sas", line_number = 1,
           operation_type = "DATA", input_datasets = list(), depth = 2),
      list(dataset = "branch_b", file = "main.sas", line_number = 3,
           operation_type = "DATA", input_datasets = list("shared"), depth = 1),
      list(dataset = "term_b", file = "main.sas", line_number = 5,
           operation_type = "DATA", input_datasets = list("branch_b"), depth = 0)
    )
    gen <- .run_feeds_test(dir, list(ops_a, ops_b), c("term_a", "term_b"), sas_code)
    feeds_by_ds <- list()
    for (n in gen$graph_nodes) feeds_by_ds[[n$dataset]] <- sort(n$feeds)

    expect_equal(feeds_by_ds[["shared"]], c("term_a", "term_b"))
    expect_equal(feeds_by_ds[["branch_a"]], "term_a")
    expect_equal(feeds_by_ds[["branch_b"]], "term_b")
    expect_equal(feeds_by_ds[["term_a"]], "term_a")
    expect_equal(feeds_by_ds[["term_b"]], "term_b")
  })
})

test_that("feeds: feeds attribute emitted in txt", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    sas_code <- paste(
      "data shared; set in; run;",
      "data term_a; set shared; run;",
      "data term_b; set shared; run;",
      sep = "\n"
    )
    ops_a <- list(
      list(dataset = "shared", file = "main.sas", line_number = 1,
           operation_type = "DATA", input_datasets = list(), depth = 1),
      list(dataset = "term_a", file = "main.sas", line_number = 2,
           operation_type = "DATA", input_datasets = list("shared"), depth = 0)
    )
    ops_b <- list(
      list(dataset = "shared", file = "main.sas", line_number = 1,
           operation_type = "DATA", input_datasets = list(), depth = 1),
      list(dataset = "term_b", file = "main.sas", line_number = 3,
           operation_type = "DATA", input_datasets = list("shared"), depth = 0)
    )
    gen <- .run_feeds_test(dir, list(ops_a, ops_b), c("term_a", "term_b"), sas_code)
    txt <- gen$generate_txt()

    # Find shared node line
    txt_lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    shared_line <- txt_lines[grepl("^N .* dataset=shared", txt_lines)]
    expect_length(shared_line, 1L)
    expect_true(grepl("feeds=term_a,term_b", shared_line, fixed = TRUE))

    # term_a feeds only itself
    term_a_line <- txt_lines[grepl("^N .* dataset=term_a", txt_lines)]
    expect_length(term_a_line, 1L)
    expect_true(grepl("feeds=term_a", term_a_line, fixed = TRUE))
    # Ensure term_b is not in term_a's feeds
    feeds_part <- sub(".*feeds=", "", term_a_line)
    feeds_part <- sub(" .*", "", feeds_part)
    expect_false(grepl("term_b", feeds_part, fixed = TRUE))
  })
})

# ===========================================================================
# TestLoadManifest
# ===========================================================================
test_that("load_manifests: simple manifest", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    manifest <- list(
      target_dataset = "output",
      operations = list(
        list(dataset = "output", file = "test.sas", line_number = 10,
             operation_type = "DATA", input_datasets = list("input"), depth = 0)
      )
    )
    mp <- file.path(dir, "manifest.json")
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)
    gen <- OperationsGraphGenerator$new(dir, "test.sas", mp)
    gen$load_manifests()
    expect_true("output" %in% gen$target_datasets)
    cs <- call_site_key("test.sas", 10)
    expect_true(exists(cs, envir = gen$operation_lookup, inherits = FALSE))
  })
})

test_that("load_manifests: multiple ops same line", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    manifest <- list(
      target_dataset = "output",
      operations = list(
        list(dataset = "out1", file = "test.sas", line_number = 10,
             operation_type = "DATA", input_datasets = list(), depth = 0),
        list(dataset = "out2", file = "test.sas", line_number = 10,
             operation_type = "DATA", input_datasets = list(), depth = 0)
      )
    )
    mp <- file.path(dir, "manifest.json")
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)
    gen <- OperationsGraphGenerator$new(dir, "test.sas", mp)
    gen$load_manifests()
    cs <- call_site_key("test.sas", 10)
    ops <- get(cs, envir = gen$operation_lookup)
    expect_length(ops, 2L)
  })
})

test_that("load_manifests: macro_source_key used as lookup key", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    manifest <- list(
      target_dataset = "output",
      operations = list(list(
        dataset = "output", file = "called.sas", line_number = 10,
        macro_source_file = "macro_def.sas", macro_source_line = 5,
        operation_type = "DATA", input_datasets = list(), depth = 0
      ))
    )
    mp <- file.path(dir, "manifest.json")
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)
    gen <- OperationsGraphGenerator$new(dir, "test.sas", mp)
    gen$load_manifests()
    expect_true(exists(call_site_key("macro_def.sas", 5),
                       envir = gen$operation_lookup, inherits = FALSE))
    expect_false(exists(call_site_key("called.sas", 10),
                        envir = gen$operation_lookup, inherits = FALSE))
  })
})

# ===========================================================================
# TestExtractCode
# ===========================================================================
test_that("extract_code: single line", {
  with_temp_sas_dir(list(
    "test.sas" = paste(
      "/* Line 1 */",
      "data output;",
      "    set input;",
      "    x = 1;",
      "run;",
      "/* Line 6 */",
      sep = "\n"
    )
  ), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    sas_path <- file.path(dir, "test.sas")
    node <- new_operation_node(
      dataset = "output", file_path = sas_path, line_number = 2,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    code <- gen$.__enclos_env__$private$extract_code(node)
    expect_true(grepl("data output;", code, fixed = TRUE))
  })
})

test_that("extract_code: multi-line", {
  with_temp_sas_dir(list(
    "test.sas" = paste(
      "/* Line 1 */",
      "data output;",
      "    set input;",
      "    x = 1;",
      "run;",
      "/* Line 6 */",
      sep = "\n"
    )
  ), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    sas_path <- file.path(dir, "test.sas")
    node <- new_operation_node(
      dataset = "output", file_path = sas_path, line_number = 2,
      operation_type = "DATA", input_datasets = character(0), depth = 0,
      end_line = 5
    )
    code <- gen$.__enclos_env__$private$extract_code(node)
    expect_true(grepl("data output;", code, fixed = TRUE))
    expect_true(grepl("set input;", code, fixed = TRUE))
    expect_true(grepl("x = 1;", code, fixed = TRUE))
    expect_true(grepl("run;", code, fixed = TRUE))
  })
})

test_that("extract_code: file not found", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = file.path(dir, "nonexistent.sas"),
      line_number = 1, operation_type = "DATA",
      input_datasets = character(0), depth = 0
    )
    code <- gen$.__enclos_env__$private$extract_code(node)
    expect_true(grepl("Could not read file", code, fixed = TRUE))
  })
})

test_that("extract_code: line out of range", {
  with_temp_sas_dir(list("test.sas" = "data x; run;"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = file.path(dir, "test.sas"),
      line_number = 999, operation_type = "DATA",
      input_datasets = character(0), depth = 0
    )
    code <- gen$.__enclos_env__$private$extract_code(node)
    expect_true(grepl("out of range", code, fixed = TRUE))
  })
})

# ===========================================================================
# TestGenerateLlm (markdown outputs)
# ===========================================================================
test_that("generate_graph_md: header contains target dataset", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    graph <- gen$generate_graph_md()
    expect_true(grepl("# Analysis for datasets: output", graph, fixed = TRUE))
  })
})

test_that("generate_code_extracts_md: header contains target dataset", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    extracts <- gen$generate_code_extracts_md()
    expect_true(grepl("# Code extracts for datasets: output", extracts, fixed = TRUE))
  })
})

test_that("generate_graph_md: contains topology sections", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    graph <- gen$generate_graph_md()
    expect_true(grepl("## Input file mapping", graph, fixed = TRUE))
    expect_true(grepl("## Data lineage dependency graph", graph, fixed = TRUE))
    expect_true(grepl("# Nodes", graph, fixed = TRUE))
    expect_true(grepl("# Edges", graph, fixed = TRUE))
  })
})

test_that("generate_graph_md: omits code extracts", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    graph <- gen$generate_graph_md()
    expect_false(grepl("```sas", graph, fixed = TRUE))
    expect_false(grepl("### ", graph, fixed = TRUE))
  })
})

test_that("generate_code_extracts_md: contains node block", {
  with_temp_sas_dir(list(
    "test.sas" = paste(
      "/* Header */",
      "data output;",
      "    set input;",
      "    x = 1;",
      "run;",
      sep = "\n"
    )
  ), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    sas_path <- file.path(dir, "test.sas")
    node <- new_operation_node(
      dataset = "output", file_path = sas_path, line_number = 2,
      operation_type = "DATA", input_datasets = character(0), depth = 0,
      end_line = 5
    )
    gen$graph_nodes <- list(node)
    extracts <- gen$generate_code_extracts_md()
    expect_true(grepl("### output_test_sas_2", extracts, fixed = TRUE))
    expect_true(grepl("- **type:** DATA", extracts, fixed = TRUE))
    expect_true(grepl("- **file:** test.sas:2-5", extracts, fixed = TRUE))
    expect_true(grepl("```sas", extracts, fixed = TRUE))
    expect_true(grepl("data output;", extracts, fixed = TRUE))
  })
})

test_that("generate_code_extracts_md: resolved path for INFILE", {
  with_temp_sas_dir(list("test.sas" = "/* header */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "infile:mydata", file_path = file.path(dir, "test.sas"),
      line_number = 1, operation_type = "INFILE",
      input_datasets = character(0), depth = 0,
      resolved_path = "&datan/myfile.dat"
    )
    gen$graph_nodes <- list(node)
    extracts <- gen$generate_code_extracts_md()
    expect_true(grepl("- **resolved path:** &datan/myfile.dat", extracts, fixed = TRUE))
  })
})

test_that("generate_code_extracts_md: no end_line shows only start", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 2,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node)
    extracts <- gen$generate_code_extracts_md()
    expect_true(grepl("- **file:** test.sas:2", extracts, fixed = TRUE))
    expect_false(grepl("test.sas:2-", extracts, fixed = TRUE))
  })
})

test_that("generate_code_extracts_md: deduplicates repeated nodes", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir, target = "output")
    gen$load_manifests()
    node <- new_operation_node(
      dataset = "output", file_path = "test.sas", line_number = 2,
      operation_type = "DATA", input_datasets = character(0), depth = 0
    )
    gen$graph_nodes <- list(node, node)
    extracts <- gen$generate_code_extracts_md()
    matches <- gregexpr("### output_test_sas_2", extracts, fixed = TRUE)[[1]]
    count <- if (matches[1] == -1L) 0L else length(matches)
    expect_equal(count, 1L)
  })
})

# ===========================================================================
# TestSpecBuildBundle
# ===========================================================================
.two_terminal_generator <- function(dir) {
  sas_code <- paste(
    "data shared; set in; run;",
    "data branch_a; set shared; run;",
    "data branch_b; set shared; run;",
    "data term_a; set branch_a; run;",
    "data term_b; set branch_b; run;",
    sep = "\n"
  )
  ops_a <- list(
    list(dataset = "shared", file = "main.sas", line_number = 1,
         operation_type = "DATA", input_datasets = list(), depth = 2),
    list(dataset = "branch_a", file = "main.sas", line_number = 2,
         operation_type = "DATA", input_datasets = list("shared"), depth = 1),
    list(dataset = "term_a", file = "main.sas", line_number = 4,
         operation_type = "DATA", input_datasets = list("branch_a"), depth = 0)
  )
  ops_b <- list(
    list(dataset = "shared", file = "main.sas", line_number = 1,
         operation_type = "DATA", input_datasets = list(), depth = 2),
    list(dataset = "branch_b", file = "main.sas", line_number = 3,
         operation_type = "DATA", input_datasets = list("shared"), depth = 1),
    list(dataset = "term_b", file = "main.sas", line_number = 5,
         operation_type = "DATA", input_datasets = list("branch_b"), depth = 0)
  )
  .run_feeds_test(dir, list(ops_a, ops_b), c("term_a", "term_b"), sas_code)
}

test_that("bucket_layout: shared-first ordering", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    gen <- .two_terminal_generator(dir)
    layout <- gen$.__enclos_env__$private$bucket_layout()

    feeds_order <- lapply(layout, function(b) b$feeds)
    expect_equal(feeds_order[[1]], c("term_a", "term_b"))

    remaining <- lapply(feeds_order[-1], identity)
    remaining_sets <- lapply(remaining, function(f) paste(f, collapse = ","))
    expect_true(setequal(remaining_sets, c("term_a", "term_b")))

    nodes_per <- vapply(layout, function(b) length(b$nodes), integer(1))
    expect_equal(nodes_per[1], 1L)  # shared bucket
  })
})

test_that("spec_index_md: lists outputs and total nodes", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    gen <- .two_terminal_generator(dir)
    index_md <- gen$generate_spec_index_md()
    expect_true(grepl("Outputs covered: term_a, term_b", index_md, fixed = TRUE))
    expect_true(grepl("Total nodes:     5", index_md, fixed = TRUE))

    md_lines <- strsplit(index_md, "\n", fixed = TRUE)[[1]]
    feeds_headers <- md_lines[grepl("^## feeds: ", md_lines)]
    expect_equal(feeds_headers, c(
      "## feeds: term_a, term_b",
      "## feeds: term_a",
      "## feeds: term_b"
    ))
  })
})

test_that("spec_index_md: per-node table carries extract range", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    gen <- .two_terminal_generator(dir)
    index_md <- gen$generate_spec_index_md()
    extracts <- gen$generate_code_extracts_md()
    ext_lines <- strsplit(extracts, "\n", fixed = TRUE)[[1]]

    md_lines <- strsplit(index_md, "\n", fixed = TRUE)[[1]]
    shared_row <- md_lines[grepl("shared_main_sas_1", md_lines) &
                             grepl("^\\|", md_lines)]
    expect_length(shared_row, 1L)
    cells <- trimws(strsplit(trimws(shared_row, which = "both"), "\\|")[[1]])
    cells <- cells[nzchar(cells)]
    range_cell <- cells[length(cells)]
    parts <- as.integer(strsplit(range_cell, "-", fixed = TRUE)[[1]])
    start <- parts[1]
    end <- parts[2]
    expect_equal(ext_lines[start], "### shared_main_sas_1")
    expect_equal(ext_lines[end], "```")
  })
})

test_that("spec_index_json: matches md structure", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    gen <- .two_terminal_generator(dir)
    payload <- jsonlite::fromJSON(gen$generate_spec_index_json(),
                                   simplifyVector = FALSE)
    expect_equal(unlist(payload$outputs), c("term_a", "term_b"))
    expect_equal(payload$total_nodes, 5L)
    feeds <- lapply(payload$buckets, function(b) unlist(b$feeds))
    expect_equal(feeds[[1]], c("term_a", "term_b"))
    remaining <- lapply(feeds[-1], identity)
    expect_true(setequal(
      vapply(remaining, paste, character(1), collapse = ","),
      c("term_a", "term_b")
    ))
    for (bucket in payload$buckets) {
      for (node in bucket$nodes) {
        rng <- unlist(node$code_extract_lines)
        expect_true(rng[1] >= 1L)
        expect_true(rng[2] >= rng[1])
      }
    }
  })
})

test_that("spec_index_md: includes input file mapping for INFILE", {
  with_temp_sas_dir(list("main.sas" = "/* placeholder */"), function(dir) {
    gen <- make_test_generator(dir, target = "output", entrypoint = "main.sas")
    gen$load_manifests()
    gen$graph_nodes <- list(
      new_operation_node(
        dataset = "infile:input.dat", file_path = "main.sas", line_number = 1,
        operation_type = "INFILE", input_datasets = character(0), depth = 1,
        resolved_path = "&datan/input.dat"
      ),
      new_operation_node(
        dataset = "output", file_path = "main.sas", line_number = 2,
        operation_type = "DATA", input_datasets = "infile:input.dat", depth = 0
      )
    )
    for (i in seq_along(gen$graph_nodes)) {
      gen$graph_nodes[[i]]$feeds <- "output"
    }
    index_md <- gen$generate_spec_index_md()
    expect_true(grepl("## Input file mapping", index_md, fixed = TRUE))
    expect_true(grepl("| input.dat | <datan>/input.dat |", index_md, fixed = TRUE))
  })
})

# ===========================================================================
# TestLog
# ===========================================================================
test_that("log: verbose prints debug", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    gen$verbose <- TRUE
    output <- capture.output(gen$log("hello world"))
    expect_true(any(grepl("hello world", output)))
    expect_true(any(grepl("\\[DEBUG\\]", output)))
  })
})

test_that("log: non-verbose is silent", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    gen$verbose <- FALSE
    output <- capture.output(gen$log("hello world"))
    expect_length(output, 0L)
  })
})

# ===========================================================================
# TestReadFileCache
# ===========================================================================
test_that("read_file: cache returns same object", {
  with_temp_sas_dir(list("test.sas" = "data x; run;"), function(dir) {
    gen <- make_test_generator(dir)
    sas_path <- file.path(dir, "test.sas")
    lines1 <- gen$.__enclos_env__$private$read_file(sas_path)
    lines2 <- gen$.__enclos_env__$private$read_file(sas_path)
    # In R, lists are reference objects in lists; check identity via env
    expect_identical(lines1, lines2)
  })
})

# ===========================================================================
# TestBuildFilenameAliasMap
# ===========================================================================
test_that("build_filename_alias_map: resolves .sas alias", {
  with_temp_sas_dir(list(
    "my_macro.sas" = "%macro test; %mend;",
    "main.sas" = 'filename mymacro "&prog/my_macro.sas";'
  ), function(dir) {
    gen <- make_test_generator(dir, entrypoint = "main.sas")
    gen$build_filename_alias_map()
    expect_true("mymacro" %in% names(gen$filename_aliases))
    expect_equal(gen$filename_aliases[["mymacro"]], file.path(dir, "my_macro.sas"))
  })
})

test_that("build_filename_alias_map: non-.sas path not added", {
  with_temp_sas_dir(list(
    "main.sas" = 'filename mydat "&datan/data.dat";'
  ), function(dir) {
    gen <- make_test_generator(dir, entrypoint = "main.sas")
    gen$build_filename_alias_map()
    expect_false("mydat" %in% names(gen$filename_aliases))
  })
})

test_that("build_filename_alias_map: no filename statement", {
  with_temp_sas_dir(list("main.sas" = "data x; run;"), function(dir) {
    gen <- make_test_generator(dir, entrypoint = "main.sas")
    gen$build_filename_alias_map()
    expect_length(gen$filename_aliases, 0L)
  })
})

# ===========================================================================
# TestResolveFilenamePath
# ===========================================================================
test_that("resolve_filename_path: direct basename", {
  with_temp_sas_dir(list("direct.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    result <- gen$.__enclos_env__$private$resolve_filename_path("direct.sas")
    expect_equal(result, file.path(dir, "direct.sas"))
  })
})

test_that("resolve_filename_path: non-.sas returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    result <- gen$.__enclos_env__$private$resolve_filename_path("/some/path/data.dat")
    expect_null(result)
  })
})

test_that("resolve_filename_path: no matching file returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    result <- gen$.__enclos_env__$private$resolve_filename_path(
      "/some/path/nonexistent_xyz.sas"
    )
    expect_null(result)
  })
})

# ===========================================================================
# TestBuildMacroMap
# ===========================================================================
test_that("build_macro_map: finds macros in sas files", {
  with_temp_sas_dir(list(
    "macros.sas" = paste("%macro mymacro;", "data x; run;", "%mend;", sep = "\n")
  ), function(dir) {
    gen <- make_test_generator(dir, entrypoint = "main.sas")
    gen$build_macro_map()
    expect_true("mymacro" %in% names(gen$macro_definitions))
  })
})

# ===========================================================================
# TestWalkCode
# ===========================================================================
test_that("walk_code: missing entrypoint prints error", {
  with_temp_sas_dir(list("placeholder.sas" = "/* p */"), function(dir) {
    gen <- make_test_generator(dir, target = "output", entrypoint = "nonexistent.sas")
    output <- capture.output(gen$walk_code())
    expect_true(any(grepl("Error", output)))
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("walk_code: valid entrypoint no crash", {
  with_temp_sas_dir(list("main.sas" = "data output; run;"), function(dir) {
    gen <- make_test_generator(dir, target = "output", entrypoint = "main.sas")
    gen$walk_code()
    # No operations in manifest, so no nodes
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("walk_code: uncalled top-level wrapper macro is walked", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    manifest <- list(
      target_dataset = "tab1_1",
      operations = list(
        list(
          dataset = "tab1_1", operation_type = "PROC EXPORT",
          file = "main.sas", line_number = 1, end_line = 1,
          input_datasets = list("tot"),
          macro_name = "lance", macro_source_file = "main.sas",
          macro_source_line = 3, macro_end_line = 6
        ),
        list(
          dataset = "tot", operation_type = "DATA",
          file = "main.sas", line_number = 1, end_line = 1,
          input_datasets = list("raw"),
          macro_name = "lance", macro_source_file = "main.sas",
          macro_source_line = 2, macro_end_line = 2
        )
      )
    )
    mp <- file.path(dir, "manifest.json")
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)

    writeLines(c(
      "%macro lance(param);",
      "    data tot; set raw; run;",
      "    proc export data=tot",
      '    outfile="&rep./&param..tab1_1.csv"',
      '    dbms=dlm replace; delimiter=";";',
      "    run;",
      "%mend;"
    ), file.path(dir, "main.sas"))

    gen <- OperationsGraphGenerator$new(dir, "main.sas", mp)
    gen$load_manifests()
    gen$build_filename_alias_map()
    gen$build_macro_map()
    gen$walk_code()

    datasets <- unique(vapply(gen$graph_nodes, function(n) n$dataset, character(1)))
    expect_true("tab1_1" %in% datasets)
    expect_true("tot" %in% datasets)
  })
})

test_that("walk_code: called top-level macro not redundantly walked", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    manifest <- list(
      target_dataset = "target",
      operations = list(list(
        dataset = "target", operation_type = "DATA",
        file = "main.sas", line_number = 4, end_line = 4,
        input_datasets = list(),
        macro_name = "pipe", macro_source_file = "main.sas",
        macro_source_line = 2, macro_end_line = 2
      ))
    )
    mp <- file.path(dir, "manifest.json")
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)

    writeLines(c(
      "%macro pipe();",
      "    data target; run;",
      "%mend;",
      "%pipe();"
    ), file.path(dir, "main.sas"))

    gen <- OperationsGraphGenerator$new(dir, "main.sas", mp)
    gen$load_manifests()
    gen$build_filename_alias_map()
    gen$build_macro_map()
    gen$walk_code()

    target_nodes <- Filter(function(n) n$dataset == "target", gen$graph_nodes)
    expect_length(target_nodes, 1L)
  })
})

# ===========================================================================
# TestFindBestMacro
# ===========================================================================
test_that("find_best_macro: no definitions returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    result <- gen$.__enclos_env__$private$find_best_macro("undefined", "test.sas", 10)
    expect_null(result)
  })
})

test_that("find_best_macro: same-file priority", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    macro_same <- new_og_macro_definition("m", "/path/test.sas", 1, 3, character(0))
    macro_other <- new_og_macro_definition("m", "/path/other.sas", 1, 3, character(0))
    # Need files to exist for macro_has_body
    dir.create("/tmp/og_test_best_macro", showWarnings = FALSE)
    on.exit(unlink("/tmp/og_test_best_macro", recursive = TRUE), add = TRUE)
    writeLines(c("%macro m;", "  data x; run;", "%mend;"),
               "/tmp/og_test_best_macro/test.sas")
    writeLines(c("%macro m;", "  data y; run;", "%mend;"),
               "/tmp/og_test_best_macro/other.sas")
    macro_same$file_path <- "/tmp/og_test_best_macro/test.sas"
    macro_other$file_path <- "/tmp/og_test_best_macro/other.sas"
    gen$macro_definitions[["m"]] <- list(macro_other, macro_same)
    result <- gen$.__enclos_env__$private$find_best_macro(
      "m", "/tmp/og_test_best_macro/test.sas", 10
    )
    expect_equal(result$file_path, "/tmp/og_test_best_macro/test.sas")
  })
})

test_that("find_best_macro: fallback to first definition", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    dir.create("/tmp/og_test_fallback", showWarnings = FALSE)
    on.exit(unlink("/tmp/og_test_fallback", recursive = TRUE), add = TRUE)
    writeLines(c("%macro m;", "  data x; run;", "%mend;"),
               "/tmp/og_test_fallback/other1.sas")
    writeLines(c("%macro m;", "  data y; run;", "%mend;"),
               "/tmp/og_test_fallback/other2.sas")
    macro1 <- new_og_macro_definition("m", "/tmp/og_test_fallback/other1.sas", 1, 3)
    macro2 <- new_og_macro_definition("m", "/tmp/og_test_fallback/other2.sas", 1, 3)
    gen$macro_definitions[["m"]] <- list(macro1, macro2)
    result <- gen$.__enclos_env__$private$find_best_macro(
      "m", "/path/test.sas", 10
    )
    expect_equal(result$file_path, "/tmp/og_test_fallback/other1.sas")
  })
})

test_that("find_best_macro: empty stub does not shadow real other-file", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    gen <- make_test_generator(dir)
    same_file <- file.path(dir, "main.sas")
    other_file <- file.path(dir, "other.sas")
    writeLines(c(
      "data prep; run;",
      "data prep2; run;",
      "data prep3; run;",
      "data prep4; run;",
      "%macro m; %mend;",
      "data prep5; run;"
    ), same_file)
    writeLines(c(
      "%macro m;",
      "    data target; set src; run;",
      "%mend;"
    ), other_file)

    stub <- new_og_macro_definition("m", same_file, 5, 5, character(0))
    real <- new_og_macro_definition("m", other_file, 1, 3, character(0))
    gen$macro_definitions[["m"]] <- list(stub, real)

    result <- gen$.__enclos_env__$private$find_best_macro("m", same_file, 10)
    expect_equal(result$file_path, other_file)
  })
})

test_that("find_best_macro: empty stub used when no real definition exists", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    gen <- make_test_generator(dir)
    same_file <- file.path(dir, "main.sas")
    other_file <- file.path(dir, "other.sas")
    writeLines(c("data prep; run;", "%macro m; %mend;"), same_file)
    writeLines("%macro m; %mend;", other_file)

    same_stub <- new_og_macro_definition("m", same_file, 2, 2, character(0))
    other_stub <- new_og_macro_definition("m", other_file, 1, 1, character(0))
    gen$macro_definitions[["m"]] <- list(other_stub, same_stub)

    result <- gen$.__enclos_env__$private$find_best_macro("m", same_file, 10)
    expect_equal(result$file_path, same_file)
  })
})

# ===========================================================================
# TestWalkFile
# ===========================================================================
test_that("walk_file: depth limit prevents infinite recursion", {
  with_temp_sas_dir(list("main.sas" = "data output; run;"), function(dir) {
    gen <- make_test_generator(dir, target = "output", entrypoint = "main.sas")
    gen$load_manifests()
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "main.sas"), 1L, list(), depth = 101L, in_macro = FALSE
    )
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("walk_file: empty file no crash", {
  with_temp_sas_dir(list(
    "empty.sas" = "",
    "test.sas" = "/* test */"
  ), function(dir) {
    gen <- make_test_generator(dir)
    gen$load_manifests()
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "empty.sas"), 1L, list(), depth = 0L, in_macro = FALSE
    )
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("walk_file: missing file no crash", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    gen$load_manifests()
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "nonexistent.sas"), 1L, list(), depth = 0L, in_macro = FALSE
    )
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("walk_file: visited location skipped", {
  with_temp_sas_dir(list("main.sas" = "data output; run;"), function(dir) {
    ops <- list(list(
      dataset = "output", file = "main.sas", line_number = 1,
      operation_type = "DATA", input_datasets = list(), depth = 0
    ))
    gen <- make_test_generator(dir, target = "output", ops = ops,
                                entrypoint = "main.sas")
    gen$load_manifests()
    gen$visited_locations <- c(gen$visited_locations,
                                call_site_key(file.path(dir, "main.sas"), 1))
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "main.sas"), 1L, list(), depth = 0L, in_macro = FALSE
    )
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("walk_file: macro definition lines skipped", {
  with_temp_sas_dir(list(
    "main.sas" = paste("%macro test;", "data output; run;", "%mend;", sep = "\n")
  ), function(dir) {
    ops <- list(list(
      dataset = "output", file = "main.sas", line_number = 2,
      operation_type = "DATA", input_datasets = list(), depth = 0
    ))
    gen <- make_test_generator(dir, target = "output", ops = ops,
                                entrypoint = "main.sas")
    gen$load_manifests()
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "main.sas"), 1L, list(), depth = 0L, in_macro = FALSE
    )
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("walk_file: include traversal", {
  with_temp_sas_dir(list(
    "main.sas" = "%include included;",
    "included.sas" = "data output; run;"
  ), function(dir) {
    ops <- list(list(
      dataset = "output", file = "included.sas", line_number = 1,
      operation_type = "DATA", input_datasets = list(), depth = 0
    ))
    gen <- make_test_generator(dir, target = "output", ops = ops,
                                entrypoint = "main.sas")
    gen$load_manifests()
    gen$filename_aliases[["included"]] <- file.path(dir, "included.sas")
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "main.sas"), 1L, list(), depth = 0L, in_macro = FALSE
    )
    expect_length(gen$graph_nodes, 1L)
    expect_equal(gen$graph_nodes[[1]]$dataset, "output")
  })
})

test_that("walk_file: macro call traversal", {
  with_temp_sas_dir(list(
    "main.sas" = "%mymacro;",
    "macros.sas" = paste("%macro mymacro;", "data output; run;", "%mend;", sep = "\n")
  ), function(dir) {
    ops <- list(list(
      dataset = "output", file = "macros.sas", line_number = 2,
      operation_type = "DATA", input_datasets = list(), depth = 0
    ))
    gen <- make_test_generator(dir, target = "output", ops = ops,
                                entrypoint = "main.sas")
    gen$load_manifests()
    gen$build_macro_map()
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "main.sas"), 1L, list(), depth = 0L, in_macro = FALSE
    )
    expect_length(gen$graph_nodes, 1L)
    expect_equal(gen$graph_nodes[[1]]$dataset, "output")
  })
})

test_that("walk_file: empty macro definition skipped", {
  with_temp_sas_dir(list("main.sas" = "%foo;"), function(dir) {
    gen <- make_test_generator(dir, target = "output", entrypoint = "main.sas")
    gen$load_manifests()
    gen$macro_definitions[["foo"]] <- list()
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "main.sas"), 1L, list(), depth = 0L, in_macro = FALSE
    )
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("walk_file: in_macro stops at end line", {
  with_temp_sas_dir(list(
    "main.sas" = paste("/* line 1 */", "/* line 2 */", "data output; run;", sep = "\n")
  ), function(dir) {
    ops <- list(list(
      dataset = "output", file = "main.sas", line_number = 3,
      operation_type = "DATA", input_datasets = list(), depth = 0
    ))
    gen <- make_test_generator(dir, target = "output", ops = ops,
                                entrypoint = "main.sas")
    gen$load_manifests()
    gen$.__enclos_env__$private$walk_file(
      file.path(dir, "main.sas"), 1L, list(), depth = 0L,
      in_macro = TRUE, macro_end_line = 3L
    )
    expect_length(gen$graph_nodes, 0L)
  })
})

# ===========================================================================
# TestCheckAndAddOperation
# ===========================================================================
test_that("check_and_add_operation: adds node on match", {
  with_temp_sas_dir(list("main.sas" = "data output; run;"), function(dir) {
    ops <- list(list(
      dataset = "output", file = "main.sas", line_number = 1,
      operation_type = "DATA", input_datasets = list(), depth = 0
    ))
    gen <- make_test_generator(dir, target = "output", ops = ops,
                                entrypoint = "main.sas")
    gen$load_manifests()
    result <- gen$.__enclos_env__$private$check_and_add_operation(
      file.path(dir, "main.sas"), 1L, "data output; run;"
    )
    expect_true(result)
    expect_length(gen$graph_nodes, 1L)
    expect_equal(gen$graph_nodes[[1]]$dataset, "output")
  })
})

test_that("check_and_add_operation: no match returns FALSE", {
  with_temp_sas_dir(list("main.sas" = "data output; run;"), function(dir) {
    gen <- make_test_generator(dir, target = "output", entrypoint = "main.sas")
    gen$load_manifests()
    result <- gen$.__enclos_env__$private$check_and_add_operation(
      file.path(dir, "main.sas"), 99L, "data output; run;"
    )
    expect_false(result)
    expect_length(gen$graph_nodes, 0L)
  })
})

test_that("check_and_add_operation: creates edge from last_modified", {
  with_temp_sas_dir(list("main.sas" = "/* test */"), function(dir) {
    ops <- list(
      list(dataset = "input_ds", file = "main.sas", line_number = 1,
           operation_type = "DATA", input_datasets = list(), depth = 1),
      list(dataset = "output", file = "main.sas", line_number = 2,
           operation_type = "DATA", input_datasets = list("input_ds"), depth = 0)
    )
    gen <- make_test_generator(dir, target = "output", ops = ops,
                                entrypoint = "main.sas")
    gen$load_manifests()
    filepath <- file.path(dir, "main.sas")
    gen$.__enclos_env__$private$check_and_add_operation(filepath, 1L, "data input_ds; run;")
    gen$.__enclos_env__$private$check_and_add_operation(filepath, 2L, "data output; set input_ds; run;")
    expect_length(gen$graph_edges, 1L)
    expect_equal(gen$graph_edges[[1]][[1]]$dataset, "input_ds")
    expect_equal(gen$graph_edges[[1]][[2]]$dataset, "output")
  })
})

test_that("check_and_add_operation: macro_end_line overrides end_line", {
  with_temp_sas_dir(list("main.sas" = "/* test */"), function(dir) {
    ops <- list(list(
      dataset = "output", file = "main.sas", line_number = 1,
      operation_type = "DATA", input_datasets = list(), depth = 0,
      end_line = 5, macro_end_line = 10
    ))
    gen <- make_test_generator(dir, target = "output", ops = ops,
                                entrypoint = "main.sas")
    gen$load_manifests()
    gen$.__enclos_env__$private$check_and_add_operation(
      file.path(dir, "main.sas"), 1L, "data output; run;"
    )
    expect_equal(gen$graph_nodes[[1]]$end_line, 10L)
  })
})

# ===========================================================================
# TestDetectIfChain
# ===========================================================================
test_that("detect_if_chain: non-if line returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c("data x; run;")
    expect_null(gen$.__enclos_env__$private$detect_if_chain(lines, 1L))
  })
})

test_that("detect_if_chain: inline if (no %do) returns NULL", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c("%if &x %then %let y=1;")
    expect_null(gen$.__enclos_env__$private$detect_if_chain(lines, 1L))
  })
})

test_that("detect_if_chain: simple if-only chain", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c(
      "%if &x %then %do;",
      "  data a; run;",
      "%end;",
      "data after; run;"
    )
    result <- gen$.__enclos_env__$private$detect_if_chain(lines, 1L)
    expect_false(is.null(result))
    expect_equal(result$branches, list(c(1, 3)))
    expect_false(result$has_else)
    expect_equal(result$end_idx, 4L)
  })
})

test_that("detect_if_chain: if-else chain", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c(
      "%if &x %then %do;",
      "  data a; run;",
      "%end;",
      "%else %do;",
      "  data b; run;",
      "%end;"
    )
    result <- gen$.__enclos_env__$private$detect_if_chain(lines, 1L)
    expect_equal(result$branches, list(c(1, 3), c(4, 6)))
    expect_true(result$has_else)
    expect_equal(result$end_idx, 7L)
  })
})

test_that("detect_if_chain: if-elseif-else chain", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c(
      "%if &x=1 %then %do;",
      "  data a; run;",
      "%end;",
      "%else %if &x=2 %then %do;",
      "  data b; run;",
      "%end;",
      "%else %do;",
      "  data c; run;",
      "%end;"
    )
    result <- gen$.__enclos_env__$private$detect_if_chain(lines, 1L)
    expect_equal(result$branches, list(c(1, 3), c(4, 6), c(7, 9)))
    expect_true(result$has_else)
    expect_equal(result$end_idx, 10L)
  })
})

test_that("detect_if_chain: if-elseif without else", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c(
      "%if &x=1 %then %do;",
      "  data a; run;",
      "%end;",
      "%else %if &x=2 %then %do;",
      "  data b; run;",
      "%end;"
    )
    result <- gen$.__enclos_env__$private$detect_if_chain(lines, 1L)
    expect_equal(result$branches, list(c(1, 3), c(4, 6)))
    expect_false(result$has_else)
    expect_equal(result$end_idx, 7L)
  })
})

test_that("detect_if_chain: nested do blocks are counted", {
  with_temp_sas_dir(list("test.sas" = "/* test */"), function(dir) {
    gen <- make_test_generator(dir)
    lines <- c(
      "%if &x %then %do;",
      "  %do i=1 %to 3;",
      "    data a&i; run;",
      "  %end;",
      "%end;"
    )
    result <- gen$.__enclos_env__$private$detect_if_chain(lines, 1L)
    expect_equal(result$branches, list(c(1, 5)))
    expect_false(result$has_else)
    expect_equal(result$end_idx, 6L)
  })
})

# ===========================================================================
# TestWalkIfChain
# ===========================================================================
test_that("walk_if_chain: both branches writing X produce two edges at join", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    sas_code <- paste(
      "data x; run;",
      "%if &cond %then %do;",
      "data x; run;",
      "%end;",
      "%else %do;",
      "data x; run;",
      "%end;",
      "data downstream; set x; run;",
      sep = "\n"
    )
    ops <- list(
      list(dataset = "x", file = "main.sas", line_number = 1,
           operation_type = "DATA", input_datasets = list(), depth = 0),
      list(dataset = "x", file = "main.sas", line_number = 3,
           operation_type = "DATA", input_datasets = list(), depth = 0),
      list(dataset = "x", file = "main.sas", line_number = 6,
           operation_type = "DATA", input_datasets = list(), depth = 0),
      list(dataset = "downstream", file = "main.sas", line_number = 8,
           operation_type = "DATA", input_datasets = list("x"), depth = 0)
    )
    writeLines(sas_code, file.path(dir, "main.sas"))
    manifest <- list(target_dataset = "downstream", operations = ops)
    mp <- file.path(dir, "manifest.json")
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)
    gen <- OperationsGraphGenerator$new(dir, "main.sas", mp)
    gen$load_manifests()
    gen$walk_code()

    downstream_inputs <- sort(vapply(
      Filter(function(e) e[[2]]$dataset == "downstream", gen$graph_edges),
      function(e) e[[1]]$line_number,
      integer(1)
    ))
    expect_equal(downstream_inputs, c(3L, 6L))
  })
})

test_that("walk_if_chain: if without else keeps prefork writer", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    sas_code <- paste(
      "data x; run;",
      "%if &cond %then %do;",
      "data x; run;",
      "%end;",
      "data downstream; set x; run;",
      sep = "\n"
    )
    ops <- list(
      list(dataset = "x", file = "main.sas", line_number = 1,
           operation_type = "DATA", input_datasets = list(), depth = 0),
      list(dataset = "x", file = "main.sas", line_number = 3,
           operation_type = "DATA", input_datasets = list(), depth = 0),
      list(dataset = "downstream", file = "main.sas", line_number = 5,
           operation_type = "DATA", input_datasets = list("x"), depth = 0)
    )
    writeLines(sas_code, file.path(dir, "main.sas"))
    manifest <- list(target_dataset = "downstream", operations = ops)
    mp <- file.path(dir, "manifest.json")
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)
    gen <- OperationsGraphGenerator$new(dir, "main.sas", mp)
    gen$load_manifests()
    gen$walk_code()

    downstream_inputs <- sort(vapply(
      Filter(function(e) e[[2]]$dataset == "downstream", gen$graph_edges),
      function(e) e[[1]]$line_number,
      integer(1)
    ))
    expect_equal(downstream_inputs, c(1L, 3L))
  })
})

test_that("walk_if_chain: only writing branch + else keeps prefork", {
  with_temp_sas_dir(list("placeholder" = ""), function(dir) {
    unlink(file.path(dir, "placeholder"))
    sas_code <- paste(
      "data x; run;",
      "%if &cond %then %do;",
      "data x; run;",
      "%end;",
      "%else %do;",
      "data unrelated; run;",
      "%end;",
      "data downstream; set x; run;",
      sep = "\n"
    )
    ops <- list(
      list(dataset = "x", file = "main.sas", line_number = 1,
           operation_type = "DATA", input_datasets = list(), depth = 0),
      list(dataset = "x", file = "main.sas", line_number = 3,
           operation_type = "DATA", input_datasets = list(), depth = 0),
      list(dataset = "unrelated", file = "main.sas", line_number = 6,
           operation_type = "DATA", input_datasets = list(), depth = 0),
      list(dataset = "downstream", file = "main.sas", line_number = 8,
           operation_type = "DATA", input_datasets = list("x"), depth = 0)
    )
    writeLines(sas_code, file.path(dir, "main.sas"))
    manifest <- list(target_dataset = "downstream", operations = ops)
    mp <- file.path(dir, "manifest.json")
    writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), mp)
    gen <- OperationsGraphGenerator$new(dir, "main.sas", mp)
    gen$load_manifests()
    gen$walk_code()

    downstream_inputs <- sort(vapply(
      Filter(function(e) e[[2]]$dataset == "downstream", gen$graph_edges),
      function(e) e[[1]]$line_number,
      integer(1)
    ))
    expect_equal(downstream_inputs, c(1L, 3L))
  })
})

# ===========================================================================
# TestGetOperationStyleExtended
# ===========================================================================
test_that("get_operation_style: unknown PROC type uses default grey", {
  node <- new_operation_node(
    dataset = "other", file_path = "test.sas", line_number = 10,
    operation_type = "PROC MEANS", input_datasets = character(0), depth = 0
  )
  style <- get_operation_style(node, character(0))
  expect_true(grepl("#f5f5f5", style, fixed = TRUE))
})

test_that("get_operation_style: unknown type falls back to DATA blue", {
  node <- new_operation_node(
    dataset = "other", file_path = "test.sas", line_number = 10,
    operation_type = "UNKNOWN_OP", input_datasets = character(0), depth = 0
  )
  style <- get_operation_style(node, character(0))
  expect_true(grepl("#bbdefb", style, fixed = TRUE))
})

# ===========================================================================
# TestParseOutputs
# ===========================================================================
test_that("parse_graph_outputs: single output", {
  expect_equal(parse_graph_outputs("out1"), "out1")
})

test_that("parse_graph_outputs: multiple outputs", {
  expect_equal(parse_graph_outputs("a,b,c"), c("a", "b", "c"))
})

test_that("parse_graph_outputs: strips whitespace", {
  expect_equal(parse_graph_outputs(" a , b ,c "), c("a", "b", "c"))
})

test_that("parse_graph_outputs: drops empty tokens", {
  expect_equal(parse_graph_outputs("a,,b,"), c("a", "b"))
})

test_that("parse_graph_outputs: empty string is rejected", {
  expect_error(parse_graph_outputs(""))
})

test_that("parse_graph_outputs: only commas is rejected", {
  expect_error(parse_graph_outputs(",,,"))
})

# ===========================================================================
# TestRunFunction
# ===========================================================================
test_that("run_operations_graph: success dot format", {
  dir <- tempfile("og_run_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  proc <- "test-proc"
  output_name <- "myoutput"
  group <- output_name

  proc_root <- file.path(dir, "procedures", paste0("migration-", proc))
  sas_dir <- file.path(proc_root, "sas")
  dir.create(sas_dir, recursive = TRUE)
  writeLines("data myoutput; run;", file.path(sas_dir, "main.sas"))

  lineage_dir <- file.path(proc_root, "migration-data", group, "lineage", output_name)
  dir.create(lineage_dir, recursive = TRUE)
  manifest <- list(
    target_dataset = output_name,
    operations = list(list(
      dataset = output_name, file = "main.sas", line_number = 1,
      operation_type = "DATA", input_datasets = list(), depth = 0
    ))
  )
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE),
             file.path(lineage_dir, "lineage-manifest.json"))

  rc <- run_operations_graph(
    proc, file.path(sas_dir, "main.sas"),
    group, output_name, format = "dot", project_root = dir
  )
  expect_equal(rc, 0L)
  expect_true(file.exists(
    file.path(proc_root, "migration-data", group, "lineage", "lineage-graph.dot")
  ))
})

test_that("run_operations_graph: success llm format writes 4 files", {
  dir <- tempfile("og_run_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  proc <- "test-proc"
  output_name <- "myoutput"
  group <- output_name

  proc_root <- file.path(dir, "procedures", paste0("migration-", proc))
  sas_dir <- file.path(proc_root, "sas")
  dir.create(sas_dir, recursive = TRUE)
  writeLines("data myoutput; run;", file.path(sas_dir, "main.sas"))

  lineage_dir <- file.path(proc_root, "migration-data", group, "lineage", output_name)
  dir.create(lineage_dir, recursive = TRUE)
  manifest <- list(
    target_dataset = output_name,
    operations = list(list(
      dataset = output_name, file = "main.sas", line_number = 1,
      operation_type = "DATA", input_datasets = list(), depth = 0
    ))
  )
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE),
             file.path(lineage_dir, "lineage-manifest.json"))

  rc <- run_operations_graph(
    proc, file.path(sas_dir, "main.sas"),
    group, output_name, format = "llm", project_root = dir
  )
  expect_equal(rc, 0L)
  base <- file.path(proc_root, "migration-data", group, "lineage")
  expect_true(file.exists(file.path(base, "lineage-graph.md")))
  expect_true(file.exists(file.path(base, "lineage-code-extracts.md")))
  expect_true(file.exists(file.path(base, "lineage-spec-index.md")))
  expect_true(file.exists(file.path(base, "lineage-spec-index.json")))

  index <- jsonlite::fromJSON(
    file.path(base, "lineage-spec-index.json"),
    simplifyVector = FALSE
  )
  expect_equal(unlist(index$outputs), output_name)
  expect_equal(index$total_nodes, 1L)
})

test_that("run_operations_graph: missing manifest returns 1", {
  dir <- tempfile("og_run_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  proc_root <- file.path(dir, "procedures", "migration-test-proc")
  sas_dir <- file.path(proc_root, "sas")
  dir.create(sas_dir, recursive = TRUE)
  writeLines("data x; run;", file.path(sas_dir, "main.sas"))

  output <- capture.output({
    rc <- run_operations_graph(
      "test-proc", file.path(sas_dir, "main.sas"),
      "no_such_group", "no_such_output", project_root = dir
    )
  })
  expect_equal(rc, 1L)
  expect_true(any(grepl("Error", output)))
})

test_that("run_operations_graph: missing entrypoint returns 1", {
  dir <- tempfile("og_run_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  proc_root <- file.path(dir, "procedures", "migration-test-proc")
  sas_dir <- file.path(proc_root, "sas")
  dir.create(sas_dir, recursive = TRUE)

  output <- capture.output({
    rc <- run_operations_graph(
      "test-proc", "nonexistent.sas",
      "group", "output", project_root = dir
    )
  })
  expect_equal(rc, 1L)
  expect_true(any(grepl("Error", output)))
})

test_that("run_operations_graph: missing target in walk returns 1", {
  dir <- tempfile("og_run_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  proc <- "test-proc"
  proc_root <- file.path(dir, "procedures", paste0("migration-", proc))
  sas_dir <- file.path(proc_root, "sas")
  dir.create(sas_dir, recursive = TRUE)
  writeLines("data myoutput; run;", file.path(sas_dir, "main.sas"))

  lineage_dir <- file.path(proc_root, "migration-data", "ghost_out", "lineage", "ghost_out")
  dir.create(lineage_dir, recursive = TRUE)
  manifest <- list(target_dataset = "ghost_out", operations = list())
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE),
             file.path(lineage_dir, "lineage-manifest.json"))

  output <- capture.output({
    rc <- run_operations_graph(
      proc, file.path(sas_dir, "main.sas"),
      "ghost_out", "ghost_out", project_root = dir
    )
  })
  expect_equal(rc, 1L)
  expect_true(any(grepl("Error", output)))
})
