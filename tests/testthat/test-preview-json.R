# A JSON file as a tree you can fold, reusing the list tree (#250).

json_html <- function(text, ext = "json") {
  path <- withr::local_tempfile(fileext = paste0(".", ext), .local_envir = parent.frame())
  writeLines(text, path, useBytes = TRUE)
  as.character(tar_board_preview(structure(path, class = "tar_board_file"), NULL))
}

test_that("a JSON object is a tree, with JSON's types", {
  html <- json_html('{"name": "board", "size": 3.5, "ok": true, "none": null, "tags": ["a", "b"],
                     "deep": {"x": {"y": {"z": [1, 2]}}}, "empty": {}, "list": []}')
  expect_match(html, "tb-tree", fixed = TRUE)
  expect_match(html, '<code class="tb-tree-name">name</code>', fixed = TRUE)
  for (type in c("string", "number", "bool", "null", "[2]", "{1}", "{0}", "[0]")) {
    expect_match(html, paste0('<span class="tb-tree-type">', type, '</span>'), fixed = TRUE, info = type)
  }
  expect_match(html, '<span class="tb-tree-value">true</span>', fixed = TRUE)
  # Deeper than an R list's two levels: z is there.
  expect_match(html, '<code class="tb-tree-name">z</code>', fixed = TRUE)
  # The raw text, a click away.
  expect_match(html, "Raw text", fixed = TRUE)
  expect_match(html, "tb-preview-text", fixed = TRUE)
})

test_that("an array of objects names its items by position", {
  html <- json_html('[{"id": 1}, {"id": 2}]')
  expect_match(html, '<code class="tb-tree-name">[[1]]</code>', fixed = TRUE)
  expect_match(html, '<span class="tb-tree-type">{1}</span>', fixed = TRUE)
})

test_that("JSON text keeps its Unicode", {
  html <- json_html(enc2utf8('{"city": "São Paulo"}'))
  expect_match(html, enc2utf8("São Paulo"), fixed = TRUE)
})

test_that("invalid JSON falls back to its text, with why on one line", {
  html <- json_html('{"a": 1,')
  expect_match(html, "Not valid JSON", fixed = TRUE)
  expect_match(html, "tb-preview-text", fixed = TRUE)
  expect_no_match(html, "tb-tree-items", fixed = TRUE)
})

test_that("a big JSON file stays text, and a wide one is capped", {
  local_mocked_bindings(json_bytes = function() 10)
  html <- json_html('{"a": 1, "b": 2, "c": 3}')
  expect_match(html, "Too large to show as a tree", fixed = TRUE)
  local_mocked_bindings(json_bytes = function() 5e6)
  wide <- paste0("[", paste(sprintf('{"i": %d, "v": [1, 2, 3]}', 1:3000), collapse = ", "), "]")
  html <- json_html(wide)
  expect_match(html, "and 2,950 more", fixed = TRUE) # 50 items a level
  # The node budget: lists stop opening once it is spent; the levels already
  # open finish as one-line leaves, up to tree_items each.
  nested <- jsonlite::toJSON(rep(list(rep(list(as.list(1:50)), 50)), 50), auto_unbox = TRUE)
  html <- json_html(as.character(nested))
  expect_lte(lengths(regmatches(html, gregexpr("tb-tree-leaf|tb-tree-node", html))), json_nodes + tree_items * json_depth)
})

test_that("NDJSON shows one tree item a line, the first lines only", {
  html <- json_html(c('{"a": 1}', '{"a": 2}', '{"b": [true]}'), ext = "ndjson")
  expect_match(html, '<code class="tb-tree-name">[[3]]</code>', fixed = TRUE)
  expect_match(html, '<span class="tb-tree-type">{1}</span>', fixed = TRUE)
  html <- json_html(c('{"a": 1}', 'not json'), ext = "jsonl")
  expect_match(html, "Not valid JSON", fixed = TRUE)
})

test_that("R lists keep their tree as before", {
  html <- as.character(list_tree(list(a = 1:3, b = list(c = "x", d = list(e = 1)))))
  expect_match(html, '<span class="tb-tree-type">int [3]</span>', fixed = TRUE)
  expect_match(html, '<span class="tb-tree-type">list [2]</span>', fixed = TRUE)
  expect_no_match(html, '<code class="tb-tree-name">e</code>', fixed = TRUE) # two levels
})
