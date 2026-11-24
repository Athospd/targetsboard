test_that("send_on_click() writes the input and its value, as JSON, for actions.js", {
  button <- as.character(send_on_click(shiny::tags$button("Go"), "pick", 3L))
  expect_match(button, 'data-tb-input="pick" data-tb-value="3"', fixed = TRUE)
  # A string, quoted: names with quotes or spaces survive.
  expect_equal(
    jsonlite::fromJSON(htmltools::tagGetAttribute(send_on_click(shiny::tags$a(), "show", 'odd "name" here'), "data-tb-value")),
    'odd "name" here'
  )
})

test_that("the board's buttons and links have no JavaScript of their own", {
  pattern <- list(branches = 5L, branch = 2L, branch_name = "b", total_bytes = 1)
  html <- paste(
    as.character(branch_picker(pattern, limit = 1e6)),
    as.character(files_sections(list(paths = sprintf("f%d.txt", 1:60)))),
    as.character(code_block("Read", "read", "x")),
    as.character(copy_code_button("x")),
    as.character(problem_ui(
      list(name = "t", type = "stem", error = "boom", workspace = TRUE, command = "f(a)", functions = "f", branches = list()),
      inputs = "a", script = "_targets.R"
    ))
  )
  expect_no_match(html, "onclick|onchange|ontoggle|setInputValue")
  files <- as.character(files_sections(list(paths = sprintf("f%d.txt", 1:60))))
  expect_match(files, 'data-tb-input="value_preview_file_open" data-tb-value="2" data-tb-output="value_preview_file_2"', fixed = TRUE)
  expect_match(files, 'data-tb-more="50"', fixed = TRUE)
})
