test_that("board_configuration() carries the id and converted data", {
  config <- board_configuration("dag", fake_nodes(), fake_edges(), "500px", "100%")

  expect_equal(config$id, "dag")
  expect_equal(config$height, "500px")
  expect_length(config$nodes, 3)
  expect_length(config$edges, 2)
  expect_equal(config$nodes[[1]]$id, "raw")
})

test_that("targetsboard() is a reactR Shiny input with the JS bundle", {
  tag <- targetsboard("dag", fake_nodes(), fake_edges())
  html <- as.character(tag)
  deps <- htmltools::findDependencies(tag)

  expect_match(html, "targetsboard")
  expect_match(html, '"id":"dag"', fixed = TRUE)
  expect_true("targetsboard" %in% vapply(deps, `[[`, "", "name"))
})

test_that("update_targetsboard() sends the full configuration", {
  sent <- NULL
  session <- list(sendInputMessage = function(inputId, message) {
    sent <<- list(inputId = inputId, message = message)
  })

  update_targetsboard(session, "dag", fake_nodes(), fake_edges())

  expect_equal(sent$inputId, "dag")
  expect_equal(sent$message$configuration$id, "dag")
  expect_length(sent$message$configuration$nodes, 3)
})

test_that("update_targetsboard_status() sends only ids, node types and statuses", {
  sent <- NULL
  session <- list(sendCustomMessage = function(type, message) {
    sent <<- list(type = type, message = message)
  })

  update_targetsboard_status(session, "dag", c(raw = "completed", model = "dispatched"))

  expect_equal(sent$type, "targetsboard:status")
  expect_equal(sent$message$id, "dag")
  expect_equal(sent$message$nodes, list(
    list(id = "raw", type = "uptodate", target_status = "completed"),
    list(id = "model", type = "running", target_status = "dispatched")
  ))

  update_targetsboard_status(session, "dag", character())
  expect_equal(sent$message$nodes, list())
})

test_that("targets with warnings carry a warning mark, without changing status", {
  nodes <- dataframe_to_reactflow_node_data(fake_nodes(), warnings = "raw")
  expect_equal(vapply(nodes, function(n) n$data$warning, TRUE), c(TRUE, FALSE, FALSE))
  expect_equal(nodes[[1]]$type, "uptodate")

  config <- board_configuration("dag", fake_nodes(), fake_edges(), "500px", "100%")
  expect_false(any(vapply(config$nodes, function(n) n$data$warning, TRUE)))
})

test_that("update_targetsboard_status() can send warning marks alone", {
  sent <- NULL
  session <- list(sendCustomMessage = function(type, message) sent <<- message)

  update_targetsboard_status(session, "dag", warnings = "model")
  expect_equal(sent$nodes, list())
  expect_equal(as.character(sent$warnings), "model")
  # Always an array, also as Shiny serialises it (unboxing length-one vectors).
  expect_equal(as.character(jsonlite::toJSON(sent$warnings, auto_unbox = TRUE)), '["model"]')

  update_targetsboard_status(session, "dag", warnings = character())
  expect_equal(as.character(jsonlite::toJSON(sent$warnings, auto_unbox = TRUE)), "[]")

  update_targetsboard_status(session, "dag", c(raw = "completed"))
  expect_null(sent$warnings) # NULL leaves the marks as they are
})

test_that("targets and edges can't be deleted from the read-only board", {
  config <- board_configuration("dag", fake_nodes(), fake_edges(), "500px", "100%")
  expect_false(any(vapply(config$nodes, function(n) n$deletable, TRUE)))
  expect_false(any(vapply(config$edges, function(e) e$deletable, TRUE)))
})

test_that("targetsboard_options() has defaults and checks its values", {
  options <- targetsboard_options()
  expect_s3_class(options, "targetsboard_options")
  expect_equal(unclass(options), list(
    color_mode = "light", minimap = TRUE, controls = TRUE, background = "dots",
    edge_type = "default", only_render_visible = FALSE, node_actions = list(),
    toolbar_actions = list(), node_group_actions = list(), toolbar_group_actions = list(),
    download_image = FALSE
  ))
  expect_equal(targetsboard_options(edge_type = "step")$edge_type, "step")
  expect_error(targetsboard_options(color_mode = "blue"), "color_mode")
  expect_error(targetsboard_options(background = "stripes"), "background")
  expect_error(targetsboard_options(minimap = "yes"), "`minimap` must be TRUE or FALSE")
})

test_that("targetsboard() sends its options; update_targetsboard() keeps them by default", {
  html <- as.character(targetsboard(
    "dag", fake_nodes(), fake_edges(),
    options = targetsboard_options(color_mode = "dark", minimap = FALSE)
  ))
  expect_match(html, '"color_mode":"dark"', fixed = TRUE)
  expect_match(html, '"minimap":false', fixed = TRUE)

  sent <- NULL
  session <- list(sendInputMessage = function(inputId, message) sent <<- message)
  update_targetsboard(session, "dag", fake_nodes(), fake_edges())
  expect_null(sent$configuration$options) # the widget keeps its own
  update_targetsboard(
    session, "dag", fake_nodes(), fake_edges(),
    options = targetsboard_options(edge_type = "step")
  )
  expect_equal(sent$configuration$options$edge_type, "step")
  expect_error(update_targetsboard(
    session, "dag", fake_nodes(), fake_edges(),
    options = list(minimap = FALSE)
  ))
})

test_that("update_targetsboard_options() sends only the options it names, checked", {
  sent <- NULL
  session <- list(sendCustomMessage = function(type, message) {
    sent <<- list(type = type, message = message)
  })

  update_targetsboard_options(session, "dag", color_mode = "dark", minimap = FALSE)
  expect_equal(sent$type, "targetsboard:options")
  expect_equal(sent$message, list(id = "dag", options = list(color_mode = "dark", minimap = FALSE)))

  expect_error(update_targetsboard_options(session, "dag", edge_type = "zigzag"), "edge_type")
  expect_error(update_targetsboard_options(session, "dag", "dark"))
})

test_that("node_actions become the menu's items, checked", {
  options <- targetsboard_options(node_actions = c(run = "Run it", copy_name = "Copy"))
  expect_equal(options$node_actions, list(
    list(id = "run", label = "Run it"),
    list(id = "copy_name", label = "Copy")
  ))
  expect_match(as.character(targetsboard("dag", fake_nodes(), fake_edges(), options = options)),
               '"node_actions":[{"id":"run","label":"Run it"}', fixed = TRUE)
  expect_error(targetsboard_options(node_actions = c("Run it")), "named")
  expect_error(targetsboard_options(node_actions = c(run = "a", run = "b")), "unique")
  expect_error(targetsboard_options(node_actions = list(run = 1)), "character")
})

test_that("update_targetsboard_focus() and targetsboard_fit_view() send commands", {
  sent <- NULL
  session <- list(sendCustomMessage = function(type, message) {
    sent <<- list(type = type, message = message)
  })

  update_targetsboard_focus(session, "dag", "raw")
  expect_equal(sent$type, "targetsboard:command")
  expect_equal(sent$message, list(id = "dag", command = "focus", node = "raw"))
  update_targetsboard_focus(session, "dag", NULL)
  expect_equal(sent$message, list(id = "dag", command = "focus")) # the widget reads null
  expect_error(update_targetsboard_focus(session, "dag", c("a", "b")), "one target id")

  targetsboard_fit_view(session, "dag", "raw")
  expect_equal(sent$message$command, "fit_view")
  expect_equal(jsonlite::toJSON(sent$message$nodes), jsonlite::toJSON(I("raw"))) # an array
  expect_equal(sent$message[c("padding", "duration", "max_zoom")],
               list(padding = 0.2, duration = 400, max_zoom = 1.5))
  targetsboard_fit_view(session, "dag")
  expect_false("nodes" %in% names(sent$message)) # all targets
  expect_error(targetsboard_fit_view(session, "dag", 1), "character")
})

test_that("a group's menu and toolbar are options of their own, checked", {
  options <- targetsboard_options(
    node_actions = c(run = "Run it"),
    node_group_actions = c(run = "Run {n}", copy_name = "Copy {n} names"),
    toolbar_group_actions = c(invalidate = "Invalidate {n}")
  )
  expect_equal(options$node_group_actions, list(
    list(id = "run", label = "Run {n}"),
    list(id = "copy_name", label = "Copy {n} names")
  ))
  expect_equal(options$toolbar_group_actions, list(list(id = "invalidate", label = "Invalidate {n}")))
  expect_equal(options$toolbar_actions, list())
  expect_match(as.character(targetsboard("dag", fake_nodes(), fake_edges(), options = options)),
               '"toolbar_group_actions":[{"id":"invalidate","label":"Invalidate {n}"}]', fixed = TRUE)
  expect_error(targetsboard_options(node_group_actions = "Run"), "`node_group_actions` must be")
  expect_error(targetsboard_options(toolbar_group_actions = c(a = "x", a = "y")), "`toolbar_group_actions` must be")
})

test_that("update_targetsboard_selection() sends the targets to select, always as an array", {
  sent <- NULL
  session <- list(sendCustomMessage = function(type, message) {
    sent <<- list(type = type, message = message)
  })
  as_json <- function() as.character(jsonlite::toJSON(sent$message, auto_unbox = TRUE))

  update_targetsboard_selection(session, "dag", c("raw", "model"))
  expect_equal(sent$type, "targetsboard:command")
  expect_equal(as_json(), '{"id":"dag","command":"select","nodes":["raw","model"]}')
  update_targetsboard_selection(session, "dag", "raw")
  expect_equal(as_json(), '{"id":"dag","command":"select","nodes":["raw"]}')
  update_targetsboard_selection(session, "dag", NULL) # clears it
  expect_equal(as_json(), '{"id":"dag","command":"select","nodes":[]}')
  expect_error(update_targetsboard_selection(session, "dag", 1), "character")
})

test_that("toolbar_actions become the toolbar's buttons, checked", {
  options <- targetsboard_options(toolbar_actions = c(run = "Run"))
  expect_equal(options$toolbar_actions, list(list(id = "run", label = "Run")))
  expect_equal(options$node_actions, list()) # independent of the menu
  expect_error(targetsboard_options(toolbar_actions = "Run"), "`toolbar_actions` must be")
})

test_that("targetsboard_export_image() asks the widget for an image", {
  sent <- NULL
  session <- list(sendCustomMessage = function(type, message) sent <<- message)
  targetsboard_export_image(session, "dag")
  expect_equal(sent, list(id = "dag", command = "export_image", format = "png"))
  targetsboard_export_image(session, "dag", "svg")
  expect_equal(sent$format, "svg")
  expect_error(targetsboard_export_image(session, "dag", "gif"), "format")
})

test_that("save_targetsboard_image() writes base64 and URL-encoded images", {
  png <- withr::local_tempfile(fileext = ".png")
  bytes <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0xff))
  image <- list(format = "png", data_url = paste0("data:image/png;base64,", jsonlite::base64_enc(bytes)))
  expect_equal(save_targetsboard_image(image, png), png)
  expect_equal(readBin(png, "raw", 100), bytes)

  svg <- withr::local_tempfile(fileext = ".svg")
  image <- list(format = "svg", data_url = paste0(
    "data:image/svg+xml;charset=utf-8,", utils::URLencode("<svg><text>a & b</text></svg>", reserved = TRUE)
  ))
  save_targetsboard_image(image, svg)
  expect_equal(readLines(svg, warn = FALSE), "<svg><text>a & b</text></svg>")

  expect_error(save_targetsboard_image(list(format = "png", error = "boom"), png), "boom")
  expect_error(save_targetsboard_image(list(data_url = "nope"), png), "_image")
})

test_that("save_targetsboard_image() writes a graph-sized SVG quickly, byte for byte", {
  text <- paste0("<svg>", strrep("<text style=\"fill: #354823\">ação 100% &amp;</text>", 40000), "</svg>")
  encoded <- utils::URLencode(enc2utf8(text), reserved = TRUE)
  expect_gt(nchar(encoded), 3e6)
  svg <- withr::local_tempfile(fileext = ".svg")
  image <- list(format = "svg", data_url = paste0("data:image/svg+xml;charset=utf-8,", encoded))
  elapsed <- system.time(save_targetsboard_image(image, svg))[["elapsed"]]
  expect_lt(elapsed, 5)
  expect_identical(readBin(svg, "raw", file.size(svg)), charToRaw(enc2utf8(text)))
})
