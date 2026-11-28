# The board's session survives bad input (#283): an error in an observer
# ends the session, and the page goes grey with no word why (#253). Each of
# the inputs the page sends gets values it never should, and the session
# must still be open.

bad_values <- list(
  NULL, "", "no_such_target", "a b", 999L, -1L, NA, list(), TRUE, c("a", "b"),
  list(action = "nope"), list(action = "unfold", node = "no_such_target"), list(x = 1)
)

# The inputs that carry data, by module. Buttons that start a run, stop the
# app or change the store are left out: a bad value there is a click.
data_inputs <- c(
  paste0("board-graph-", c("flow_focused_node", "flow_double_clicked_node", "flow_selected_nodes",
                            "flow_node_action", "flow_branch_click", "flow_layout")),
  paste0("board-preview-", c("branch", "value_tab", "target_link", "inputs_open", "file_open",
                              "cell_open", "cell_drill", "sidebar")),
  "board-meta-row_clicked", "board-problems-row_clicked",
  paste0("board-script-", c("path", "project")),
  paste0("board-export-", c("which", "max_mb")),
  "board-preview-profile-on",
  paste0("theme-", c("pick", "mode", "saved"))
)

test_that("the board's session survives bad values in every input that carries data", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()
  failures <- character()
  for (id in data_inputs) {
    errors <- character()
    shiny::testServer(server, {
      backend_of(server)$stop_poller()
      session$setInputs(`board-graph-flow_focused_node` = "c", `board-graph-flow_double_clicked_node` = "c")
      for (value in bad_values) {
        # Shiny reports an observer's error as a warning, and closes the session.
        withCallingHandlers(tryCatch({
          do.call(session$setInputs, stats::setNames(list(value), id))
          session$elapse(100)
        }, error = function(e) errors <<- c(errors, conditionMessage(e))), warning = function(w) {
          if (startsWith(conditionMessage(w), "Error in")) {
            errors <<- c(errors, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        })
        if (session$isClosed() || length(errors)) {
          failures <<- c(failures, sprintf("%s = %s: %s", id, deparse1(value), paste(unique(errors), collapse = "; ")))
          break
        }
      }
    })
  }
  expect(length(failures) == 0, paste(c("Inputs whose bad values end the session:", failures), collapse = "\n"))
})
