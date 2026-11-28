# Profile a target (#141).

test_that("profile_code() is the reproduce code, its command under profvis", {
  code <- profile_code("b", "f(a)\ng(a)", FALSE, inputs = "a", seed = 7L)
  expect_match(code, "targets::tar_load(c(a)) # its inputs", fixed = TRUE)
  expect_match(code, "set.seed(7)", fixed = TRUE)
  expect_true(endsWith(code, "profvis::profvis({\n  f(a)\n  g(a)\n})"))
  expect_match(profile_code("b", "f(a)", TRUE), "targets::tar_workspace(b)", fixed = TRUE)
})

test_that("Debug gives the profile block it's handed", {
  html <- as.character(debug_ui("b", command = "f(a)", profile_block = shiny::tags$div(class = "tb-profile-here")))
  expect_match(html, "tb-profile-here", fixed = TRUE)
})

# A process that has already ended, as callr's does.
fake_process <- function(result = NULL, error = NULL) {
  list(is_alive = function() FALSE, get_result = function() if (!is.null(error)) stop(error) else result)
}

test_that("Profile is off by default; on, it gives the code and runs it in the background", {
  withr::local_options(targetsboard.profile = NULL)
  started <- 0
  start <- function(code, dir, out) {
    started <<- started + 1
    writeLines("<html></html>", out)
    fake_process()
  }
  target <- shiny::reactiveVal(list(name = "b", code = "profvis::profvis(f(a))", seconds = 2, dir = tempdir()))
  shiny::testServer(profile_server, args = list(target = target, start = start), {
    html <- output$block$html
    expect_match(html, "Profile", fixed = TRUE)
    expect_no_match(html, "profvis::profvis", fixed = TRUE)
    session$setInputs(on = TRUE)
    html <- output$block$html
    expect_match(html, "profvis::profvis(f(a))", fixed = TRUE)
    skip_if_not_installed("profvis")
    session$setInputs(profile = 1)
    expect_equal(started, 1)
    session$flushReact()
    expect_equal(session$returned()$name, "b")
    expect_match(session$returned()$url, "b.html?v=", fixed = TRUE)
  })
})

test_that("options(targetsboard.profile = TRUE) turns it on from the start", {
  withr::local_options(targetsboard.profile = TRUE)
  target <- shiny::reactiveVal(list(name = "b", code = "profvis::profvis(f(a))", seconds = 2, dir = tempdir()))
  shiny::testServer(profile_server, args = list(target = target), {
    expect_match(output$block$html, "profvis::profvis(f(a))", fixed = TRUE)
  })
})

test_that("a long build asks first; a failed profile says why", {
  skip_if_not_installed("profvis")
  withr::local_options(targetsboard.profile = TRUE)
  started <- 0
  start <- function(code, dir, out) {
    started <<- started + 1
    fake_process(error = "object 'a' not found")
  }
  target <- shiny::reactiveVal(list(name = "slow", code = "x", seconds = 600, dir = tempdir()))
  shiny::testServer(profile_server, args = list(target = target, start = start), {
    session$setInputs(profile = 1)
    expect_equal(started, 0) # asked, not started
    session$setInputs(confirm = 1)
    expect_equal(started, 1)
    session$flushReact()
    expect_equal(session$returned()$error, "object 'a' not found")
  })
})

test_that("a target that can't be profiled shows nothing", {
  shiny::testServer(profile_server, args = list(target = shiny::reactive(NULL)), {
    expect_null(output$block$html %||% NULL)
  })
})
