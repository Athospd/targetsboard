# Pipelines whose metadata lives in the cloud (#111): the board says so, and
# downloads it only when asked, behind a confirmation.

cloud_script <- function(dir, repository = "gcp") {
  writeLines(c(
    "library(targets)",
    sprintf("tar_option_set(repository_meta = '%s',", repository),
    "  resources = tar_resources(gcp = tar_resources_gcp(bucket = 'my-bucket', prefix = 'proj')))",
    "list(tar_target(a, 1))"
  ), file.path(dir, "_targets.R"))
  file.path(dir, "_targets.R")
}

test_that("the poller finds where a pipeline's metadata lives in the cloud", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  withr::defer(targets::tar_option_reset())
  found <- poller_why(cloud_script(dir))
  expect_equal(found$cloud, list(repository = "gcp", bucket = "my-bucket", prefix = "proj"))
  # The poller runs each in a fresh process: here, the options set above go.
  targets::tar_option_reset()
  writeLines(c("library(targets)", "list(tar_target(a, 1))"), "_targets.R")
  expect_null(poller_why("_targets.R")$cloud) # local metadata: nothing to say
})

test_that("cloud_meta_notice() says where the metadata is, and offers the download", {
  html <- as.character(cloud_meta_notice(list(repository = "aws", bucket = "b", prefix = "p"), button_id = "x"))
  expect_match(html, "metadata lives in the cloud", fixed = TRUE)
  expect_match(html, "(aws, bucket b, prefix p):", fixed = TRUE) # no stray spaces
  expect_match(html, "Download metadata", fixed = TRUE)
  expect_null(cloud_meta_notice(NULL, button_id = "x"))
})

test_that("Download metadata asks first, then downloads, and says what went wrong", {
  calls <- character()
  download <- function(script) {
    calls <<- c(calls, script)
    if (length(calls) > 1) stop("there is no package called 'googleCloudStorageR'")
  }
  shiny::testServer(cloud_meta_server, args = list(
    cloud = shiny::reactive(list(repository = "gcp", bucket = "b", prefix = NULL)),
    script = shiny::reactive("_targets.R"), download = download, refresh = function() NULL
  ), {
    session$setInputs(download = 1)
    expect_length(calls, 0) # a confirmation first: it may cost
    session$setInputs(confirm = 1)
    expect_equal(calls, "_targets.R")
    session$setInputs(confirm = 2)
    expect_match(last_error(), "googleCloudStorageR", fixed = TRUE)
  })
})

test_that("on the board: the notice, the confirmation, and the download's error in plain words", {
  dir <- local_pipeline()
  cloud_script(dir)
  app <- local_board_app(dir, "cloud-meta")
  app$wait_for_js("document.querySelector('.tb-cloud-meta') !== null", timeout = 60000)
  expect_match(app$get_text(".tb-cloud-meta"), "gcp, bucket my-bucket, prefix proj", fixed = TRUE)
  app$click(selector = ".tb-cloud-meta button")
  app$wait_for_js("document.querySelector('.modal-dialog') !== null", timeout = 10000)
  expect_match(app$get_text(".modal-dialog"), "tar_meta_download()", fixed = TRUE)
  app$click(selector = ".modal-dialog .btn-primary")
  # Without googleCloudStorageR here, it fails: the cause, not callr's wrapper.
  app$wait_for_js(
    "[...document.querySelectorAll('.shiny-notification')].some(n => n.textContent.includes(\"Couldn't download the metadata\"))",
    timeout = 60000
  )
  text <- app$get_js("[...document.querySelectorAll('.shiny-notification')].map(n => n.textContent).join(' ')")
  if (!nzchar(system.file(package = "googleCloudStorageR"))) expect_match(text, "googleCloudStorageR", fixed = TRUE)
  expect_no_match(text, "callr subprocess", fixed = TRUE)
})
