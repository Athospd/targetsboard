# Headless Chrome, which the browser tests start, leaves `com.google.Chrome.*`
# folders in TMPDIR on Linux and macOS, and R CMD check reports them as
# detritus in the temp directory. Chrome, and every other process the tests
# start, gets a TMPDIR inside this session's own tempdir() instead, which R
# deletes when the session ends.
local({
  chrome_tmp <- file.path(tempdir(), "processes")
  dir.create(chrome_tmp, showWarnings = FALSE)
  withr::local_envvar(TMPDIR = chrome_tmp, .local_envir = testthat::teardown_env())
})
