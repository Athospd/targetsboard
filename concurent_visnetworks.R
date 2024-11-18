library(targetsboard)


a <- tar_visnetwork_bg(targets_only = TRUE, script = "inst/test_targets/targets_randomly_breaking.R")
b <- a$process()




tar_vis_tempdir <- tempdir()
tar_vis_path <- fs::path(tar_vis_tempdir, "tar_visnetwork.rds")
script = "inst/test_targets/targets_randomly_breaking.R"
tar_visnetwork_instance <- \() {
  while(TRUE) {
    tar_vis_obj <- targets::tar_visnetwork(script = script, targets_only = TRUE)
    saveRDS(tar_vis_obj, file = tar_vis_path)
    print("tar_visnetwork updated")
    NULL
  }
}

tar_visnetwork_instance()

 bg_process <- callr::r_bg(
  tar_visnetwork_instance,
  supervise = TRUE,
  poll_connection = TRUE,
  stdout = "|",
  stderr = "|"
)
a = list(bg_process, tar_vis_path)


a <- bg_process
a()




#' @export
tar_board <- function() {
  bg_process <- callr::r_bg(
    \() shiny::runApp(port = 9999),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = "|",
    stderr = "|"
  )
  invisible(bg_process)
}

a <- tar_board()

#' @export
tar_visnetwork_bg <- function(script, ...) {
  tar_vis_tempdir <- tempdir()
  tar_vis_path <- fs::path(tar_vis_tempdir, "tar_visnetwork.rds")

  tar_visnetwork_instance <- \(script, tar_vis_tempdir, tar_vis_path, ...) {
    while(TRUE) {
      tar_vis_obj <- targets::tar_visnetwork(script = script, ...)
      saveRDS(tar_vis_obj, file = tar_vis_path)
      print("tar_visnetwork updated")
    }
    return(NULL)
  }
  bg_process <- callr::r_bg(
    tar_visnetwork_instance,
    args = list(script = script, tar_vis_tempdir = tar_vis_tempdir, tar_vis_path = tar_vis_path, ...),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = "|",
    stderr = "|"
  )
  invisible(list(process = bg_process, tar_vis_path = tar_vis_path))
}


a <- tar_visnetwork_bg(targets_only = TRUE, script = "inst/test_targets/targets_randomly_breaking.R")
a$process$is_alive()
# a$process$get_result()
a$process$kill()







