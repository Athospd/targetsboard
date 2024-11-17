
meta <- readRDS("metadata.rds")

outputs <- purrr::map(meta$x$nodes$id, \(x) {
  exp <- quo(targets::tar_read(!!x))
  tryCatch(eval_tidy(exp), error = function(e) "No output availabe")
}) |> 
  set_names(meta$x$nodes$id)


train_data <- targets::tar_read("train_data")
test_data <- targets::tar_read("test_data")

