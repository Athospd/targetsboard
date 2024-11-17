# targets::tar_visnetwork(targets_only = TRUE, script = "inst/test_targets/targets_randomly_breaking.R")
library(coro)
tar_visnetwork_generator <- async_generator(function() { 
  while (TRUE) { 
    vn <- targets::tar_visnetwork(targets_only = TRUE, script = "inst/test_targets/targets_randomly_breaking.R")
    yield(vn) 
  } 
})


async_vis <- async(function() {
  async_sleep(10)
  stream <- tar_visnetwork_generator()
  values <- await(async_collect(stream, 2))
  values
})


a <- async_vis()
