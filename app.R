library(shiny)
library(targetsboard)
library(promises)
library(bslib)

metadata <- targets::tar_visnetwork(targets_only = TRUE, script = "inst/test_targets/targets_randomly_breaking.R")
saveRDS(metadata, "metadata.rds")
metadata <- readRDS("metadata.rds")




shinyApp(app_ui(metadata), app_server(metadata), options = list(port = 9999))

