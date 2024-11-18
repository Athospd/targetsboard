library(shiny)
library(targetsboard)
library(promises)
library(bslib)
# metadata <- targets::tar_visnetwork(targets_only = TRUE, script = "inst/test_targets/targets_randomly_breaking.R")
# saveRDS(metadata, "metadata.rds")
script = "inst/test_targets/targets_randomly_breaking.R"
metadata <- readRDS("metadata.rds")$x


shinyApp(app_ui(metadata, script), app_server(metadata), options = list(port = 9999))

