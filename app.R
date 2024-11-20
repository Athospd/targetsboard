library(shiny)
library(targetsboard)
library(promises)
library(bslib)
script = "inst/test_targets/targets_massive_pipeline.R"
metadata <- targets::tar_visnetwork(targets_only = TRUE, script = script)
saveRDS(metadata, "metadata.rds")
metadata <- readRDS("metadata.rds")$x


shinyApp(app_ui(metadata, script), app_server(metadata), options = list(port = 9999))

