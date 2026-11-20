library(shiny)
library(targetsboard)
library(promises)
library(bslib)
script = "inst/examples/pipelines/targets_massive_pipeline.R"
metadata <- targets::tar_visnetwork(targets_only = TRUE, script = script)
saveRDS(metadata, "dev/metadata.rds")
metadata <- readRDS("dev/metadata.rds")$x


tar_board_app(script, metadata = metadata, options = list(port = 9999))

