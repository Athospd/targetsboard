system("yarn run webpack --mode=development")
# devtools::document()
# pak::local_install(upgrade = FALSE, ask = FALSE)
# library(targetsboard)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

shiny::runApp()

