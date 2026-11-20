# Rebuild the JS bundle (use `npm run dev` for a build with a source map),
# regenerate docs and reload the package, then run the board.
system("npm run build")
devtools::document()
devtools::load_all()

shiny::runApp("dev/app.R")
