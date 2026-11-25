# Every theme of board_themes() on the real board, in light and dark mode:
# its text is readable (WCAG contrast), and its canvas is its own. Slow (one
# board per theme): on CI it runs on Linux only.

test_that("every board theme keeps the board readable, in both modes", {
  skip_if(nzchar(Sys.getenv("CI")) && Sys.info()[["sysname"]] != "Linux", "Linux CI only")
  dir <- local_pipeline(make = TRUE)
  themes <- board_themes()
  contrast_js <- paste(
    "(() => { const rgba = c => { const v = c.match(/[0-9.]+/g).map(Number); return v.length < 4 ? [...v, 1] : v; };",
    "const lum = c => { const [r, g, b] = c.slice(0, 3).map(v => { v /= 255;",
    "  return v <= 0.03928 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4; });",
    "  return 0.2126 * r + 0.7152 * g + 0.0722 * b; };",
    "const bg = el => { for (; el; el = el.parentElement) { const c = rgba(getComputedStyle(el).backgroundColor);",
    "  if (c[3] > 0.5) return c; } return [255, 255, 255, 1]; };",
    "const ct = (sel, prop = 'color', inner = null) => { const el = document.querySelector(sel); const b = bg(el);",
    "  const f = rgba(getComputedStyle(inner ? el.shadowRoot.querySelector(inner) : el)[prop]); const t = f.slice(0, 3).map((v, i) => v * f[3] + b[i] * (1 - f[3]));",
    "  const [x, y] = [lum(t), lum(b)]; return (Math.max(x, y) + 0.05) / (Math.min(x, y) + 0.05); };",
    "return { heading: ct('.sidebar-title'), run_all: ct('#board-run-all'), refresh: ct('#board-script-refresh'),",
    "  tab: ct('#board-view .nav-link:not(.active)'), toolbar: ct('.tb-node-toolbar button'), picker: ct('#theme-pick'),",
    "  switch: ct('.tb-mode-button'),",
    "  canvas: getComputedStyle(document.querySelector('.react-flow')).backgroundColor }; })()"
  )
  for (i in seq_len(nrow(themes))) {
    name <- themes$name[i]
    app <- local_board_app(dir, paste0("theme-", i), c(
      sprintf("options(targetsboard.theme = '%s')", name),
      "targetsboard::tar_board_app()"
    ))
    app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
    app$click(selector = ".react-flow__node[data-id='b']")
    app$wait_for_js("document.querySelector('.tb-node-toolbar button') !== null")
    modes <- if (themes$dark_only[i]) "dark" else c("light", "dark")
    expect_equal(app$get_js("document.querySelector('.tb-mode-button').hidden"), themes$dark_only[i])
    for (mode in modes) {
      app$run_js(sprintf("document.querySelector('bslib-input-dark-mode').setAttribute('mode', '%s')", mode))
      app$wait_for_js(sprintf("document.documentElement.dataset.bsTheme === '%s'", mode))
      wait_for_widget_mode(app, mode)
      wait_for_settled(app)
      got <- app$get_js(contrast_js)
      label <- sprintf("%s, %s mode", name, mode)
      for (part in c("heading", "run_all", "refresh", "toolbar", "picker")) {
        expect_gte(got[[part]], 4.5, label = paste(label, part))
      }
      expect_gte(got$tab, 3, label = paste(label, "inactive tab")) # bold, large-ish
      if (!themes$dark_only[i]) { # else hidden
        expect_gte(got$switch, 3, label = paste(label, "dark mode button")) # an icon
      }
      if (is.na(themes$preset[i])) {
        expected <- if (mode == "light") "rgb(240, 229, 215)" else "rgb(20, 20, 20)"
        expect_equal(got$canvas, expected, label = paste(label, "canvas"))
      }
    }
    app$stop()
  }
})
