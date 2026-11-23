# Extracted from test-app.R:187

# test -------------------------------------------------------------------------
metadata <- list(nodes = fake_nodes(), edges = fake_edges())
html <- as.character(app_ui(metadata, "b.R", scripts = c("a.R", "b.R", "c.R")))
expect_match(html, 'id="script"><option', fixed = TRUE)
expect_match(html, '<option value="b.R" selected>b.R</option>', fixed = TRUE)
options <- regmatches(html, gregexpr('<option value="[^"]*"', html))[[1]]
expect_equal(options, paste0('<option value="', c("b.R", "a.R", "c.R"), '"'))
expect_no_match(html, '"create":true', fixed = TRUE)
expect_no_match(html, '"createOnBlur"', fixed = TRUE)
