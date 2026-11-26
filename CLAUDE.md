# targetsboard

A local board for a {targets} pipeline (R Shiny, bslib, a React Flow widget
in `srcjs/`). Words: `GLOSSARY.md`. Decisions: `docs/adr/`.

- **Reviews** use the checklist in [`.github/REVIEW.md`](.github/REVIEW.md),
  for any review, by a person or `/code-review`.
- **PRs** follow [`.github/pull_request_template.md`](.github/pull_request_template.md).
- **Tests**: browser tests run the installed package, so install the branch to
  a scratch library first; set `NOT_CRAN=true` to run them.
- **JS**: `npm ci && npm run build` after changing `srcjs/`; commit the bundle.
