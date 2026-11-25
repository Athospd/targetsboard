# targetsboard: logo concepts and brand sources

A storage branch, kept apart from main so the package doesn't carry these
assets. Nothing here is built or installed.

- `dev/logo/make_logo.py` writes every brand file from one definition of
  the Pulse mark: `inst/app/www/{wordmark,mark,favicon,pulse-spinner}.svg`,
  `man/figures/logo.svg`, and the ink and white versions here. It needs
  fontTools and the Bahnschrift font.
- `dev/logo/render_logo.R` renders the PNGs (`man/figures/logo.png`,
  `inst/app/www/favicon.png`, `dev/logo/*.png`) with headless Chrome.
- `dev/logo/concepts/`: the 25 concepts and the hex variants (#105).
- `dev/logo/old-hex/`: the hex before the Pulse, and its scripts.

To change the brand: check out main, copy `dev/logo/` from this branch into
it (`git checkout logo-concepts -- dev/logo`), run both scripts from the
package folder, commit only the generated files to main, and commit any
change to the scripts back here.
