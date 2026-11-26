# The demo videos

Real recordings of the board, in headless Chrome, on an air-quality
pipeline. Each take has its own folder and builds its own video:

| Take | Video | What it is |
|---|---|---|
| `v1/` | `targetsboard-demo-v1.mp4` | the first 2.5-minute walkthrough, with a title card |
| `v2/` | `targetsboard-demo-v2.mp4` | faster, on DAG control, values and debugging; camera moves and chapter cards drawn in post |
| `v3/` | `targetsboard-demo-v3.mp4` | v2's path with the waits cut out, zooms framed on what they show, the fix in three steps, and a jazz soundtrack; linked from the package README |
| `v4/` | `targetsboard-demo-v4.mp4`, `targetsboard-demo-v4-vertical.mp4` | a ~100-second pitch, "See it. Understand it. Fix it. Without rerunning what's already done.": a fast hook (run, fail, fix, rerun one), five chapters on a 21-target pipeline, the Pulse-brand cards and thumbnail; plus a 30-second vertical cut for social media (#179) |
| `projects/` | `targetsboard-projects.mp4` | a 90-second tutorial on targets projects (`_targets.yaml`), on `dev/projects/`'s sandbox, following its `play.R` |

Take one goes like this:

1. **Watch it run**: Run all, spinners and flowing edges, a pattern's branch
   counts, then the warning and error marks.
2. **Values**: a skimr summary, two ggplots and a gt HTML report in the
   preview.
3. **Your DAG, your way**: drag, Tidy, the right-click menu selecting a
   lineage, and Ctrl-click on a group.
4. **Debugging**: the Problems tab and the Problem pane. Then the script is
   fixed and the errored target alone is rebuilt.
5. **And more**: the Log tab and dark mode.

Each take's soundtrack is synthesized from scratch by its `soundtrack.py`:
no samples, no licences.

## Rebuilding a take

From the package folder, with the package installed (the recordings use the
installed targetsboard):

```sh
R CMD INSTALL .
python demo/v1/make-demo-v1.py              # about 4 minutes
python demo/v1/make-demo-v1.py --no-record  # reuse the frames, redo cards, music and video
```

`v2/make-demo-v2.py`, `v3/make-demo-v3.py`, `v4/make-demo-v4.py` and
`projects/make-demo-projects.py` work the same way. The projects take reuses take three's soundtrack and
assembler, and drives the board with `driver.R`: take three's driving
helpers (cursor, clicks, captions, camera, cuts), plus a console card for R
code and file contents.

It needs:
- **R:** shinytest2, chromote, skimr, ggplot2, broom and gt;
- **Chrome;**
- **Python:** numpy, pillow and imageio-ffmpeg (which brings its own ffmpeg).

Intermediate files go to each take's `build/`. Neither they nor the videos
are committed: they are on YouTube, v1 at https://youtu.be/z2TOGc4vmFk and
v3 at https://youtu.be/pEsTN9rke2U. The README links to v3 through the
poster `man/figures/README-demo.jpg`, which `v3/make-demo-v3.py` rewrites
too.

To check a take without watching it, make a contact sheet: a frame every 5
seconds of the recording.

```sh
python demo/sheet.py 0 150 5 sheet.png demo/v1/build
```

v2 and v3 can also render single frames of the finished cut:
`python demo/v3/assemble.py demo/v3/build demo/v3/build/end.png - - --stills 5 20 40`.

## Files

Shared by every take, in `demo/`:

| File | Role |
|---|---|
| `pipeline.R` | the demo pipeline, broken or fixed |
| `cards.R`, `cards/` | the title and end cards |
| `common.py` | finding Rscript |
| `sheet.py` | contact sheets, to review a take |

In each take's folder:

| File | Role |
|---|---|
| `make-demo-v*.py` | runs every step |
| `record.R` | the scenes: cursor moves, clicks, captions (v2, v3: also camera moves and cuts, as events) |
| `soundtrack.py` | the music |
| `assemble.py` | frames, cards and music into the video |
| `v1/recorder.R` | the screencast to `build/frames/` |
| `v1/overlay.js` | the cursor, click ripples, captions and code card, drawn over the page |

Take four has its own pipeline (`v4/pipeline.R`, with a second project for
the projects scene), cards (`v4/cards/`, rendered by `v4/cards.R`) and an
assembler on top of take three's (`v4/assemble.py`): it adds the hook, the
tagline card, tooltips drawn in post and the vertical cut. Its
`record.R` marks the hook's stretches with `hook()` and the vertical cut's
with `short()`. Check a cut with
`python demo/v4/assemble.py demo/v4/build - - [--vertical] --stills 5 20`.
