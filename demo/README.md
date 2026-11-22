# The demo video

`make-demo.py` builds `targetsboard-demo.mp4`, the 2.5-minute walkthrough
linked from the package README. It is a real recording of the board, in
headless Chrome, on an air-quality pipeline:

1. **Watch it run**: Run all, spinners and flowing edges, a pattern's branch
   counts, then the warning and error marks.
2. **Values**: a skimr summary, two ggplots and a gt HTML report in the
   preview.
3. **Your DAG, your way**: drag, Tidy, the right-click menu selecting a
   lineage, and Ctrl-click on a group.
4. **Debugging**: the Problems tab and the Problem pane. Then the script is
   fixed and the errored target alone is rebuilt.
5. **And more**: the Log tab and dark mode.

The soundtrack is synthesized from scratch by `soundtrack.py`: no samples,
no licences.

## Rebuilding it

From the package folder, with the package installed (the recording uses the
installed targetsboard):

```sh
R CMD INSTALL .
python demo/make-demo.py              # about 4 minutes
python demo/make-demo.py --no-record  # reuse the frames, redo cards, music and video
```

It needs:
- **R:** shinytest2, chromote, skimr, ggplot2, broom and gt;
- **Chrome;**
- **Python:** numpy, pillow and imageio-ffmpeg (which brings its own ffmpeg).

Intermediate files go to `build/`. Neither they nor the video are committed:
the video is on YouTube (https://youtu.be/z2TOGc4vmFk) and attached to the
GitHub release. The README links to YouTube through the poster
`man/figures/README-demo.jpg`, which `make-demo.py` rewrites too.

To check a take without watching it, make a contact sheet: a frame every 5
seconds of the recording.

```sh
python demo/sheet.py 0 150 5 sheet.png demo/build
```

| File | Role |
|---|---|
| `make-demo.py` | runs every step |
| `pipeline.R` | the demo pipeline, broken or fixed |
| `record_demo.R` | the scenes: cursor moves, clicks, captions |
| `recorder.R` | the screencast to `build/frames/` |
| `overlay.js` | the cursor, click ripples, captions and code card drawn over the page |
| `cards.R`, `cards/` | the title and end cards |
| `soundtrack.py` | the music |
| `assemble.py` | frames, cards and music into the video |
| `sheet.py` | contact sheets, to review a take |
