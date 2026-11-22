"""Builds demo/v1/targetsboard-demo-v1.mp4, take one: the first walkthrough,
with a title card. The README links take three (../v3), which draws the poster.

A real recording of the board (the installed targetsboard: R CMD INSTALL .
first) on an air-quality pipeline, in headless Chrome:

  1. record.R        drives the board and records it   -> build/frames/
  2. ../cards.R        renders the title and end cards     -> build/*.png
  3. soundtrack.py   synthesizes the music to length     -> build/soundtrack.wav
  4. assemble.py     puts it together with ffmpeg        -> targetsboard-demo-v1.mp4

    python demo/v1/make-demo-v1.py              # everything
    python demo/v1/make-demo-v1.py --no-record  # keep the frames, redo the rest

Needs R with shinytest2 and chromote (and skimr, ggplot2, broom, gt for the
demo pipeline), Chrome, and Python with numpy, pillow and imageio-ffmpeg.
The video is attached to the GitHub release, not committed.
"""
import csv
import os
import pathlib
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
DEMO = HERE.parent
ROOT = DEMO.parent
OUT = HERE / "build"
VIDEO = HERE / "targetsboard-demo-v1.mp4"

sys.path.insert(0, str(DEMO))
from common import rscript  # noqa: E402


def run(*args):
    env = dict(os.environ, DEMO_DIR=str(DEMO), DEMO_OUT=str(OUT), NOT_CRAN="true")
    subprocess.run(list(args), cwd=ROOT, env=env, check=True)


def main():
    OUT.mkdir(exist_ok=True)
    if "--no-record" not in sys.argv:
        run(rscript(), str(HERE / "record.R"))
    run(rscript(), str(DEMO / "cards.R"))
    times = [float(r["t"]) for r in csv.DictReader(open(OUT / "frames.csv"))]
    length = 5 + (times[-1] - times[0] + 0.5) + 7.5  # title + board + end card
    run(sys.executable, str(HERE / "soundtrack.py"), f"{length:.2f}", str(OUT / "soundtrack.wav"))
    run(sys.executable, str(HERE / "assemble.py"), str(OUT), str(VIDEO))
    print(f"{VIDEO} ({length:.1f} s)")


if __name__ == "__main__":
    main()
