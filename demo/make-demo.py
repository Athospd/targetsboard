"""Builds demo/targetsboard-demo.mp4, the walkthrough linked from the README,
and its poster, man/figures/README-demo.jpg.

A real recording of the board (the installed targetsboard: R CMD INSTALL .
first) on an air-quality pipeline, in headless Chrome:

  1. record_demo.R   drives the board and records it   -> build/frames/
  2. cards.R         renders the title and end cards     -> build/*.png
  3. soundtrack.py   synthesizes the music to length     -> build/soundtrack.wav
  4. assemble.py     puts it together with ffmpeg        -> targetsboard-demo.mp4
  5. the poster: a frame of the run, with a play button  -> man/figures/README-demo.jpg

    python demo/make-demo.py              # everything
    python demo/make-demo.py --no-record  # keep the frames, redo the rest

Needs R with shinytest2 and chromote (and skimr, ggplot2, broom, gt for the
demo pipeline), Chrome, and Python with numpy, pillow and imageio-ffmpeg.
The video is attached to the GitHub release, not committed.
"""
import csv
import glob
import os
import pathlib
import shutil
import subprocess
import sys

import imageio_ffmpeg
from PIL import Image, ImageDraw, ImageFilter

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent
OUT = HERE / "build"
VIDEO = HERE / "targetsboard-demo.mp4"
POSTER = ROOT / "man" / "figures" / "README-demo.jpg"


def rscript():
    found = os.environ.get("RSCRIPT") or shutil.which("Rscript")
    if not found:
        candidates = sorted(glob.glob("C:/Program Files/R/R-*/bin/Rscript.exe"))
        found = candidates[-1] if candidates else None
    if not found:
        raise SystemExit("Rscript not found: put it on the PATH or set RSCRIPT.")
    return found


def run(*args):
    env = dict(os.environ, DEMO_DIR=str(HERE), DEMO_OUT=str(OUT), NOT_CRAN="true")
    subprocess.run(list(args), cwd=ROOT, env=env, check=True)


def poster(at=25.0):
    src = OUT / "poster_src.png"
    subprocess.run([imageio_ffmpeg.get_ffmpeg_exe(), "-y", "-loglevel", "error", "-ss", str(at),
                    "-i", str(VIDEO), "-frames:v", "1", str(src)], check=True)
    im = Image.open(src).convert("RGB").resize((1280, 720), Image.LANCZOS).convert("RGBA")
    im = Image.alpha_composite(im, Image.new("RGBA", im.size, (47, 34, 61, 70)))
    cx, cy, r = 640, 330, 70
    glow = Image.new("RGBA", im.size, (0, 0, 0, 0))
    ImageDraw.Draw(glow).ellipse([cx - r - 8, cy - r - 4, cx + r + 8, cy + r + 12], fill=(0, 0, 0, 110))
    im = Image.alpha_composite(im, glow.filter(ImageFilter.GaussianBlur(12)))
    d = ImageDraw.Draw(im)
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=(47, 34, 61, 235), outline=(242, 201, 76, 255), width=5)
    d.polygon([(cx - 22, cy - 36), (cx - 22, cy + 36), (cx + 40, cy)], fill=(247, 239, 228, 255))
    im.convert("RGB").save(POSTER, quality=86, optimize=True)


def main():
    OUT.mkdir(exist_ok=True)
    if "--no-record" not in sys.argv:
        run(rscript(), str(HERE / "record_demo.R"))
    run(rscript(), str(HERE / "cards.R"))
    times = [float(r["t"]) for r in csv.DictReader(open(OUT / "frames.csv"))]
    length = 5 + (times[-1] - times[0] + 0.5) + 7.5  # title + board + end card
    run(sys.executable, str(HERE / "soundtrack.py"), f"{length:.2f}", str(OUT / "soundtrack.wav"))
    run(sys.executable, str(HERE / "assemble.py"), str(OUT), str(VIDEO))
    poster()
    print(f"{VIDEO} ({length:.1f} s) and {POSTER}")


if __name__ == "__main__":
    main()
