"""Builds demo/v3/targetsboard-demo-v3.mp4: take three of the demo. Take
two's path, with the app's waits cut out, zooms framed on what they show, a
solid code card, the fix told in three steps, a still board before the end
card, and a lo-fi jazz soundtrack. No intro card: it opens on the board, and
the logo comes at the end. The README links this take, so it also draws
the README's poster, man/figures/README-demo.jpg.

  1. record.R        drives the board at 2x and logs every event  -> build/
  2. ../cards.R      renders the end card                          -> build/end.png
  3. soundtrack.py   synthesizes the music to the cut's length      -> build/soundtrack.wav
  4. assemble.py     speed ramps, cuts, camera, cursor, captions, cards -> the video

    python demo/v3/make-demo-v3.py              # everything
    python demo/v3/make-demo-v3.py --no-record  # reuse the recording

Needs what ../v1/make-demo-v1.py needs; R CMD INSTALL . first.
"""
import os
import pathlib
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
DEMO = HERE.parent
ROOT = DEMO.parent
OUT = HERE / "build"
VIDEO = HERE / "targetsboard-demo-v3.mp4"
POSTER_AT = 15.5  # the README poster's frame: the whole run, with its warning and error marks

sys.path.insert(0, str(DEMO))
from common import poster, rscript  # noqa: E402


def run(*args):
    env = dict(os.environ, DEMO_DIR=str(DEMO), DEMO_OUT=str(OUT), NOT_CRAN="true")
    subprocess.run(list(args), cwd=ROOT, env=env, check=True)


def main():
    OUT.mkdir(exist_ok=True)
    r = rscript()
    if "--no-record" not in sys.argv:
        run(r, str(HERE / "record.R"))
    run(r, str(DEMO / "cards.R"))
    probe = subprocess.run([sys.executable, str(HERE / "assemble.py"), str(OUT), str(OUT / "end.png"), "-", "-"],
                           capture_output=True, text=True, check=True).stdout
    total = float(probe.split("total ")[1].split(" s")[0])
    run(sys.executable, str(HERE / "soundtrack.py"), f"{total + 0.5:.2f}", str(OUT / "soundtrack.wav"))
    run(sys.executable, str(HERE / "assemble.py"), str(OUT), str(OUT / "end.png"), str(OUT / "soundtrack.wav"), str(VIDEO))
    print(f"{VIDEO} ({total:.1f} s) and {poster(VIDEO, OUT, POSTER_AT)}")


if __name__ == "__main__":
    main()
