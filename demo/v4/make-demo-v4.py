"""Builds take four (#179): a ~100-second pitch and a 30-second vertical cut.

  1. record.R            drives the board and logs every event        -> build/
  2. cards.R             the Pulse-brand cards and the thumbnail       -> build/*.png
  3. ../v3/soundtrack.py synthesizes the music to each cut's length    -> build/*.wav
  4. assemble.py         the hook, the take, the cards, the vertical cut -> the videos

    python demo/v4/make-demo-v4.py              # everything
    python demo/v4/make-demo-v4.py --no-record  # reuse the recording

Needs what ../v1/make-demo-v1.py needs; R CMD INSTALL . first, from main.
The videos, the thumbnail and build/ are not committed (#158).
"""
import os
import pathlib
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
DEMO = HERE.parent
ROOT = DEMO.parent
OUT = HERE / "build"
VIDEO = HERE / "targetsboard-demo-v4.mp4"
SHORT = HERE / "targetsboard-demo-v4-vertical.mp4"
THUMB = HERE / "targetsboard-demo-v4-thumbnail.png"

sys.path.insert(0, str(DEMO))
from common import rscript  # noqa: E402


def run(*args):
    env = dict(os.environ, DEMO_DIR=str(DEMO), DEMO_OUT=str(OUT), NOT_CRAN="true")
    subprocess.run(list(args), cwd=ROOT, env=env, check=True)


def total(*flags):
    probe = subprocess.run([sys.executable, str(HERE / "assemble.py"), str(OUT), "-", "-", *flags],
                           capture_output=True, text=True, check=True).stdout
    return float(probe.split("total ")[1].split(" s")[0])


def thumbnail_still():
    """The board for the thumbnail: the run's frame with the errored target
    and the branch counts, the hook's last frame before the fix."""
    sys.path.insert(0, str(HERE))
    import assemble
    tl = assemble.timeline(str(OUT))
    errored = next(e["t"] for e in tl.of("caption") if e["text"].startswith("Errors and warnings"))
    n = tl.frame_at(errored + 0.3)
    from PIL import Image
    Image.open(OUT / "frames" / f"{n:06d}.jpg").convert("RGB").save(OUT / "thumb-board.jpg", quality=92)


def main():
    OUT.mkdir(exist_ok=True)
    r = rscript()
    if "--no-record" not in sys.argv:
        run(r, str(HERE / "record.R"))
    thumbnail_still()
    run(r, str(HERE / "cards.R"))
    (OUT / "thumbnail.png").replace(THUMB)
    for flags, video, wav in [([], VIDEO, "soundtrack.wav"), (["--vertical"], SHORT, "soundtrack-v.wav")]:
        seconds = total(*flags)
        run(sys.executable, str(DEMO / "v3" / "soundtrack.py"), f"{seconds + 0.5:.2f}", str(OUT / wav))
        run(sys.executable, str(HERE / "assemble.py"), str(OUT), str(OUT / wav), str(video), *flags)
        print(f"{video} ({seconds:.1f} s)")
    print(THUMB)


if __name__ == "__main__":
    main()
