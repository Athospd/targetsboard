"""What the takes' make-demo-v*.py share: finding Rscript, and drawing the
README's poster from a video."""
import glob
import os
import pathlib
import shutil
import subprocess

POSTER = pathlib.Path(__file__).resolve().parent.parent / "man" / "figures" / "README-demo.jpg"


def rscript():
    found = os.environ.get("RSCRIPT") or shutil.which("Rscript")
    if not found:
        candidates = sorted(glob.glob("C:/Program Files/R/R-*/bin/Rscript.exe"))
        found = candidates[-1] if candidates else None
    if not found:
        raise SystemExit("Rscript not found: put it on the PATH or set RSCRIPT.")
    return found


def poster(video, out, at):
    """man/figures/README-demo.jpg: the frame of <video> at <at> seconds, dimmed,
    with a play button. The README links it to the video on YouTube."""
    import imageio_ffmpeg
    from PIL import Image, ImageDraw, ImageFilter
    src = pathlib.Path(out) / "poster_src.png"
    subprocess.run([imageio_ffmpeg.get_ffmpeg_exe(), "-y", "-loglevel", "error", "-ss", str(at),
                    "-i", str(video), "-frames:v", "1", str(src)], check=True)
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
    return POSTER
