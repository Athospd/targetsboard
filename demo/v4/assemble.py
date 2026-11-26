"""Assembles take four (#179) from what record.R left in <out>, with take
three's machinery (../v3/assemble.py: speed ramps, cuts, cursor, camera,
captions, chapter, code and console cards), in the Pulse brand's colours,
plus:

- the hook: the stretches record.R marked with hook(), played first and
  fast, under one caption, then the tagline card;
- tooltips, drawn from the events record.R logged (a native tooltip isn't
  in the screencast);
- a vertical cut (1080 x 1920) for social media: the hook and the stretches
  marked with short(), the board in the middle and big captions under it.

    python demo/v4/assemble.py <out> <soundtrack.wav|-> <video|-> [--vertical] [--stills 5 20]

<out> holds the recording and the cards (cards.R): tagline.png, end.png,
and for the vertical cut tagline-v.png, end-v.png, frame-v.png.
"""
import importlib.util
import pathlib
import subprocess
import sys
import textwrap

import imageio_ffmpeg
from PIL import Image, ImageDraw

HERE = pathlib.Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("v3", HERE.parent / "v3" / "assemble.py")
v3 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(v3)

# ---- The Pulse brand ------------------------------------------------------------------
NAVY = (20, 28, 43)
TEAL = (60, 200, 180)
LIGHT = (238, 243, 249)
v3.INK, v3.CREAM = NAVY, LIGHT
FPS, W, H = v3.FPS, v3.W, v3.H
HOOK_SPEED = 3.5      # the hook plays this fast
HOOK_CAPTION = "Your targets pipeline, live"
TAGLINE = 3.2         # seconds of the tagline card
END_CARD = 5.5


def draw_ripples(img, tl, s):  # take three's, in teal
    d = ImageDraw.Draw(img)
    for e in tl.of("click"):
        age = s - e["t"]
        if 0 <= age < 0.55:
            f = age / 0.55
            r = (8 + 26 * f) * tl.scale
            x, y = e["x"] * tl.scale, e["y"] * tl.scale
            d.ellipse([x - r, y - r, x + r, y + r], outline=TEAL + (int(255 * (1 - f)),), width=int(4 * tl.scale))


def draw_tooltip(img, tl, s):
    """The latest tooltip event, if on: a dark box under the element, as the
    browser draws one, on the page (so the camera zooms on it too)."""
    e = tl.latest("tooltip", s)
    if not e or not e["on"]:
        return
    k = tl.scale
    f = v3.font("segoeui.ttf", int(13 * k))
    lines = []
    for para in e["text"].split("\n"):
        lines += textwrap.wrap(para, 46) or [""]
    pad, row = 8 * k, 18 * k
    w = max(f.getlength(t) for t in lines) + 2 * pad
    h = row * len(lines) + 2 * pad
    x = min(max(e["x"] * k - w / 2, 4 * k), img.width - w - 4 * k)
    y = e["y"] * k
    a = min(1, (s - e["t"]) / 0.15)
    d = ImageDraw.Draw(img)
    d.rounded_rectangle([x + 2 * k, y + 3 * k, x + w + 2 * k, y + h + 3 * k], 5 * k, fill=(0, 0, 0, int(60 * a)))
    d.rounded_rectangle([x, y, x + w, y + h], 5 * k, fill=(32, 36, 44, int(245 * a)), outline=(90, 96, 110, int(255 * a)))
    for i, t in enumerate(lines):
        d.text((x + pad, y + pad + i * row), t, font=f, fill=(240, 242, 246, int(255 * a)))


BASE_SPEED = 1.1     # the whole main cut, a touch faster than recorded


def timeline(out):
    """Take three's Timeline, every speed ramp times BASE_SPEED."""
    tl = v3.Timeline(out)
    for e in tl.events:
        if e["type"] == "speed":
            e["factor"] *= BASE_SPEED
    tl.events.insert(0, {"type": "speed", "t": tl.t0 - 1, "factor": BASE_SPEED})
    return tl


def render(tl, out, s, cache, caption=None, chapters=True, captions=True):
    """Take three's render(), with the tooltip, and the caption replaced
    (the hook) or left out (the vertical cut draws its own)."""
    n = tl.frame_at(s)
    if cache.get("n") != n:
        cache["n"], cache["img"] = n, Image.open(f"{out}/frames/{n:06d}.jpg").convert("RGBA")
    img = cache["img"].copy()
    draw_ripples(img, tl, s)
    draw_tooltip(img, tl, s)
    cx, cy = tl.cursor_at(s)
    v3.draw_cursor(img, cx * tl.scale, cy * tl.scale, tl.scale)
    x, y, z = tl.camera_at(s)
    sw, sh = img.width / z, img.height / z
    left = min(max(x * tl.scale - sw / 2, 0), img.width - sw)
    top = min(max(y * tl.scale - sh / 2, 0), img.height - sh)
    frame = img.resize((W, H), Image.BICUBIC, box=(left, top, left + sw, top + sh))
    frame = v3.code_overlay(frame, tl, s)
    frame = v3.console_overlay(frame, tl, s)
    if captions:
        if caption is None:
            v3.draw_caption(frame, tl, s, chapter_color(tl, s))
        else:
            img = v3.caption_image(caption, TEAL)
            frame.alpha_composite(img, (W // 2 - img.width // 2, H - 64 - img.height))
    if chapters:
        for e in tl.of("chapter"):
            v3.chapter_overlay(frame, e, s)
    return frame


def chapter_color(tl, s):
    e = tl.latest("chapter", s)
    return tuple(int(e["color"][i:i + 2], 16) for i in (1, 3, 5)) if e else TEAL


def marked(tl, kind):
    """The stretches between kind(TRUE) and kind(FALSE): (start, end, text)."""
    out, start = [], None
    for e in tl.of(kind):
        if e["on"]:
            start = e
        elif start is not None:
            out.append((start["t"], e["t"], start.get("text", "")))
            start = None
    return out


def stretch_samples(tl, stretches, speed):
    """Source times through the stretches at a steady speed, with record.R's
    cuts (skip()) left out. Returns (time, text, first-of-stretch)."""
    skips = marked(tl, "skip")
    out = []
    for a, b, text in stretches:
        s, first = a, True
        while s < b:
            jump = next((e for st, e, _ in skips if st <= s < e), None)
            if jump is not None:
                s = jump
                continue
            out.append((s, text, first))
            first = False
            s += speed / FPS
    return out


def card_frames(png, size, dur, fade_from=None):
    card = Image.open(png).convert("RGBA").resize(size, Image.LANCZOS)
    w, h = size
    n = int(dur * FPS)
    for i in range(n):
        t = i / FPS
        z = 1 + 0.04 * (t / dur)
        sw, sh = w / z, h / z
        f = card.resize(size, Image.BICUBIC, box=((w - sw) / 2, (h - sh) / 2, (w + sw) / 2, (h + sh) / 2))
        if fade_from is not None and t < 0.4:
            f = Image.blend(fade_from, f, v3.ease(t / 0.4))
        yield f


def fade_out(frames, dur=1.0):
    frames = list(frames)
    n = int(dur * FPS)
    for i, f in enumerate(frames):
        left = len(frames) - i
        if left <= n:
            f = Image.blend(f, Image.new("RGBA", f.size, (0, 0, 0, 255)), v3.ease(1 - left / n))
        yield f


# ---- 16:9 ----------------------------------------------------------------------------
def landscape(tl, out):
    cache = {}
    last = None
    for i, (s, _, first) in enumerate(stretch_samples(tl, marked(tl, "hook"), HOOK_SPEED)):
        f = render(tl, out, s, cache, caption=HOOK_CAPTION, chapters=False)
        if first and last is not None:  # a quick dissolve between stretches
            prev = last
            for k in range(4):
                yield Image.blend(prev, f, (k + 1) / 5)
        if i == 0:
            f = Image.blend(Image.new("RGBA", (W, H), (0, 0, 0, 255)), f, 0.6)
        last = f
        yield f
    yield from card_frames(f"{out}/tagline.png", (W, H), TAGLINE, fade_from=last)
    samples = tl.samples()
    cut_from = None
    for i, s in enumerate(samples):
        last = render(tl, out, s, cache)
        if i in tl.cuts:
            cut_from = (i, render(tl, out, tl.cuts[i], {}))
        if cut_from and i - cut_from[0] < v3.DISSOLVE * FPS:
            last = Image.blend(cut_from[1], last, v3.ease((i - cut_from[0] + 1) / (v3.DISSOLVE * FPS)))
        if i < 8:
            last = Image.blend(Image.new("RGBA", (W, H), NAVY + (255,)), last, (i + 1) / 9)
        yield last
    yield from fade_out(card_frames(f"{out}/end.png", (W, H), END_CARD, fade_from=last), 1.2)


# ---- Vertical ---------------------------------------------------------------------------
VW, VH = 1080, 1920
SHORT_SPEED = 1.5


def vertical_caption(text):
    f = v3.font("segoeuib.ttf", 64)
    lines = textwrap.wrap(text, 24)
    img = Image.new("RGBA", (VW, 90 * len(lines) + 20), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    for i, t in enumerate(lines):
        d.text(((VW - f.getlength(t)) / 2, 10 + 90 * i), t, font=f, fill=LIGHT)
    return img


def vertical(tl, out):
    """The hook, then the short() stretches: the board in the middle, the
    stretch's title above it in teal, the take's caption under it."""
    bg = Image.open(f"{out}/frame-v.png").convert("RGBA").resize((VW, VH), Image.LANCZOS)
    # The camera's view, cropped square: what it frames is at its centre.
    bw = bh = H
    top = (VH - bh) // 2
    kicker = v3.font("segoeuib.ttf", 46)
    cache, last = {}, None
    shorts = stretch_samples(tl, marked(tl, "short"), SHORT_SPEED)

    def one(s, title, caption):
        f = bg.copy()
        # Square, around what the camera aims at (it stops at the page's edges,
        # so that isn't always the frame's centre).
        x, y, z = tl.camera_at(s)
        sw = W / z
        left = min(max(x * tl.scale - sw * tl.scale / 2, 0), W * tl.scale - sw * tl.scale) / tl.scale
        aim = (x - left) * z * W / 1280
        x0 = int(min(max(aim - H / 2, 0), W - H))
        board = render(tl, out, s, cache, chapters=False, captions=False).crop((x0, 0, x0 + H, H))
        f.alpha_composite(board, (0, top))
        if title:
            d = ImageDraw.Draw(f)
            d.text(((VW - kicker.getlength(title)) / 2, top - 90), title, font=kicker, fill=TEAL)
        if caption:
            f.alpha_composite(vertical_caption(caption), (0, top + bh + 40))
        return f

    for s, _, first in stretch_samples(tl, marked(tl, "hook"), HOOK_SPEED):
        last = one(s, "", HOOK_CAPTION)
        yield last
    yield from card_frames(f"{out}/tagline-v.png", (VW, VH), 2.6, fade_from=last)
    prev = None
    for s, title, first in shorts:
        cap = tl.latest("caption", s)
        f = one(s, title, cap["text"] if cap else "")
        if first and prev is not None:
            for k in range(4):
                yield Image.blend(prev, f, (k + 1) / 5)
        prev = last = f
        yield f
    yield from fade_out(card_frames(f"{out}/end-v.png", (VW, VH), 4.0, fade_from=last), 1.0)


# ---- Output -------------------------------------------------------------------------------
def encode(frames, size, wav, video, seconds):
    if wav == "-":
        return seconds
    ff = imageio_ffmpeg.get_ffmpeg_exe()
    proc = subprocess.Popen(
        [ff, "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{size[0]}x{size[1]}",
         "-r", str(FPS), "-i", "-", "-i", wav, "-map", "0:v", "-map", "1:a",
         "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
         "-c:a", "aac", "-b:a", "192k", "-af", f"loudnorm=I=-16:TP=-1.5:LRA=11,afade=t=out:st={seconds - 2.5:.2f}:d=2.5", "-ar", "44100", "-shortest", "-movflags", "+faststart", video],
        stdin=subprocess.PIPE)
    n = 0
    for f in frames:
        proc.stdin.write(f.convert("RGB").tobytes())
        n += 1
        if n % 300 == 0:
            print(f"  {n / FPS:5.1f} s", flush=True)
    proc.stdin.close()
    proc.wait()
    return n / FPS


def length(tl, out, vertical_cut):
    """Seconds of the cut, without rendering: for the soundtrack."""
    hook = len(stretch_samples(tl, marked(tl, "hook"), HOOK_SPEED))
    if vertical_cut:
        shorts = stretch_samples(tl, marked(tl, "short"), SHORT_SPEED)
        return (hook + len(shorts) + 4 * (sum(1 for x in shorts if x[2]) - 1)) / FPS + 2.6 + 4.0
    hooks = marked(tl, "hook")
    return (hook + 4 * (len(hooks) - 1) + len(tl.samples())) / FPS + TAGLINE + END_CARD


def main():
    out, wav, video = sys.argv[1:4]
    vertical_cut = "--vertical" in sys.argv
    tl = timeline(out)
    if "--stills" in sys.argv:
        frames = vertical(tl, out) if vertical_cut else landscape(tl, out)
        want = sorted(int(float(a) * FPS) for a in sys.argv[sys.argv.index("--stills") + 1:])
        for i, f in enumerate(frames):
            if i in want:
                f.convert("RGB").save(f"{out}/still{'-v' if vertical_cut else ''}_{i / FPS:05.1f}.png")
            if i >= want[-1]:
                break
        return
    total = length(tl, out, vertical_cut)
    print(f"total {total:.1f} s")
    if wav == "-":
        return
    size = (VW, VH) if vertical_cut else (W, H)
    encode(vertical(tl, out) if vertical_cut else landscape(tl, out), size, wav, video, total)


if __name__ == "__main__":
    main()
