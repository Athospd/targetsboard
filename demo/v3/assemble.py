"""Assembles take three of the demo from what record.R left in <out>: the frames
(2560 x 1440), their times, and the events. In post, for each output frame:
the source frame (with speed ramps and cuts), the cursor and click ripples, the
camera (eased zooms), then the caption, chapter cards and the code card.
The end card follows with a slow push-in, and the soundtrack goes under it.

    python demo/v3/assemble.py <out> <end.png> <soundtrack.wav> <video>
    python demo/v3/assemble.py <out> <end.png> - - --stills 5 20 40   # check frames
"""
import bisect
import csv
import json
import math
import subprocess
import sys

import imageio_ffmpeg
from PIL import Image, ImageDraw, ImageFont

FPS = 30
W, H = 1920, 1080
FONTS = "C:/Windows/Fonts/"
INK = (47, 34, 61)
CREAM = (247, 239, 228)
END_CARD = 6.0
DISSOLVE = 0.3  # seconds of crossfade over a cut


def font(name, size):
    return ImageFont.truetype(FONTS + name, size)


def ease(f):  # ease in-out cubic
    f = min(max(f, 0.0), 1.0)
    return 4 * f ** 3 if f < 0.5 else 1 - (-2 * f + 2) ** 3 / 2


def back_out(f):  # a little overshoot, for pops
    f = min(max(f, 0.0), 1.0)
    c = 1.6
    return 1 + (c + 1) * (f - 1) ** 3 + c * (f - 1) ** 2


class Timeline:
    def __init__(self, out):
        rows = [(int(r["n"]), float(r["t"])) for r in csv.DictReader(open(f"{out}/frames.csv", encoding="utf-8"))]
        self.frame_n = [n for n, _ in rows]
        self.frame_t = [t for _, t in rows]
        data = json.load(open(f"{out}/events.json", encoding="utf-8"))
        self.scale = data["scale"]
        self.events = sorted(data["events"], key=lambda e: e["t"])
        self.cursor = sorted((c[0], c[1], c[2]) for c in data["cursor"])
        self.cursor_t = [c[0] for c in self.cursor]
        # A still page sends no frames, so the take ends at the "end" event,
        # not at the last frame.
        ends = [e["t"] for e in self.events if e["type"] == "end"]
        self.t0, self.t1 = self.frame_t[0], max([self.frame_t[-1] + 0.3] + ends)
        self.of = lambda kind: [e for e in self.events if e["type"] == kind]

    def samples(self):
        """Source times of the output frames, with the speed ramps applied and
        the skipped stretches cut out. Sets self.cuts: output frame index ->
        the source time just before the cut, to dissolve from."""
        ramps = [(e["t"], e["factor"]) for e in self.of("speed")]
        skips, start = [], None
        for e in self.of("skip"):
            if e["on"]: start = e["t"]
            elif start is not None: skips.append((start, e["t"])); start = None
        s, out, self.cuts = self.t0, [], {}
        while s <= self.t1:
            for a, b in skips:
                if a <= s < b:
                    self.cuts[len(out)] = out[-1] if out else a
                    s = b
            out.append(s)
            factor = 1.0
            for t, f in ramps:
                if t <= s: factor = f
            s += factor / FPS
        return out

    def frame_at(self, s):
        i = max(0, bisect.bisect_right(self.frame_t, s) - 1)
        return self.frame_n[i]

    def cursor_at(self, s):
        i = bisect.bisect_right(self.cursor_t, s)
        if i == 0: return self.cursor[0][1:]
        if i >= len(self.cursor): return self.cursor[-1][1:]
        (ta, xa, ya), (tb, xb, yb) = self.cursor[i - 1], self.cursor[i]
        if tb - ta > 0.12: return (xa, ya)  # a pause: stay put
        f = (s - ta) / (tb - ta) if tb > ta else 1
        return (xa + (xb - xa) * f, ya + (yb - ya) * f)

    def camera_at(self, s):
        x, y, z = 640.0, 360.0, 1.0
        for e in self.of("camera"):
            if e["t"] > s: break
            f = ease((s - e["t"]) / max(e["dur"], 1e-3))
            x, y, z = x + (e["x"] - x) * f, y + (e["y"] - y) * f, z + (e["zoom"] - z) * f
        return x, y, z

    def latest(self, kind, s):
        found = None
        for e in self.of(kind):
            if e["t"] <= s: found = e
        return found


# ---- Overlays -------------------------------------------------------------------------
CURSOR = [(3, 2), (3, 19), (7.5, 14.8), (10.6, 21.5), (13.4, 20.3), (10.3, 13.7), (16.5, 13.7)]


def draw_cursor(img, x, y, scale):
    d = ImageDraw.Draw(img)
    k = 1.15 * scale  # the arrow's 24-unit box is ~28 css px
    pts = [(x + (px - 3) * k, y + (py - 2) * k) for px, py in CURSOR]
    shadow = [(px + 2 * scale, py + 3 * scale) for px, py in pts]
    d.polygon(shadow, fill=(0, 0, 0, 70))
    d.polygon(pts, fill=(255, 255, 255), outline=(29, 29, 29), width=max(2, int(1.6 * scale)))


def draw_ripples(img, tl, s):
    d = ImageDraw.Draw(img)
    for e in tl.of("click"):
        age = s - e["t"]
        if 0 <= age < 0.55:
            f = age / 0.55
            r = (8 + 26 * f) * tl.scale
            a = int(255 * (1 - f))
            x, y = e["x"] * tl.scale, e["y"] * tl.scale
            d.ellipse([x - r, y - r, x + r, y + r], outline=(212, 149, 54, a), width=int(4 * tl.scale))


_caption_cache = {}


def caption_image(text, kicker_color):
    key = (text, kicker_color)
    if key in _caption_cache: return _caption_cache[key]
    f = font("segoeuib.ttf", 46)
    pad_x, pad_y, bar = 40, 22, 10
    box = f.getbbox(text)
    w, h = box[2] - box[0] + 2 * pad_x + bar, 46 + 2 * pad_y + 8
    img = Image.new("RGBA", (w + 40, h + 40), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    d.rounded_rectangle([24, 26, 24 + w, 26 + h], 22, fill=(0, 0, 0, 60))  # shadow
    d.rounded_rectangle([20, 20, 20 + w, 20 + h], 22, fill=INK + (242,))
    d.rounded_rectangle([20, 20, 20 + bar + 14, 20 + h], 22, fill=kicker_color + (255,))
    d.rectangle([20 + bar, 20, 20 + bar + 16, 20 + h], fill=INK + (242,))
    d.text((20 + bar + pad_x - box[0], 20 + pad_y - box[1] + 4), text, font=f, fill=CREAM)
    _caption_cache[key] = img
    return img


def draw_caption(frame, tl, s, color):
    e = tl.latest("caption", s)
    if not e or not e["text"]:
        # fade the previous one out
        prev = [c for c in tl.of("caption") if c["t"] <= s and c["text"]]
        if not prev or not e: return
        age = s - e["t"]
        if age > 0.2: return
        e, alpha, k = prev[-1], 1 - age / 0.2, 1.0
    else:
        age = s - e["t"]
        alpha, k = min(1, age / 0.18), 0.86 + 0.14 * back_out(age / 0.35)
    img = caption_image(e["text"], color)
    if k != 1:
        img = img.resize((max(1, int(img.width * k)), max(1, int(img.height * k))), Image.BICUBIC)
    if alpha < 1:
        img = img.copy(); img.putalpha(img.getchannel("A").point(lambda a: int(a * alpha)))
    frame.alpha_composite(img, (W // 2 - img.width // 2, H - 64 - img.height + int(20 * (1 - min(1, age / 0.3)))))


def chapter_overlay(frame, e, s):
    age = s - e["t"]
    if not 0 <= age < 1.25: return
    color = tuple(int(e["color"][i:i + 2], 16) for i in (1, 3, 5))
    light = sum(color) / 3 > 140
    txt = INK if light else CREAM
    # A panel wipes in from the left, holds, and wipes out to the right.
    if age < 0.32: x0, x1 = 0, W * ease(age / 0.32)
    elif age < 0.93: x0, x1 = 0, W
    else: x0, x1 = W * ease((age - 0.93) / 0.32), W
    over = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(over)
    d.rectangle([x0, 0, x1, H], fill=color + (255,))
    stripe = INK if not light else (255, 255, 255)
    d.rectangle([x0, H * 0.62, x1, H * 0.62 + 10], fill=stripe + (90,))
    f = font("seguibl.ttf", 150)
    box = f.getbbox(e["title"])
    tw = box[2] - box[0]
    slide = 60 * (1 - back_out(min(1, (age - 0.12) / 0.45))) if age < 0.93 else -80 * ease((age - 0.93) / 0.32)
    tx = W / 2 - tw / 2 + slide
    text_layer = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    ImageDraw.Draw(text_layer).text((tx - box[0], H / 2 - 110), e["title"], font=f, fill=txt + (255,))
    mask = Image.new("L", (W, H), 0)
    ImageDraw.Draw(mask).rectangle([x0, 0, x1, H], fill=255)
    over.paste(text_layer, (0, 0), Image.composite(text_layer.getchannel("A"), Image.new("L", (W, H), 0), mask))
    frame.alpha_composite(over)


def changed_span(old, new):
    """The characters that differ between two lines: (start, end) in each."""
    i = 0
    while i < min(len(old), len(new)) and old[i] == new[i]: i += 1
    j = 0
    while j < min(len(old), len(new)) - i and old[-1 - j] == new[-1 - j]: j += 1
    return i, len(old) - j, len(new) - j


_code_cache = {}


def code_card(show, saved):
    key = (show["old"], show["new"], saved)
    if key in _code_cache: return _code_cache[key]
    f, small = font("consola.ttf", 36), font("segoeuib.ttf", 26)
    cw = f.getlength("x")
    i, oe, ne = changed_span(show["old"], show["new"])
    lines = [(" ", "  " + show.get("before", ""), None),
             ("-", "- " + show["old"], (i + 2, oe + 2)),
             ("+", "+ " + show["new"], (i + 2, ne + 2)),
             (" ", "  " + show.get("after", ""), None)]
    row = 62
    w = int(max(f.getlength(t) for _, t, _ in lines)) + 96
    h = 84 + row * len(lines) + 26
    img = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    # Solid: everything is drawn on an opaque card, and only the whole card fades.
    card = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    d = ImageDraw.Draw(card)
    d.rounded_rectangle([0, 0, w - 1, h - 1], 22, fill=(31, 26, 41, 255), outline=(70, 60, 88, 255), width=2)
    d.text((40, 26), show.get("file", "_targets.R"), font=small, fill=(190, 180, 205))
    if saved:
        chip = "✓ saved"
        cwid = small.getlength(chip) + 32
        d.rounded_rectangle([w - 40 - cwid, 20, w - 40, 62], 21, fill=(53, 110, 60, 255))
        d.text((w - 40 - cwid + 16, 26), chip, font=small, fill=(225, 250, 215))
    for k, (kind, t, span) in enumerate(lines):
        y = 84 + k * row
        if kind == "-":
            d.rectangle([24, y - 8, w - 24, y + 50], fill=(92, 36, 34, 255))
            fg, hi = (255, 196, 184), (196, 64, 44, 255)
        elif kind == "+":
            d.rectangle([24, y - 8, w - 24, y + 50], fill=(38, 74, 42, 255))
            fg, hi = (214, 246, 190), (70, 150, 66, 255)
        else:
            fg, hi = (200, 192, 214), None
        if span and hi:
            x0 = 40 + span[0] * cw
            d.rounded_rectangle([x0 - 2, y - 4, x0 + (span[1] - span[0]) * cw + 2, y + 46], 6, fill=hi)
        d.text((40, y), t, font=f, fill=fg)
    img.alpha_composite(card)
    _code_cache[key] = img
    return img


def code_overlay(frame, tl, s):
    on = None
    for e in tl.of("code"):
        if e["t"] <= s: on = e
    if not on: return frame
    if on["on"]:
        age, show = s - on["t"], on
    else:
        age = s - on["t"]
        if age > 0.3: return frame
        prev = [c for c in tl.of("code") if c["on"] and c["t"] <= on["t"]]
        show = prev[-1]; age = 0.3 - age
    a = min(1, age / 0.3)
    saved = any(e["t"] <= s and e["t"] >= show["t"] for e in tl.of("code_saved"))
    # Dim the board behind it, so the card reads on its own.
    frame = Image.blend(frame, Image.new("RGBA", (W, H), (18, 14, 24, 255)), 0.55 * a)
    img = code_card(show, saved)
    k = 0.92 + 0.08 * back_out(a)
    img = img.resize((int(img.width * k), int(img.height * k)), Image.BICUBIC)
    if a < 1:
        img = img.copy(); img.putalpha(img.getchannel("A").point(lambda v: int(v * a)))
    frame.alpha_composite(img, (W // 2 - img.width // 2, int(H * 0.42) - img.height // 2))
    return frame


def chapter_color(tl, s):
    e = tl.latest("chapter", s)
    return tuple(int(e["color"][i:i + 2], 16) for i in (1, 3, 5)) if e else (212, 149, 54)


# ---- Rendering --------------------------------------------------------------------------
def render(tl, out, s, cache):
    n = tl.frame_at(s)
    if cache.get("n") != n:
        cache["n"], cache["img"] = n, Image.open(f"{out}/frames/{n:06d}.jpg").convert("RGBA")
    img = cache["img"].copy()
    draw_ripples(img, tl, s)
    cx, cy = tl.cursor_at(s)
    draw_cursor(img, cx * tl.scale, cy * tl.scale, tl.scale)
    x, y, z = tl.camera_at(s)
    sw, sh = img.width / z, img.height / z
    left = min(max(x * tl.scale - sw / 2, 0), img.width - sw)
    top = min(max(y * tl.scale - sh / 2, 0), img.height - sh)
    frame = img.resize((W, H), Image.BICUBIC, box=(left, top, left + sw, top + sh))
    frame = code_overlay(frame, tl, s)
    draw_caption(frame, tl, s, chapter_color(tl, s))
    for e in tl.of("chapter"):
        chapter_overlay(frame, e, s)
    if s - tl.t0 < 0.25:  # a quick fade in
        frame = Image.blend(Image.new("RGBA", (W, H), (0, 0, 0, 255)), frame, (s - tl.t0) / 0.25)
    return frame


def end_frames(end_png, last):
    card = Image.open(end_png).convert("RGBA").resize((W, H), Image.LANCZOS)
    n = int(END_CARD * FPS)
    for i in range(n):
        t = i / FPS
        z = 1 + 0.05 * (t / END_CARD)
        sw, sh = W / z, H / z
        f = card.resize((W, H), Image.BICUBIC, box=((W - sw) / 2, (H - sh) / 2, (W + sw) / 2, (H + sh) / 2))
        if t < 0.45:
            f = Image.blend(last, f, ease(t / 0.45))
        if t > END_CARD - 1.2:
            f = Image.blend(f, Image.new("RGBA", (W, H), (0, 0, 0, 255)), ease((t - (END_CARD - 1.2)) / 1.2))
        yield f


def main():
    out, end_png, wav, video = sys.argv[1:5]
    tl = Timeline(out)
    samples = tl.samples()
    if "--stills" in sys.argv:
        for at in map(float, sys.argv[sys.argv.index("--stills") + 1:]):
            s = samples[min(len(samples) - 1, int(at * FPS))]
            render(tl, out, s, {}).convert("RGB").save(f"{out}/still_{at:05.1f}.png")
        return
    total = len(samples) / FPS + END_CARD
    print(f"board {len(samples) / FPS:.1f} s, total {total:.1f} s, {len(tl.cuts)} cuts")
    if wav == "-": return
    ff = imageio_ffmpeg.get_ffmpeg_exe()
    proc = subprocess.Popen(
        [ff, "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS),
         "-i", "-", "-i", wav, "-map", "0:v", "-map", "1:a",
         "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
         "-c:a", "aac", "-b:a", "192k", "-af", f"afade=t=out:st={total - 2.5:.2f}:d=2.5",
         "-shortest", "-movflags", "+faststart", video],
        stdin=subprocess.PIPE)
    cache, last, cut_from = {}, None, None
    for i, s in enumerate(samples):
        last = render(tl, out, s, cache)
        if i in tl.cuts: cut_from = (i, render(tl, out, tl.cuts[i], {}))
        if cut_from and i - cut_from[0] < DISSOLVE * FPS:
            last = Image.blend(cut_from[1], last, ease((i - cut_from[0] + 1) / (DISSOLVE * FPS)))
        proc.stdin.write(last.convert("RGB").tobytes())
        if i % 300 == 0: print(f"  {i / FPS:5.1f} s", flush=True)
    for f in end_frames(end_png, last):
        proc.stdin.write(f.convert("RGB").tobytes())
    proc.stdin.close()
    proc.wait()


if __name__ == "__main__":
    main()
