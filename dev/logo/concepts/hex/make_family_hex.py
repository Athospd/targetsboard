"""targetsboard hex in targetfy's style: two versions, family (teal, navy)
and mono (one ink). python dev/logo/concepts/hex/make_family_hex.py dev/logo/concepts/hex"""
import math, pathlib, sys
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont
from fontTools.varLib import instancer

OUT = pathlib.Path(sys.argv[1])
W, H = 518, 600
CX, CY = W / 2, H / 2


def hexagon(r):
    return " ".join(f"{CX + r * math.cos(math.radians(-90 + 60 * k)):.2f},{CY + r * math.sin(math.radians(-90 + 60 * k)):.2f}" for k in range(6))


def text_path(text, size, baseline, weight=600, tracking=0.02):
    font = TTFont("C:/Windows/Fonts/bahnschrift.ttf")
    font = instancer.instantiateVariableFont(font, {"wght": weight})
    upem = font["head"].unitsPerEm
    scale = size / upem
    cmap, glyphs, hmtx = font.getBestCmap(), font.getGlyphSet(), font["hmtx"]
    names = [cmap[ord(c)] for c in text]
    advance = sum(hmtx[n][0] for n in names) + tracking * upem * (len(names) - 1)
    x = CX / scale - advance / 2
    parts = []
    for n in names:
        pen = SVGPathPen(glyphs)
        glyphs[n].draw(TransformPen(pen, (scale, 0, 0, -scale, x * scale, baseline)))
        parts.append(pen.getCommands())
        x += hmtx[n][0] + tracking * upem
    return " ".join(parts)


NAME = text_path("targetsboard", 66, 414)


def sticker(top, bottom, rim, ink, accent, glow):
    # The Pulse mark on the 64 grid, scaled to ~330 px wide, centred on y = 250.
    s = 5.3
    tx, ty = CX - 31.5 * s, 250 - 32 * s
    return f"""<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="0.35" y2="1">
      <stop offset="0" stop-color="{top}"/><stop offset="1" stop-color="{bottom}"/>
    </linearGradient>
    <radialGradient id="glow" cx="0.5" cy="0.4" r="0.55">
      <stop offset="0" stop-color="{glow}" stop-opacity="0.16"/><stop offset="1" stop-color="{glow}" stop-opacity="0"/>
    </radialGradient>
  </defs>
  <polygon points="{hexagon(282)}" fill="url(#bg)" stroke="{rim}" stroke-width="16" stroke-linejoin="round"/>
  <polygon points="{hexagon(262)}" fill="url(#glow)"/>
  <polygon points="{hexagon(262)}" fill="none" stroke="#FFFFFF" stroke-opacity="0.09" stroke-width="2" stroke-linejoin="round"/>
  <g transform="translate({tx:.1f} {ty:.1f}) scale({s})" fill="none" stroke-linecap="round">
    <path d="M3 32h11l6-14 9 28 6-14h5" stroke="{ink}" stroke-width="3.2" stroke-linejoin="round"/>
    <circle cx="50" cy="32" r="10" fill="{accent}" fill-opacity="0.0" stroke="{ink}" stroke-width="3.2"/>
    <circle cx="50" cy="32" r="4.2" fill="{accent}" stroke="none"/>
  </g>
  <path d="{NAME}" fill="{ink}"/>
</svg>
"""


OUT.mkdir(parents=True, exist_ok=True)
(OUT / "hex-family.svg").write_text(sticker("#26354F", "#141C2B", "#3CC8B4", "#EEF3F9", "#3CC8B4", "#3CC8B4"), encoding="utf-8")
(OUT / "hex-mono.svg").write_text(sticker("#2b3138", "#15181c", "#59626d", "#EEF3F9", "#EEF3F9", "#FFFFFF"), encoding="utf-8")
print("ok")
