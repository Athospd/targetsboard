# targetsboard's brand: the Pulse mark (a live signal reaching a target),
# in one colour, and everything made from it. To change the mark, change
# PULSE below and run both scripts again. The other concepts are in
# dev/logo/concepts, the hex sticker before this one in dev/logo/old-hex.
#
# From the package folder:
#   python dev/logo/make_logo.py
#   Rscript dev/logo/render_logo.R
#
# Writes:
#   inst/app/www/mark.svg     the mark in currentColor, inlined in the top
#                             bar, so it takes the theme's text colour
#   inst/app/www/favicon.svg  the browser tab's icon: the mark on an ink tile
#   inst/app/www/pulse-spinner.svg
#                             the loading spinner: the mark, animated (the
#                             line draws, the target pulses); index.css
#                             uses it as a mask, painted in the theme's
#                             primary colour
#   man/figures/logo.svg      the hex sticker (README, pkgdown), in the style
#                             of targetfy's, its sibling package
#   dev/logo/mark-ink.svg     the mark in ink, for documents
#   dev/logo/mark-white.svg   the mark in white, for dark backgrounds
#   inst/app/www/wordmark.svg the mark and the name, the top bar's title, in
#                             currentColor; dev/logo/wordmark-ink.svg and
#                             wordmark-white.svg for documents
# render_logo.R then renders the PNGs (man/figures/logo.png,
# inst/app/www/favicon.png, dev/logo/mark-512.png).
import math
import re
import os

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

INK = "#1d2126"      # the brand's one colour
PAPER = "#ffffff"    # what goes on it

# The mark, on a 64 x 64 grid: a heartbeat line, then a bullseye.
PULSE = (
    '<path d="M3 32h11l6-14 9 28 6-14h5" stroke-width="4" stroke-linejoin="round"/>'
    '<circle cx="50" cy="32" r="10" stroke-width="4"/>'
    '<circle cx="50" cy="32" r="3.5" fill="{c}" stroke="none"/>'
)


def mark(colour, size=None, extra=""):
    dims = f' width="{size}" height="{size}"' if size else ""
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64"{dims} fill="none" '
            f'stroke="{colour}" stroke-linecap="round"{extra}>{PULSE.format(c=colour)}</svg>\n')


def write(rel, text):
    path = os.path.join(ROOT, rel)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="\n") as f:
        f.write(text)
    print("wrote", rel)


# The mark, in the theme's text colour: hidden from screen readers, as the
# title next to it names the app.
write("inst/app/www/mark.svg", mark("currentColor", extra=' class="tb-brand-mark" aria-hidden="true" focusable="false"'))
write("dev/logo/mark-ink.svg", mark(INK, 256))
write("dev/logo/mark-white.svg", mark(PAPER, 256))

# The favicon: the mark on a rounded ink tile, readable on light and dark tabs.
write("inst/app/www/favicon.svg",
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">'
      f'<rect width="64" height="64" rx="14" fill="{INK}"/>'
      f'<g transform="translate(6 6) scale(0.8125)" fill="none" stroke="{PAPER}" stroke-linecap="round">'
      f'{PULSE.format(c=PAPER)}</g></svg>\n')

# The loading spinner: the mark, animated in 1.6 s. The line draws itself,
# the target pulses as it lands, the line wipes away. Black: index.css uses
# it as a mask, so only its shape and opacity count. Slower where the
# reader asks for less motion.
write("inst/app/www/pulse-spinner.svg",
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" fill="none" stroke="#000" stroke-linecap="round">'
      '<style>'
      '.line{stroke-dasharray:100 100;animation:draw 1.6s cubic-bezier(.5,0,.3,1) infinite}'
      '.ring,.dot{transform-origin:50px 32px}'
      '.ring{animation:ring 1.6s ease-out infinite}'
      '.dot{animation:dot 1.6s ease-out infinite}'
      '@keyframes draw{0%{stroke-dashoffset:100}55%{stroke-dashoffset:0}85%,100%{stroke-dashoffset:-100}}'
      '@keyframes ring{0%,50%{transform:scale(1);opacity:.35}62%{transform:scale(1.18);opacity:1}85%,100%{transform:scale(1);opacity:.35}}'
      '@keyframes dot{0%,52%{transform:scale(.4)}64%{transform:scale(1.3)}85%,100%{transform:scale(.4)}}'
      '@media (prefers-reduced-motion:reduce){.line,.ring,.dot{animation-duration:4.8s}}'
      '</style>'
      '<path class="line" pathLength="100" d="M3 32h11l6-14 9 28 6-14h5" stroke-width="4.5" stroke-linejoin="round"/>'
      '<circle class="ring" cx="50" cy="32" r="10" stroke-width="4.5"/>'
      '<circle class="dot" cx="50" cy="32" r="4" fill="#000" stroke="none"/>'
      '</svg>\n')

# The hex sticker, in targetfy's family style (its sibling package): a navy
# gradient, a teal rim, a soft glow behind the mark, a faint inner line, and
# the name as outlines in Bahnschrift SemiBold, so it needs no font to show.
# Pointy-top, 518 x 600 px, the size of the stickers in
# github.com/rstudio/hex-stickers (1.73" x 2"). Needs fontTools and the
# Bahnschrift font (it ships with Windows 10 and later).
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont
from fontTools.varLib import instancer

NAVY_TOP, NAVY_BOTTOM = "#26354F", "#141C2B"   # targetfy's
TEAL = "#3CC8B4"
LIGHT = "#EEF3F9"
W, H = 518, 600
CX, CY = W / 2, H / 2


def hexagon(r):
    return " ".join(f"{CX + r * math.cos(math.radians(-90 + 60 * k)):.2f},"
                    f"{CY + r * math.sin(math.radians(-90 + 60 * k)):.2f}" for k in range(6))


def text_path(text, size, baseline, weight=600, tracking=0.02):
    """The text as one SVG path, centred. tracking is in em."""
    font = instancer.instantiateVariableFont(TTFont("C:/Windows/Fonts/bahnschrift.ttf"), {"wght": weight})
    upem = font["head"].unitsPerEm
    scale = size / upem
    cmap, glyphs, hmtx = font.getBestCmap(), font.getGlyphSet(), font["hmtx"]
    names = [cmap[ord(c)] for c in text]
    advance = sum(hmtx[n][0] for n in names) + tracking * upem * (len(names) - 1)
    x = CX / scale - advance / 2
    parts = []
    for n in names:
        pen = SVGPathPen(glyphs)
        # Font units are y-up: flip, then place on the baseline.
        glyphs[n].draw(TransformPen(pen, (scale, 0, 0, -scale, x * scale, baseline)))
        parts.append(pen.getCommands())
        x += hmtx[n][0] + tracking * upem
    return " ".join(parts)


# The mark on the 64 grid, about 330 px wide, centred on y = 250; its line a
# little thinner at this size, and the target's dot in teal.
s = 5.3
tx, ty = CX - 31.5 * s, 250 - 32 * s
write("man/figures/logo.svg", f"""<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="0.35" y2="1">
      <stop offset="0" stop-color="{NAVY_TOP}"/><stop offset="1" stop-color="{NAVY_BOTTOM}"/>
    </linearGradient>
    <radialGradient id="glow" cx="0.5" cy="0.4" r="0.55">
      <stop offset="0" stop-color="{TEAL}" stop-opacity="0.16"/><stop offset="1" stop-color="{TEAL}" stop-opacity="0"/>
    </radialGradient>
  </defs>
  <polygon points="{hexagon(282)}" fill="url(#bg)" stroke="{TEAL}" stroke-width="16" stroke-linejoin="round"/>
  <polygon points="{hexagon(262)}" fill="url(#glow)"/>
  <polygon points="{hexagon(262)}" fill="none" stroke="#FFFFFF" stroke-opacity="0.09" stroke-width="2" stroke-linejoin="round"/>
  <g transform="translate({tx:.1f} {ty:.1f}) scale({s})" fill="none" stroke="{LIGHT}" stroke-linecap="round">
    <path d="M3 32h11l6-14 9 28 6-14h5" stroke-width="3.2" stroke-linejoin="round"/>
    <circle cx="50" cy="32" r="10" stroke-width="3.2"/>
    <circle cx="50" cy="32" r="4.2" fill="{TEAL}" stroke="none"/>
  </g>
  <path d="{text_path("targetsboard", 66, 414)}" fill="{LIGHT}"/>
</svg>
""")


# The wordmark: the mark, then the name, "targets" regular and "board" bold,
# the two words the name joins, without a space. Text as outlines,
# Bahnschrift, kerned with the font's own pairs (GPOS). The mark's ink is
# 1.2 times the letters' ascender, optically their weight beside them, and
# centred between baseline and ascender; the gap after it is 0.38 of the
# ascender. In currentColor for the top bar, in ink
# and in white for documents.
def kerning(font):
    """The font's kerning, from its GPOS 'kern' feature: f(left, right)."""
    gpos = font["GPOS"].table
    lookups = {i for fr in gpos.FeatureList.FeatureRecord if fr.FeatureTag == "kern" for i in fr.Feature.LookupListIndex}
    pairs, classes = {}, []
    for i in sorted(lookups):
        lookup = gpos.LookupList.Lookup[i]
        subs = [s.ExtSubTable for s in lookup.SubTable] if lookup.LookupType == 9 else lookup.SubTable
        for st in subs:
            if st.Format == 1:
                for g, ps in zip(st.Coverage.glyphs, st.PairSet):
                    for rec in ps.PairValueRecord:
                        pairs.setdefault((g, rec.SecondGlyph), getattr(rec.Value1, "XAdvance", 0) or 0)
            elif st.Format == 2:
                classes.append(st)

    def value(a, b):
        if (a, b) in pairs:
            return pairs[(a, b)]
        for st in classes:
            if a in st.Coverage.glyphs:
                rec = st.Class1Record[st.ClassDef1.classDefs.get(a, 0)].Class2Record[st.ClassDef2.classDefs.get(b, 0)]
                v = getattr(rec.Value1, "XAdvance", 0) or 0
                if v:
                    return v
        return 0
    return value


def rounded(d):
    """Path data to 2 decimals: a hundredth of a pixel at 40 px."""
    return re.sub(r"-?\d+\.\d+", lambda m: f"{float(m.group()):.2f}".rstrip("0").rstrip("."), d)


def wordmark(colour, size=40, gap=0.38, grow=1.2):
    from fontTools.pens.boundsPen import BoundsPen
    fonts = {w: instancer.instantiateVariableFont(TTFont("C:/Windows/Fonts/bahnschrift.ttf"), {"wght": w}) for w in (400, 700)}
    regular = fonts[400]
    upem = regular["head"].unitsPerEm
    sc = size / upem
    gs = regular.getGlyphSet()

    def top(ch):
        b = BoundsPen(gs)
        gs[regular.getBestCmap()[ord(ch)]].draw(b)
        return b.bounds[3] * sc
    asc, xh = top("b"), top("x")
    baseline = 60
    s = grow * asc / 32                           # the mark's ink, 32 units tall
    mx, my = -1 * s, baseline - asc / 2 - 32 * s  # ink from x = 1
    x = (62 * s + mx + gap * asc) / sc            # in font units, after the gap
    paths, prev = [], None
    for text, weight in (("targets", 400), ("board", 700)):
        f = fonts[weight]
        cmap, glyphs, hmtx, kern = f.getBestCmap(), f.getGlyphSet(), f["hmtx"], kerning(f)
        for ch in text:
            g = cmap[ord(ch)]
            if prev:
                x += kern(prev, g)
            pen = SVGPathPen(glyphs)
            glyphs[g].draw(TransformPen(pen, (sc, 0, 0, -sc, x * sc, baseline)))
            paths.append(pen.getCommands())
            x += hmtx[g][0]
            prev = g
    width = x * sc + 1
    top_y = min(baseline - asc, my + 16 * s) - 1.5
    bottom_y = max(baseline + size * 0.2, my + 48 * s) + 1
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 {top_y:.2f} {width:.2f} {bottom_y - top_y:.2f}" '
            f'fill="{colour}" role="img" aria-label="targetsboard"{{extra}}>'
            f'<g transform="translate({mx:.2f} {my:.2f}) scale({s:.4f})" fill="none" stroke="{colour}" stroke-linecap="round">'
            f'{PULSE.format(c=colour)}</g><path d="{rounded(" ".join(paths))}"/></svg>\n')


write("inst/app/www/wordmark.svg", wordmark("currentColor").replace("{extra}", ' class="tb-wordmark" focusable="false"'))
write("dev/logo/wordmark-ink.svg", wordmark(INK).replace("{extra}", ""))
write("dev/logo/wordmark-white.svg", wordmark(PAPER).replace("{extra}", ""))
