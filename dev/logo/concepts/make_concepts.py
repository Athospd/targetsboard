"""25 monochrome logo concepts for targetsboard: a sheet and one SVG each.

python dev/logo/concepts/make_concepts.py dev/logo/concepts
Each mark is drawn on a 64 x 64 grid in currentColor, so it takes the
theme's text colour.
"""
import math, os, sys

out = sys.argv[1]
os.makedirs(out, exist_ok=True)

F = 'fill="currentColor" stroke="none"'


def dot(x, y, r):
    return f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{r}" {F}/>'


def ring(x, y, r, w=4):
    return f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{r}" stroke-width="{w}"/>'


def path(d, w=4, extra=""):
    return f'<path d="{d}" stroke-width="{w}" {extra}/>'


def pol(cx, cy, r, deg):  # math angle, y up
    a = math.radians(deg)
    return cx + r * math.cos(a), cy - r * math.sin(a)


def bullseye(x, y, r=12, w=4, d=4.5):
    return ring(x, y, r, w) + dot(x, y, d)


marks = []


def mark(name, idea, body):
    marks.append((name, idea, body))


# 1 Reticle: an analog sight; two targets converge on the centre.
mark("Reticle", "a sight: targets converge on its centre",
     '<circle cx="32" cy="32" r="26" stroke-width="4" stroke-dasharray="0 4 17 4" pathLength="100" stroke-linecap="butt"/>'
     + path("M32 2v10M32 52v10M2 32h10M52 32h10")
     + path("M18 21c8 0 8 11 14 11M18 43c8 0 8-11 14-11", 3.5)
     + dot(17, 21, 4) + dot(17, 43, 4) + dot(33, 32, 6))

# 2 Converge: three of the board's nodes flow into a bullseye.
mark("Converge", "the pipeline flows into its goal",
     ''.join(f'<rect x="3" y="{y}" width="17" height="9" rx="4.5" {F}/>' for y in (11, 27.5, 44))
     + path("M20 15.5c10 0 10 16.5 18 16.5M20 32h18M20 48.5c10 0 10-16.5 18-16.5", 3.5)
     + bullseye(48, 32, 12.5))

# 3 Orbit: targets on a ring, all feeding its centre.
pts = [pol(32, 32, 25, a) for a in (180, 135, 225)]
mark("Orbit", "upstream targets around the one they feed",
     ring(32, 32, 25, 3.5)
     + ''.join(path(f"M{x:.2f} {y:.2f}L32 32", 3) for x, y in pts)
     + ''.join(dot(x, y, 5) for x, y in pts) + dot(32, 32, 7))

# 4 t-graph: a lowercase t drawn as a DAG, its tail ends on a target.
mark("t-graph", "the t of targets, as a graph",
     path("M26 6v34c0 9 5 14 13 14", 5) + path("M12 20h28", 5)
     + dot(12, 20, 5) + dot(40, 20, 5) + dot(26, 6, 5) + bullseye(47, 52, 8, 3.5, 3))

# 5 tb: a ligature; the b's bowl is a bullseye.
mark("tb", "a monogram: the b's bowl is a target",
     path("M14 8v36c0 7 4 10 10 10", 5) + path("M6 20h16", 5)
     + path("M32 6v50", 5) + ring(44, 42, 12, 5) + dot(44, 42, 3.5))

# 6 Grid: the canvas's dots; an edge runs through them to a target.
grid = ''.join(dot(x, y, 2.6) for x in (12, 32, 52) for y in (12, 32, 52) if (x, y) != (52, 12))
mark("Grid", "the board's dotted canvas, one path to the goal",
     grid + path("M12 52C24 52 20 32 32 32S40 12 52 12", 3.5) + dot(12, 52, 5) + bullseye(52, 12, 7.5, 3.5, 3))

# 7 Hit: an edge lands in a bullseye.
mark("Hit", "an edge lands dead centre",
     ring(40, 30, 20, 4) + ring(40, 30, 10, 4)
     + path("M6 56C16 56 22 34 40 30", 3.5) + dot(6, 56, 5) + dot(40, 30, 4))

# 8 Gauge: an analog meter: 240 degrees, ticks, a needle.
ticks = ''.join(path("M{:.2f} {:.2f}L{:.2f} {:.2f}".format(*pol(32, 38, 19, a), *pol(32, 38, 13, a)), 3.5)
                for a in (210, 150, 90, 30, -30))
nx, ny = pol(32, 38, 20, 55)
mark("Gauge", "an instrument reading the pipeline",
     path("M{:.2f} {:.2f}A26 26 0 1 1 {:.2f} {:.2f}".format(*pol(32, 38, 26, 210), *pol(32, 38, 26, -30)))
     + ticks + path(f"M32 38L{nx:.2f} {ny:.2f}", 4.5) + dot(32, 38, 6))

# 9 Board: concentric rounded squares: a target, as a board.
mark("Board", "a target drawn as a board",
     '<rect x="6" y="6" width="52" height="52" rx="13" stroke-width="4"/>'
     + '<rect x="19" y="19" width="26" height="26" rx="6" stroke-width="4"/>' + dot(32, 32, 4.5))

# 10 Cascade: two sources, a step, the goal.
mark("Cascade", "a small DAG, left to right",
     path("M9 11c10 0 8 11 17 11M9 33c10 0 8-11 17-11", 3.5) + path("M26 22c12 0 8 20 20 20", 3.5)
     + dot(9, 11, 5) + dot(9, 33, 5) + dot(26, 22, 5) + bullseye(46, 42, 12))

# 11 Hex: the R package hexagon around a bullseye.
hexp = "M" + "L".join("{:.2f} {:.2f}".format(*pol(32, 32, 28, 90 + 60 * k)) for k in range(6)) + "Z"
mark("Hex", "an R package's hexagon, a target inside",
     path(hexp, 4, 'stroke-linejoin="round"') + bullseye(32, 32, 11))

# 12 Funnel: two nodes above, merging down into the goal.
mark("Funnel", "a top-down DAG, merging",
     f'<rect x="4" y="5" width="20" height="10" rx="5" {F}/><rect x="40" y="5" width="20" height="10" rx="5" {F}/>'
     + path("M14 15c0 14 18 10 18 21M50 15c0 14-18 10-18 21", 3.5) + bullseye(32, 47, 12))

# 13 Compass: four targets, from every side, into one.
c = [pol(32, 32, 26, a) for a in (0, 90, 180, 270)]
mark("Compass", "every direction leads to the target",
     ''.join(path("M{:.2f} {:.2f}L{:.2f} {:.2f}".format(x, y, *pol(32, 32, 11, a)), 3.5)
             for (x, y), a in zip(c, (0, 90, 180, 270)))
     + ''.join(dot(x, y, 4.5) for x, y in c) + bullseye(32, 32, 9, 4, 3.5))

# 14 Watch: a loop around a dot: the board keeps watching.
sx, sy = pol(32, 32, 22, 120)
ex, ey = pol(32, 32, 22, 60)
mark("Watch", "a live loop around the pipeline",
     path(f"M{sx:.2f} {sy:.2f}A22 22 0 1 0 {ex:.2f} {ey:.2f}", 4.5)
     + f'<path d="M{ex + 7:.2f} {ey - 2:.2f}L{ex - 3:.2f} {ey - 6:.2f}L{ex + 1:.2f} {ey + 4:.2f}Z" {F}/>'
     + dot(32, 32, 7))

# 15 Pin: a map pin with a bullseye: where the pipeline stands.
mark("Pin", "where your pipeline stands",
     path("M32 60C22 46 10 38 10 25a22 22 0 0 1 44 0c0 13-12 21-22 35Z", 4, 'stroke-linejoin="round"')
     + bullseye(32, 25, 9, 4, 3.5))

# 16 Eye: tar_watch's successor: the iris is a target.
mark("Eye", "watching the pipeline; the iris is a target",
     path("M4 32C16 12 48 12 60 32C48 52 16 52 4 32Z", 4, 'stroke-linejoin="round"') + bullseye(32, 32, 10, 4, 4))

# 17 Split: half up to date (solid), half outdated (dashed).
mark("Split", "half up to date, half outdated",
     path("M32 6A26 26 0 0 1 32 58", 5) + path("M32 58A26 26 0 0 1 32 6", 5, 'stroke-dasharray="5 6.2" stroke-linecap="butt"')
     + dot(32, 32, 8))

# 18 Pulse: a heartbeat line into the target: a live board.
mark("Pulse", "a live signal reaching the target",
     path("M3 32h11l6-14 9 28 6-14h5", 4, 'stroke-linejoin="round"') + bullseye(50, 32, 10, 4, 3.5))

# 19 Triangle: the smallest DAG: two targets and their sink.
mark("Triangle", "the smallest DAG",
     path("M13 46L30 16M13 46H44M30 16L44 46", 3.5) + dot(13, 46, 6) + dot(30, 16, 6) + bullseye(48, 46, 9, 4, 3.5))

# 20 Clipboard: a board with a clip, a small DAG on it.
mark("Clipboard", "a board, a pipeline pinned on it",
     '<rect x="9" y="10" width="46" height="48" rx="7" stroke-width="4"/>'
     + f'<rect x="23" y="4" width="18" height="10" rx="3" {F}/>'
     + path("M20 27c8 0 8 8 14 8M20 44c8 0 8-9 14-9", 3.5) + dot(20, 27, 4.5) + dot(20, 44, 4.5)
     + bullseye(41, 35, 7, 3.5, 3))

# 21 Viewfinder: an analog camera's corners around the target.
mark("Viewfinder", "framing the target, as a camera",
     path("M6 20V12a6 6 0 0 1 6-6h8M44 6h8a6 6 0 0 1 6 6v8M58 44v8a6 6 0 0 1-6 6h-8M20 58h-8a6 6 0 0 1-6-6v-8", 4.5)
     + bullseye(32, 32, 12, 4.5, 5))

# 22 Dial: a clock face's ticks around a target.
dial = ''.join(path("M{:.2f} {:.2f}L{:.2f} {:.2f}".format(*pol(32, 32, 28, a), *pol(32, 32, 22 if a % 90 else 19, a)), 3.5)
               for a in range(0, 360, 30))
mark("Dial", "an instrument's dial, a target at its heart",
     dial + bullseye(32, 32, 11, 4, 4.5))

# 23 Square: the board's frame; two edges into its centre.
mark("Square", "the board, two edges into the goal",
     '<rect x="6" y="6" width="52" height="52" rx="15" stroke-width="4"/>'
     + path("M6 22c14 0 14 10 26 10M6 42c14 0 14-10 26-10", 3.5) + dot(32, 32, 7))

# 24 Lanes: three lanes run into the target.
mark("Lanes", "parallel lanes reaching the goal",
     path("M4 18h24M10 32h18M4 46h24", 4.5) + bullseye(44, 32, 15, 4.5, 5))

# 25 Lens: a magnifier over a target: inspect any of them.
mark("Lens", "inspect any target",
     ring(26, 26, 19, 4.5) + path("M40 40L57 57", 7) + bullseye(26, 26, 9, 3.5, 3.5))

assert len(marks) == 25

# One SVG each.
for i, (name, idea, body) in enumerate(marks, 1):
    svg = (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="256" height="256" '
           f'fill="none" stroke="currentColor" stroke-linecap="round" color="#212529">{body}</svg>\n')
    open(os.path.join(out, f"{i:02d}-{name.lower()}.svg"), "w", encoding="utf-8").write(svg)

# The sheet: each mark big, at 24 and 16 px, and in the top bar; light and dark.
symbols = ''.join(
    f'<symbol id="m{i}" viewBox="0 0 64 64" fill="none" stroke="currentColor" stroke-linecap="round">{body}</symbol>'
    for i, (_, _, body) in enumerate(marks, 1))
cards = []
for i, (name, idea, _) in enumerate(marks, 1):
    use = f'<use href="#m{i}"/>'
    cards.append(f'''<div class="card">
  <div class="light"><div class="num">{i}</div><div class="name">{name}</div>
    <svg class="big">{use}</svg>
    <div class="row"><svg class="s24">{use}</svg><svg class="s16">{use}</svg>
      <span class="bar"><svg class="s22">{use}</svg>Targets board</span></div></div>
  <div class="dark"><svg class="s24">{use}</svg><svg class="s16">{use}</svg>
    <span class="bar"><svg class="s22">{use}</svg>Targets board</span></div>
  <div class="idea">{idea}</div>
</div>''')
html = f'''<!DOCTYPE html><html><head><meta charset="utf-8"><style>
body {{ margin: 0; padding: 16px; background: #f1f3f5; font-family: system-ui, "Segoe UI", Roboto, sans-serif; }}
.grid {{ display: grid; grid-template-columns: repeat(5, 1fr); gap: 12px; }}
.card {{ background: #fff; border-radius: 10px; overflow: hidden; border: 1px solid #dee2e6; }}
.light {{ color: #212529; padding: 10px 12px 12px; position: relative; display: flex; flex-direction: column; align-items: center; gap: 10px; }}
.dark {{ background: #212529; color: #dee2e6; padding: 8px 12px; display: flex; align-items: center; gap: 10px; justify-content: center; }}
.num {{ position: absolute; left: 10px; top: 8px; font-size: 12px; font-weight: 700; opacity: .45; }}
.name {{ font-size: 13px; font-weight: 600; }}
.big {{ width: 96px; height: 96px; }}
.row {{ display: flex; align-items: center; gap: 10px; }}
.s24 {{ width: 24px; height: 24px; }} .s16 {{ width: 16px; height: 16px; }} .s22 {{ width: 22px; height: 22px; }}
.bar {{ display: inline-flex; align-items: center; gap: 6px; font-size: 15px; font-weight: 700; margin-left: 4px; }}
.idea {{ font-size: 11px; color: #6c757d; padding: 6px 10px 8px; text-align: center; }}
</style></head><body><svg width="0" height="0" style="position:absolute">{symbols}</svg>
<div class="grid">{''.join(cards)}</div></body></html>'''
open(os.path.join(out, "logo-sheet-25.html"), "w", encoding="utf-8").write(html)
print("ok")
