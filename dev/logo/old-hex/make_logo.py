# The hex sticker: a small DAG in the board's own colours (up to date,
# running with its spinner and dashed edges, errored, outdated; a warning
# mark) on the board's dotted canvas. Pointy-top hexagon, 518 x 600 px, the
# size of the stickers in github.com/rstudio/hex-stickers (1.73" x 2").
#
# From the package folder:
#   python dev/logo/make_logo.py
#   Rscript dev/logo/render_logo.R man/figures/logo.svg man/figures/logo.png 1
import math
W, H = 518, 600
cx, cy, R = W / 2, H / 2, 300
def hexagon(r):
    pts = [(cx + r * math.sin(math.radians(a)), cy - r * math.cos(math.radians(a))) for a in range(0, 360, 60)]
    return " ".join(f"{x:.1f},{y:.1f}" for x, y in pts)

ink = "#2f223d"
status = {  # fill, label colour: the board's node colours
    "uptodate": ("#354823", "#d3f0b7"),
    "outdated": ("#78B7C5", "#10363f"),
    "running": ("#d49536", "#3f2f11"),
    "errored": ("#C93312", "#ffb6a6"),
}
nw, nh = 98, 36
nodes = {  # id: (x, y of the top-left corner, status, label bar length)
    "a": (40, 160, "uptodate", 40), "b": (40, 254, "uptodate", 56),
    "c": (210, 128, "uptodate", 48), "d": (210, 212, "running", 34), "e": (210, 296, "errored", 52),
    "f": (380, 174, "outdated", 44), "g": (380, 268, "outdated", 58),
}
edges = [("a", "c"), ("a", "d"), ("b", "d"), ("b", "e"), ("c", "f"), ("d", "f"), ("d", "g"), ("e", "g")]

out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">',
       "<title>targetsboard hex sticker</title>",
       "<defs>",
       f'<clipPath id="hex"><polygon points="{hexagon(R)}"/></clipPath>',
       f'<pattern id="dots" width="18" height="18" patternUnits="userSpaceOnUse"><circle cx="9" cy="9" r="1.7" fill="{ink}" fill-opacity="0.32"/></pattern>',
       "</defs>",
       '<g clip-path="url(#hex)">',
       f'<rect width="{W}" height="{H}" fill="#f0e5d7"/>',
       f'<rect width="{W}" height="{H}" fill="url(#dots)"/>']
# Edges: from the right of one node to the left of the next, as React Flow draws them.
for s, t in edges:
    sx, sy = nodes[s][0] + nw, nodes[s][1] + nh / 2
    tx, ty = nodes[t][0], nodes[t][1] + nh / 2
    feeding_running = nodes[t][2] == "running"
    style = f'stroke="{ink}" stroke-width="3.2" stroke-opacity="0.75"' + (' stroke-dasharray="8 6"' if feeding_running else "")
    out.append(f'<path d="M{sx},{sy} C{sx + 45},{sy} {tx - 45},{ty} {tx},{ty}" fill="none" {style}/>')
for nid, (x, y, st, bar) in nodes.items():
    fill, label = status[st]
    out.append(f'<rect x="{x}" y="{y}" width="{nw}" height="{nh}" rx="9" fill="{fill}" stroke="{ink}" stroke-width="2.5"/>')
    out.append(f'<line x1="{x + 16}" y1="{y + nh / 2}" x2="{x + 16 + bar}" y2="{y + nh / 2}" stroke="{label}" stroke-width="6" stroke-linecap="round"/>')
    for hx in (x, x + nw):  # handles
        out.append(f'<circle cx="{hx}" cy="{y + nh / 2}" r="4" fill="#f0e5d7" stroke="{ink}" stroke-width="2"/>')
    if st == "running":  # the spinner
        sx, sy, r = x + nw - 22, y + nh / 2, 8
        out.append(f'<circle cx="{sx}" cy="{sy}" r="{r}" fill="none" stroke="{label}" stroke-opacity="0.3" stroke-width="3.5"/>')
        out.append(f'<path d="M{sx},{sy - r} A{r},{r} 0 0 1 {sx + r},{sy}" fill="none" stroke="{label}" stroke-width="3.5" stroke-linecap="round"/>')
# The warning mark on c.
wx, wy = nodes["c"][0] + 4, nodes["c"][1] - 2
out.append(f'<circle cx="{wx}" cy="{wy}" r="11" fill="#f2c94c" stroke="{ink}" stroke-width="2"/>')
out.append(f'<rect x="{wx - 1.8}" y="{wy - 6.5}" width="3.6" height="8" rx="1.5" fill="{ink}"/>')
out.append(f'<circle cx="{wx}" cy="{wy + 5}" r="2" fill="{ink}"/>')
# The name.
out.append(f'<text x="{cx}" y="452" text-anchor="middle" font-family="Segoe UI, Helvetica Neue, Arial, sans-serif" '
           f'font-weight="700" font-size="66" letter-spacing="-1" textLength="392" lengthAdjust="spacingAndGlyphs" fill="{ink}">targetsboard</text>')
out.append("</g>")
# The border, drawn inside the edge so the die cut keeps it whole.
out.append(f'<polygon points="{hexagon(R - 9)}" fill="none" stroke="{ink}" stroke-width="18" stroke-linejoin="round"/>')
out.append("</svg>")
open("man/figures/logo.svg", "w", encoding="utf-8", newline="\n").write("\n".join(out) + "\n")
