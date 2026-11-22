"""Contact sheet of a recording, to review it without watching it: a frame
every <step> seconds from <start> to <end>.

    python demo/sheet.py <start> <end> <step> <sheet.png> [<out>]
"""
import csv, os, sys
from PIL import Image, ImageDraw
SHEET = os.path.abspath(sys.argv[4])
if len(sys.argv) > 5: os.chdir(sys.argv[5])
rows = [(int(r['n']), float(r['t'])) for r in csv.DictReader(open('frames.csv'))]
t0 = rows[0][1]
start, end, step = float(sys.argv[1]), float(sys.argv[2]), float(sys.argv[3])
out = SHEET
picks = []
t = start
while t <= end:
    n = max((r for r in rows if r[1] - t0 <= t), key=lambda r: r[1])[0]
    picks.append((t, n)); t += step
W, H, cols = 480, 270, 4
sheet = Image.new('RGB', (W * cols, H * ((len(picks) + cols - 1) // cols)), 'white')
d = ImageDraw.Draw(sheet)
for i, (t, n) in enumerate(picks):
    im = Image.open(f'frames/{n:06d}.jpg').resize((W, H))
    x, y = (i % cols) * W, (i // cols) * H
    sheet.paste(im, (x, y)); d.rectangle([x, y, x + 60, y + 22], fill='black'); d.text((x + 5, y + 5), f'{t:.0f}s', fill='white')
sheet.save(out)
